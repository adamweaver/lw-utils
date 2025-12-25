(in-package :dict)

(defun copy-hash-table (hash)
  (let ((new (make-hash-table :test (hash-table-test hash) :size (hash-table-size hash))))
    (loop for v being the hash-values of hash using (hash-key k) do (setf (gethash k new) v))
    new))

(defun maphash* (type function hash-table)
  "CL:MAPHASH always returns NIL because ... errr... why? This one returns a TYPE of FUNCTION called on HASH-TABLE entries"
  (let ((values (loop for key being the hash-keys of hash-table using (hash-value value) collect (funcall function key value))))
    (coerce values type)))

(defun make-hash-table-from-alist (&rest alist)
  "Make a hash table from an alist"
  (let ((hash (make-hash-table :test #'equalp)))
    (loop for (key . value) in alist do (setf (gethash key hash) value))
    hash))

(defun make-hash-table-from-plist (&rest plist)
  "Make a hash table from a plist"
  (let ((hash (make-hash-table :test #'equalp)))
    (loop for (key value) on plist by #'cddr do (setf (gethash key hash) value))
    hash))

(defun make-hash-table-from-object (object &rest sans)
  "Make a hash table from the keys/values in OBJECT, excluding those in SANS"
  (loop with hash = (make-hash-table :test #'equalp)
        for slot in (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))
        unless (find slot sans :test #'eq)
          do (setf (gethash (string-downcase (symbol-name slot)) hash) (slot-value object slot))
        finally (return hash)))

(defun dictionary (needle dict &key default (key #'identity))
  "Find the value belonging to NEEDLE in DICT (which may be a plist, alist, or hash-table). Return DEFAULT if not found"
  (let ((test (cond ((stringp needle) #'string=) ((symbolp needle) #'eq) (t #'eql))))
    (cond ((null dict) default)
          ((and (consp dict) (consp (car dict)))
           (lw:if-let (kv (assoc needle dict :key key :test test))
                      (cdr kv)
                      default))
          ((consp dict)
           (loop for (item value) on dict by #'cddr when (funcall test needle (funcall key item))
                 do (return-from dictionary value))
           default)
          ((hash-table-p dict)
           (loop for item being the hash-keys of dict using (hash-value value) when (funcall test needle (funcall key item))
                 do (return-from dictionary value))
           default)
          (t
           (error "Dictionary must be ALIST, PLIST, or HASH-TABLE, not ~S" (type-of dict))))))

(defun dictionary* (needle dict &key (key #'identity))
  "Confirm a value belonging to NEEDLE in DICT exists (which may be a plist, alist, or hash-table)."
  (let ((test (cond ((stringp needle) #'string=) ((symbolp needle) #'eq) (t #'eql))))
    (cond ((null dict) nil)
          ((and (consp dict) (consp (car dict))) (assoc needle dict :key key :test test))
          ((consp dict) (loop for (item) on dict by #'cddr thereis (and (funcall test needle (funcall key item)) t)))
          ((hash-table-p dict) (nth-value 1 (gethash needle dict)))
          (t (error "Dictionary must be ALIST, PLIST, or HASH-TABLE, not ~S" (type-of dict))))))

(defun (setf dictionary) (value needle dict &key (key #'identity))
  "Set the value VALUE for slot NEEDLE in DICT"
  (let ((test (cond ((stringp needle) #'string=) ((symbolp needle) #'eq) (t #'eql))))
    (cond ((and (consp dict) (consp (car dict)))
           (lw:if-let (kv (assoc needle dict :key key :test test))
                      (rplacd kv value)
                      (setf dict (acons needle value dict))))
          ((consp dict)
           (list* needle value dict))
          ((hash-table-p dict)
           (setf (gethash needle dict) value))
          (t
           (error "DICTIONARY must be ALIST, PLIST or HASH-TABLE, not ~S" (type-of dict))))))

(defun dictionary-if (test dict &key (key #'identity) default)
  "Find the value where TEST returns non-NIL in DICT (which maybe a plist, alist, or hash-table). Return DEFAULT if not found"
  (cond ((null dict) default)
        ((and (consp dict) (consp (car dict)))
         (lw:if-let (kv (assoc-if test dict :key key))
                    (cdr kv)
                    default))
        ((consp dict)
         (loop for (item value) on dict by #'cddr when (funcall test (funcall key item))
               do (return-from dictionary-if value))
         default)
        ((hash-table-p dict)
         (loop for item being the hash-keys of dict using (hash-value value) when (funcall test (funcall key item))
               do (return-from dictionary-if value)))
        (t
         (error "Dictionary must be ALIST, PLIST, or HASH-TABLE, not ~S" (type-of dict)))))

(defun map-dictionary (result-type function dict)
  "Run FUNCTION over each KEY VALUE of DICT, returning a RESULT TYPE sequence"
  (let ((results (if (hash-table-p dict)
                     (loop for v being the hash-values of dict using (hash-key k) collect (funcall function k v))
                     (loop for (k . v) in dict collect (funcall function k v)))))
    (when result-type
      (coerce results result-type))))

(defun mapcan-dictionary (result-type function dict)
  "Run FUNCTION over each KEY VALUE of DICT, nconcing a RESULT-TYPE sequence"
  (let ((results (if (hash-table-p dict)
                     (loop for v being the hash-values of dict using (hash-key k) nconc (funcall function k v))
                     (loop for (k . v) in dict nconc (funcall function k v)))))
    (when result-type
      (coerce results result-type))))

(defun memoise-objects (list key)
  (let ((hash (make-hash-table :test #'equalp)))
    (loop for item in list do (setf (gethash (funcall key item) hash) item))
    hash))

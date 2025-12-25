(in-package :utils)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun strcat (&rest args)
    "Create a new string from ARGS"
    (apply #'concatenate 'string args))

  (defun strcat-adjacent (list)
    "Catenate adjacent strings in a list into a single string, leaving non-string elements alone"
    (cond
      ((and (stringp (car list)) (stringp (cadr list))) (strcat-adjacent (cons (strcat (car list) (cadr list)) (cddr list))))
      ((cadr list) (cons (car list) (strcat-adjacent (cdr list))))
      (t list))))

(define-compiler-macro strcat (&whole form &rest args)
  (let* ((reduction (strcat-adjacent args))
         (length (length reduction)))
    (cond
      ((= (length args) length) form)
      ((= length 1) reduction)
      (t `(strcat ,@reduction)))))

(defun join (join list)
  "Turn our string LIST into a single string interspersed with JOIN"
  (apply #'concatenate 'string (intersperse join list)))

(defun left-pad (string count character)
  "Pad STRING out to COUNT length, prepending CHARACTER as necessary"
  (let ((length (length string)))
    (if (< length count)
        (apply #'strcat (nconc (loop with str = (string character) repeat (- count length) collect str) (list string)))
        string)))

(defun urlify (str)
  "Lowercase STR and replace non alphanumeric characters with #\-, collapsing adjacent duplicates"
  (coerce (loop with p = #\-
                for c across str
                for char = (if (or (alphanumericp c) (char= c #\.)) (char-downcase c) #\-)
                when (or (char/= char #\-) (char/= char p))
                  collect char and do (setf p char))
          'string))

(defun random-alpha-ascii-string (length)
  "Create a random string of LENGTH of ascii characters [A-Za-z0-9]"
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
        (string (make-string length)))
    (loop for i below length do (setf (schar string i) (schar chars (random 62))))
    string))

(defun starts-with (string start &key (test #'char=))
  "Does STRING commence with START using TEST func"
  (let ((mismatch (mismatch string start :test test)))
    (or (null mismatch) (= mismatch (length start)))))

(defun ends-with (string end &key (test #'char=))
  "Does STRING finish with END using TEST func"
  (let ((mismatch (mismatch string end :test test :from-end t)))
    (or (null mismatch) (= mismatch (- (length string) (length end))))))

(defun & (&rest strings)
  (apply #'concatenate 'string (mapcar #'princ-to-string strings)))

(defun && (&rest strings)
  (apply #'concatenate 'string (intersperse " " (mapcar #'princ-to-string strings))))

(defun match (regex string)
  (cl-ppcre:scan regex string))

(defun compose (&rest functions)
  "Create a function pipeline: (compose #'identity #'list #'car) => (lambda (X)  (identity (list (car X))))"
  (destructuring-bind (fn1 . rest) (reverse functions)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v)) rest :initial-value (apply fn1 args)))))

(defun partial (function &rest args)
  "Turn FUNCTION into a partial application with calling argument to precede ARGS.
(partial #'eql 3) => (lambda (x) (eql x 3))"
  (lambda (&rest more)
    (apply function (append more args))))

(defun partial* (function &rest args)
  "Turn FUNCTION into a partial application with ARGS to precede the calling argument(s)
(partial* #'member :bob) => (lambda (x) (member :bob x))"
  (lambda (&rest more)
    (apply function (append args more))))

(defun reverse-c-byte-order (buffer length &optional (start 0))
  "Reverse byte order in a C buffer (i.e. convert LE to BE)"
  (loop for i below (/ length 2) do
    (rotatef (fli:dereference buffer :index (+ start i))
             (fli:dereference buffer :index (- (+ start length) 1 i)))))

(defun group/r (source acc n)
  (let ((rest (nthcdr n source)))
    (if (consp rest)
        (group/r rest (cons (subseq source 0 n) acc) n)
        (nreverse (cons source acc)))))

(defun group* (source n)
  "Create sublists from SOURCE of length N each"
  (if (zerop n) (error "zero length GROUP makes no sense"))
  (when source
    (group/r source nil n)))

(defun flatten (structure)
  "Remove all internal list structure. (1 (2 3 (4 5) 6 (7))) => (1 2 3 4 5 6 7)"
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun intersperse (value list)
  "Create a new list from LIST with VALUE between each element"
  (loop for l on list collect (car l)
        when (cdr l) collect value))

(defun mklist (atom-or-list)
  "Turn ATOM-OR-LIST into a list (if not already) containing ATOM-OR-LIST"
  (cond
    ((null atom-or-list) nil)
    ((atom atom-or-list) (list atom-or-list))
    (t atom-or-list)))

(defun uncons (list)
  "Turn a list of conses into a list of lists; e.g. ((a . b) (c . d)) => ((a b) (c d))"
  (mapcar (lambda (c) (list (car c) (cdr c))) list))

(defun nsubseq (sequence start &optional end)
  "Returns a subsequence by pointing to location in original sequence."
  (make-array (- (or end (length sequence)) start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defmacro case* (test arg &body forms)
  "Convenience macro to test ARG against each of FORMS using TEST, much like a CASE uses eql"
  (let ((once-only (gensym "CASE-")))
    `(let ((,once-only ,arg))
       (cond ,@(mapcar (lambda (form)
                         (if (or (eq (car form) t) (eq (car form) 'otherwise))
                             `(t
                               ,@(cdr form))
                             `((or ,@(mapcar (lambda (f) `(funcall ,test ,once-only ,f))
                                             (if (consp (car form)) (car form) (list (car form)))))
                               ,@(cdr form))))
                       forms)))))


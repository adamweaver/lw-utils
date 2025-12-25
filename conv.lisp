(in-package :conv)

(defun number-decoration-p (char)
  (or (char= char #\_) (char= char #\,) (char= char #\$)))

(defun string! (val)
  (or (string? val) ""))

(defun string? (val)
  (unless (null val)
    (cond ((stringp val) val)
          ((numberp val) (princ-to-string val))
          ((eq val t) "TRUE")
          ((date:datep val) (date:format-date val))
          ((symbolp val) (symbol-name val))
          ((hash-table-p val) (princ-to-string (hash-table-count val))))))

(defun integer! (val)
  (or (integer? val) 0))

(defun integer? (val)
  (unless (null val)
    (cond ((integerp val) val)
          ((numberp val) (truncate val))
          ((stringp val) (ignore-errors (parse-integer (remove-if #'number-decoration-p val) :junk-allowed t)))
          ((date:datep val) (date:seconds val))
          ((hash-table-p val) (hash-table-count val)))))

(defun money! (val)
  (truncate (* 100 (float! val))))

(defun money? (val)
  (lw:when-let (v (float? val))
    (truncate (* v 100))))

(defun boolean! (val)
  (unless (null val)
    (cond ((eq val t) t)
          ((stringp val) (or (string-equal val "TRUE") (string-equal val "on") (not (zerop (integer! val)))))
          ((numberp val) (not (zerop val)))
          ((hash-table-p val) (not (zerop (hash-table-count val)))))))

(defun float! (val)
  (or (ignore-errors (hcl:parse-float (remove-if #'number-decoration-p val))) 0.0))

(defun float? (val)
  (ignore-errors (hcl:parse-float (remove-if #'number-decoration-p val))))

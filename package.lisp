(defpackage :conv
  (:use :cl)
  (:export
   "BOOLEAN!"
   "FLOAT!"
   "FLOAT?"
   "INTEGER!"
   "INTEGER?"
   "MONEY!"
   "MONEY?"
   "STRING!"
   "STRING?"))

(defpackage :dict
  (:use :cl)
  (:export
   "COPY-HASH-TABLE"
   "DICTIONARY"
   "DICTIONARY*"
   "DICTIONARY-IF"
   "MAKE-HASH-TABLE-FROM-ALIST"
   "MAKE-HASH-TABLE-FROM-OBJECT"
   "MAKE-HASH-TABLE-FROM-PLIST"
   "MAP-DICTIONARY"
   "MAPCAN-DICTIONARY"
   "MAPHASH*"))

(defpackage :utils
  (:use :cl)
  (:export
   "&"
   "&&"
   "CASE*"
   "COMPOSE"
   "ENDS-WITH"
   "FLATTEN"
   "GROUP*"
   "INTERSPERSE"
   "JOIN"
   "LEFT-PAD"
   "MATCH"
   "MKLIST"
   "NSUBSEQ"
   "PARTIAL"
   "PARTIAL*"
   "RANDOM-ALPHA-ASCII-STRING"
   "REVERSE-C-BYTE-ORDER"
   "STARTS-WITH"
   "STRCAT"
   "STRCAT-ADJACENT"
   "UNCONS"
   "URLIFY"))

(defpackage :english 
  (:use :cl :utils)
  (:export
   "FORMAT-ABN"
   "FORMAT-PHONE-NUMBER"
   "MAKE-ABN" 
   "MAKE-EMAIL-ADDRESS"
   "MAKE-PHONE-NUMBER"
   "NORMALISE-PHONE-NUMBER"
   "PLURALISE-WORD"
   "VOWELP"))


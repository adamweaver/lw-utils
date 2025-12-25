(defpackage :conv
  (:use :cl)
  (:export "STRING!" "STRING?" "INTEGER!" "INTEGER?" "MONEY!" "MONEY?" "BOOLEAN!" "FLOAT!" "FLOAT?"))

(defpackage :english 
  (:use :cl)
  (:export "VOWELP" "PLURALISE-WORD" "FORMAT-PHONE-NUMBER" "NORMALISE-PHONE-NUMBER" "FORMAT-ABN" "MAKE-ABN" 
	   "MAKE-PHONE-NUMBER" "MAKE-EMAIL-ADDRESS"))

(defpackage :string
  (:use :cl)
  (:export 
    case* strcat strcat-adjacent join left-pad urlify random-alpha-ascii-string starts-with ends-with & && match
    compose partial partial* reverse-c-byte-order group* flatten intersperse mklist uncons nsubseq)




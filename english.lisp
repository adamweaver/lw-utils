(in-package :english)

(defconstant +irregular-words+
  (loop with hash = (make-hash-table :test #'equalp)
        for (singular plural) on
        '("ox" "oxen" 
	  "are" "is" 
	  "cod" "cod" 
	  "elk" "elk" 
	  "pro" "pros" 
	  "info" "info"
          "beef" "beeves" 
	  "carp" "carp" 
	  "data" "data" 
	  "have" "has" 
	  "mews" "mews" 
	  "news" "news"
          "todo" "todos" 
	  "trans" "trans"
	  "tuna" "tuna" 
	  "afrit" "afriti" 
	  "bison" "bison"
          "bream" "bream" 
	  "cactus" "cacti" 
	  "child" "children" 
	  "cherub" "cherubim" 
	  "corps" "corps"
          "djinn" "djinn"
	  "focus" "foci" 
	  "goose" "geese" 
	  "guano" "guanos"
          "index" "indices" 
	  "money" "monies" 
	  "mouse" "mice" 
	  "mumak" "mumakil"
          "rhino" "rhinos" 
	  "sinus" "sinus" 
	  "swine" "swine" 
	  "trout" "trout"
          "afreet" "afreeti" 
	  "armour" "armour" 
	  "bonobo" "bonobos" 
	  "debris" "debris"
          "efreet" "efreeti" 
	  "charges" "charges"
	  "eland" "eland" 
	  "fungus" "fungi" 
	  "radius" "radiii"
          "salmon" "salmon" 
	  "seraph" "seraphim" 
	  "series" "series"
          "shears" "shears" 
	  "sphynx" "sphynxes" 
	  "status" "status"
          "vortex" "vortices"
	  "albino" "albinos" 
	  "gallows" "gallows"
          "incubus" "incubi" 
	  "inferno" "infernos" 
	  "mongoose" "mongooses"
          "pincers" "pincers" 
	  "species" "species" 
	  "whiting" "whiting"
          "breeches" "breeches" 
	  "britches" "britches" 
	  "commando" "commandos"
          "graffiti" "graffiti" 
	  "mackerel" "mackerel" 
	  "succubus" "succubi"
          "apparatus" "apparatus" 
	  "armadillo" "armadillos" 
	  "vertebra" "vertebrae"
          "wildebeest" "wildebeest" 
	  "candelabrum" "candelabra"
          "headquarters" "headquarters") by #'cddr
        do (setf (gethash singular hash) plural)
        finally (return hash)))

(defconstant +irregular-suffixes+
  '(("ch" . "ches") ("sh" . "shes") ("ss" . "sses") ("alf" .  "alves")
    ("elf" . "elves") ("olf" . "olves") ("eaf" . "eaves") ("arf" . "arves")
    ("ois" . "ois") ("eau" . "eaux") ("ieu" . "ieux") ("inx" . "inges")
    ("anx" . "anges") ("ynx" . "ynges") ("oof" . "ooves") ("nife" . "nives")
    ("data" . "data") ("datum" . "data") ("wife" . "wives") ("fish" . "fish") ("deer" . "deer")
    ("pox" . "pos") ("itis" . "itis") ("trix" . "trices") ("sheep" . "sheep")))

(defun vowelp (char)
  (member char '(#\a #\e #\i #\o #\u) :test #'char-equal))

(defun pluralise-word (orig-word)
  "Turn WORD into its plural form."
  (let* ((word (string-downcase orig-word)) (word-length (length word)))
    (flet ((last-letter (num)
             (schar word (- word-length num)))

           (irregularp (suffix)
             (let ((sufflen (length suffix)))
               (and (>= word-length sufflen)
                    (string= suffix word :start2 (- word-length sufflen))))))

      (or (when (< word-length 2)  word)
          ;; 1. is the word irregular?
          (gethash word +irregular-words+)

          ;; 2. does it have an irregular suffix?
          (lw:when-let (found (assoc-if #'irregularp +irregular-suffixes+))
            (strcat (subseq word 0 (- word-length (length (car found)))) (cdr found)))

          ;; 3. *[aeiou]y => *[aeiou]ys, else *y => *ies
          (when (char= (last-letter 1) #\y)
            (if (vowelp (last-letter 2))
                (strcat word "s")
                (strcat (subseq word 0 (1- word-length)) "ies")))

          ;; 4. *[aeiou]o => *[aeiou]os, else *o => *oes
          (when (char= (last-letter 1) #\o)
            (strcat word (if (vowelp (last-letter 2)) "s" "es")))

          ;; 5. *[aeiou][xs] => *es
          (when (and (find (last-letter 1) "sx" :test #'char=) (vowelp (last-letter 2)))
            (strcat word "es"))

          ;; 6. * => *s"
          (strcat word "s")))))

(defun format-phone-number (number)
  (flet ((part (start end)
           (let ((len (length number)))
             (if (and (< start len) (if end (<= end len) t))
                 (subseq number start end)
                 "???"))))
    (cond ((string= number "") "-")
          ((starts-with number "+614") (format nil "0~A ~A ~A" (part 3 6) (part 6 9) (part 9 12)))
          ((starts-with number "+61") (format nil "0~A ~A ~A" (part 3 4) (part 4 8) (part 8 12)))
          ((starts-with number "+1") (format nil "0011 1 ~A" (part 2 nil)))
          (t (format nil "0011 ~A ~A" (part 1 3) (part 3 nil))))))

(defun normalise-phone-number (text)
  (lw:when-let (number (if (stringp text) (remove-if-not #'digit-char-p text)))
    (case (length number)
      (8 (concatenate 'string "+618" number))
      (10 (concatenate 'string "+61" (subseq number 1)))
      (11 (concatenate 'string "+" number)))))

(defun format-abn (number)
  (flet ((part (start end)
           (let ((len (length number)))
             (if (and (< start len) (if end (<= end len) t))
                 (subseq number start end)
                 "???"))))
    (unless (or (null number) (string= number "") (string= number "FALSE"))
      (format nil "~A ~A ~A ~A" (part 0 2) (part 2 5) (part 5 8) (part 8 11)))))

(defun make-abn (number)
  (let ((number (remove-if-not #'digit-char-p number)))
    (and (= (length number) 11) number)))

(defun make-phone-number (string)
  (when (and string (string/= string ""))
    (let* ((str (remove-if-not #'digit-char-p string)) (len (length str)))
      (cond ((and (starts-with str "61") (= len 11)) (concatenate 'string "+" str))
            ((and (starts-with str "04") (= len 10)) (concatenate 'string "+614" (subseq str 2)))
            ((= len 8) (concatenate 'string "+618" str))
            ((starts-with str "0") (concatenate 'string "+61" (subseq str 1)))
            (t nil)))))

(defun make-email-address (string)
  (flet ((allowed-p (c)
           (or (alphanumericp c) (find c ".-_+~!#$%^&*" :test #'char=)))
         (strip-mailto (c)
           (if (starts-with c "mailto:") (subseq c 7) c)))

    (let* ((amphora (position #\@ string :test #'char=))
           (local (when amphora (delete-if-not #'allowed-p (strip-mailto (subseq string 0 amphora)))))
           (domain (when amphora (delete-if-not #'allowed-p (subseq string (1+ amphora))))))
      (when (and local domain (string/= local "") (string/= domain ""))
        (concatenate 'string local "@" domain)))))

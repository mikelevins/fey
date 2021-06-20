(in-package :fi)

;;;
;;; Fálláláthá => fál lál láth -á
;;; Heiróván    => heir róv -án
;;; Ímbrelín    => ím brel -ín
;;; Láthéyá     => láth théy -á
;;; Limnics     => lim nics
;;; Baelveg     => bél veg
;;; Óndá        => ónd -á
;;; Urgónd      => úr gónd
;;; Dórgól      => dór gól
;;;
;;; (find-cvc-particles "gól")
;;; (find-vc-particles "úr")

(defparameter $particle-initial-consonants
  '("b" "bl" "br" "c" "cr" "d" "dr" "f" "g" "gl" "h" "hr" "l" "m" "n" "r" "t" "tr" "th" "v" "w"))

(defparameter $particle-final-consonants
  '("ch" "dh" "cs" "g" "gh" "l" "m" "n" "nd" "ng" "r" "s" "sh" "th" "v" "w" "wr" "y"))

(defparameter $particle-initial-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $particle-medial-vowels
  '("á" "ai" "au" "é" "e" "ei" "í" "ó" "ou" "ú" "ua" "ui"))

(defparameter $particle-final-vowels
  '("á" "é" "í" "ó" "ú"
    "au" "ei" "iu" "ou" "ua" "ui"))

(defparameter $suffix-initial-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $suffix-final-consonants
  '("ch" "dh" "g" "l" "n" "ng" "r" "s" "th" "w"))

(defun all-vc-particles ()
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for v in $particle-initial-vowels
               collect (loop for c in $particle-final-consonants
                          collect (concatenate 'string v c))))
    :test #'equalp)
   #'string<))

(defparameter $all-vc-particles
  (all-vc-particles))

(defun write-all-vc-particles (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for part in (all-vc-particles)
         do (format out "~A~%" part))))

(defun find-vc-particles (prefix)
  (remove-if-not (lambda (p)(prefix-match prefix p))
                 (all-vc-particles)))


(defun all-cv-particles ()
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for c in $particle-initial-consonants
               collect (loop for v in $particle-final-vowels
                          collect (concatenate 'string c v))))
    :test #'equalp)
   #'string<))

(defparameter $all-cv-particles
  (all-cv-particles))

(defun write-all-cv-particles (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for part in (all-cv-particles)
         do (format out "~A~%" part))))


(defun find-cv-particles (prefix)
  (remove-if-not (lambda (p)(prefix-match prefix p))
                 (all-cv-particles)))

(defun all-cvc-particles ()
  (sort
   (delete-duplicates
    (reduce #'append
            (reduce #'append
                    (loop for c1 in $particle-initial-consonants
                       collect (loop for v in $particle-medial-vowels
                                  collect (loop for c2 in $particle-final-consonants
                                             collect (concatenate 'string c1 v c2))))))
    :test #'equalp)
   #'string<))

(defparameter $all-cvc-particles
  (all-cvc-particles))

(defun write-all-cvc-particles (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for part in (all-cvc-particles)
         do (format out "~A~%" part))))


(defun find-cvc-particles (prefix)
  (remove-if-not (lambda (p)(prefix-match prefix p))
                 (all-cvc-particles)))


(defun all-suffixes ()
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for v in $suffix-initial-vowels
               collect (loop for c in $suffix-final-consonants
                          collect (concatenate 'string v c))))
    :test #'equalp)
   #'string<))

(defparameter $all-suffixes
  (all-suffixes))

(defun write-all-suffixes (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for part in (all-suffixes)
         do (format out "~A~%" part))))

(defun find-suffixes (suffix)
  (remove-if-not (lambda (p)(suffix-match suffix p))
                 (all-suffixes)))


;;; (length $all-vc-particles)
;;; (length $all-cv-particles)
;;; (length $all-cvc-particles)
;;; (length $all-suffixes)
;;; (write-all-vc-particles "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/all-vc-particles.txt")
;;; (write-all-cv-particles "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/all-cv-particles.txt")
;;; (write-all-cvc-particles "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/all-cvc-particles.txt")
;;; (write-all-suffixes "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/all-suffixes.txt")

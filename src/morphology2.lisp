(in-package :fi)

;;; ---------------------------------------------------------------------
;;; semantic particles
;;; ---------------------------------------------------------------------

(defun all-semantic-particles ()
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for left in $initial-consonants
               collect (loop for right in $vowels
                          collect (concatenate 'string left right))))
    :test #'equalp)
   #'string<))

(defparameter $all-semantic-particles
  (all-semantic-particles))

(defun write-semantic-particles-csv (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "particle, gloss, definition, argcount~%")
    (loop for part in $all-semantic-particles
       do (format out "~A,,,~%" part))))

;;; (write-semantic-particles-csv "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/semantic-particles.csv")

;;; ---------------------------------------------------------------------
;;; structure particles
;;; ---------------------------------------------------------------------

(defun all-vcv-particles ()
  (reduce #'append
          (reduce #'append
                  (loop for v1 in (append $long-vowels $nasal-vowels $rhotic-vowels $lateralized-vowels)
                     collect (loop for c in $initial-consonants
                                collect (loop for v2 in (append $vowels)
                                           collect (concatenate 'string v1 c v2)))))))

(defparameter $all-vcv-particles
  (all-vcv-particles))

(defun write-vcv-particles-csv (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "particle~%")
    (loop for part in $all-vcv-particles
       do (format out "~A~%" part))))

;;; (write-vcv-particles-csv "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/vcv-particles.csv")


(defun all-cvc-particles ()
  (reduce #'append
          (reduce #'append
                  (loop for c1 in $initial-consonants
                     collect (loop for v in $vowels
                                collect (loop for c2 in $final-consonants
                                           collect (concatenate 'string c1 v c2)))))))

(defparameter $all-cvc-particles
  (all-cvc-particles))

(defun write-cvc-particles-csv (path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "particle~%")
    (loop for part in $all-cvc-particles
       do (format out "~A~%" part))))

;;; (write-cvc-particles-csv "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/cvc-particles.csv")



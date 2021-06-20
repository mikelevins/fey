(in-package :fi)

(defun all-cv-syllables ()
  (delete-duplicates
   (reduce #'append
           (loop for left in $onset-consonants
              collect (loop for right in $coda-vowels
                         collect (concatenate 'string left right))))
   :test #'equalp))

(defun all-vc-syllables ()
  (delete-duplicates
   (reduce #'append
           (loop for left in $onset-vowels
              collect (loop for right in $coda-consonants
                         collect (concatenate 'string left right))))
   :test #'equalp))

(defun all-cvc-syllables ()
  (delete-duplicates
   (reduce #'append
           (reduce #'append
                   (loop for left in $onset-consonants
                      collect (loop for mid in $medial-vowels
                                 collect (loop for right in $coda-consonants
                                            collect (concatenate 'string left mid right))))))
   :test #'equalp))

(defun all-syllables ()
  (sort
   (concatenate 'list
                (all-vc-syllables)
                (all-cv-syllables)
                (all-cvc-syllables))
   #'string<))

(defun onset-consonant (str)
  (let ((onsets (copy-tree $onset-consonants)))
    (setf onsets (sort onsets (lambda (x y)(> (length x)(length y)))))
    (find-if (lambda (on)(prefix-match on str))
             onsets)))

(defun coda-consonant (str)
  (let ((codas (copy-tree $coda-consonants)))
    (setf codas (sort codas (lambda (x y)(> (length x)(length y)))))
    (find-if (lambda (co)(suffix-match co str))
             codas)))

(defun coda-vowel (str)
  (let ((codas (copy-tree $coda-vowels)))
    (setf codas (sort codas (lambda (x y)(> (length x)(length y)))))
    (find-if (lambda (co)(suffix-match co str))
             codas)))

;;; (two-syllable-words left right)
;;; left is a syllable coda
;;; right is a syllable onset
;;; result is a list of two-syllable words in which the first
;;; syllable's coda is left and the right syllable's onset is right
(defun two-syllable-words (left-coda right-onset)
  (flet ((joinable? (l r)
           (and (suffix-match left-coda l)
                (prefix-match right-onset r))))
    (let* ((syllables (all-syllables))
           (words nil))
      (loop for left in syllables
         do (loop for right in syllables
               do (when (joinable? left right)
                    (push (concatenate 'string left right)
                          words))))
      words)))

;;; (defparameter $words (time (two-syllable-words "é" "br")))
;;; (defparameter $words (time (two-syllable-words "e" "v")))

(defun random-two-syllable-word ()
  (any (two-syllable-words (any $codas)
                           (any $onsets))))

(with-open-file (out "/Users/mikel/Desktop/syllables.txt" :direction :output :if-exists :supersede)
  (loop for s in (all-syllables) do (format out "~A,~%" s)))

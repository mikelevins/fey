
(in-package :morpho)

;;; ---------------------------------------------------------------------
;;; utility functions
;;; ---------------------------------------------------------------------

(defmethod any ((s sequence))
  (elt s (random (length s))))

(defun vowel? (x)
  (member (string x)
          $vowels
          :test #'equalp))

(defun separate-clusters (str)
  (let ((vowel? (vowel? (elt str 0)))
        (change-indexes (list 0)))
    (loop for i from 0 below (length str)
       do (progn
            (if vowel?
                (unless (vowel? (elt str i))
                  (push i change-indexes)
                  (setf vowel? nil))
                (when (vowel? (elt str i))
                  (push i change-indexes)
                  (setf vowel? t)))))
    (loop for changes on (reverse change-indexes)
       collect (subseq str (first changes)(second changes)))))


;;; ---------------------------------------------------------------------
;;; generating and claiming
;;; ---------------------------------------------------------------------

(defun generate-vc ()
  (concatenate 'string
               (any (append $vowels $diphtongs))
               (any $final-consonants)))

(defun generate-cv ()
  (concatenate 'string
               (any $initial-consonants)
               (any (append $vowels $diphtongs))))

(defun generate-cvc ()
  (concatenate 'string
               (any $initial-consonants)
               (any (append $vowels $diphtongs))
               (any $final-consonants)))

(defun generate-vcv ()
  (concatenate 'string
               (any (append $vowels $diphtongs))
               (any $medial-consonants)
               (any (append $vowels $diphtongs))))

(defun generate-cvcv ()
  (concatenate 'string
               (any $initial-consonants)
               (any (append $vowels $diphtongs))
               (any $medial-consonants)
               (any (append $vowels $diphtongs))))

(defun generate-cvcvc ()
  (concatenate 'string
               (any $initial-consonants)
               (any (append $vowels $diphtongs))
               (any $medial-consonants)
               (any (append $vowels $diphtongs))
               (any $final-consonants)))

(defparameter *claimed-strings* (make-hash-table :test #'equal))

(defun claimed-string? (s)
  (gethash s *claimed-strings* nil))

(defun claim-string (s)
  (assert (not (claimed-string? s))() "Already claimed: ~S" s)
  (setf (gethash s *claimed-strings*) t)
  s)

;;; claimed words
;;; ---------------------------------------------------------------------

(defparameter $claimed-words
  '(
    chaen
    chlaidh
    chlár
    chruich
    dhauth
    dhór
    dhrom
    dhruich
    dhúh
    faol
    faum
    fawx
    féh
    fel
    fém
    flos
    fweath
    lán
    láth
    léth
    lhawch
    máth
    mauth
    mel
    mén
    mérh
    mwael
    ném
    nórh
    nwerh
    rax
    rén
    réth
    rhaul
    rioth
    ruin
    sél
    swain
    theox
    thán
    thír
    thlaw
    thoath
    thwál
    wón
    xleidh
    ))

(loop for word in $claimed-words
   do (claim-string (string-downcase (symbol-name word))))

(defun claimed-words ()
  (sort (loop for word being the hash-keys in *claimed-strings* collect word)
        #'string<))

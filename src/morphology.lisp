(in-package :fi)


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
;;; base predicates
;;; ---------------------------------------------------------------------

(defparameter $core-initials
  (sort
   (delete-duplicates
    (mapcar (compose #'car #'separate-clusters)
            $core-syllables)
    :test #'equalp)
   #'string<))

(defparameter $core-finals
  (sort
   (delete-duplicates
    (mapcar (compose (compose #'first #'last) #'separate-clusters)
            $core-syllables)
    :test #'equalp)
   #'string<))

(defun core-consonant-patterns ()
  (sort
   (mapcar (lambda (ls)
             (remove-if (lambda (s)(member s $vowels :test #'equalp))
                        ls))
           (mapcar #'separate-clusters $core-syllables))
   (lambda (x y)(equalp (first x)(first y)))))

(defun possible-consonant-patterns ()
  (let ((initial-consonants (remove-if (lambda (s)(member s $vowels :test #'equalp))
                                       $core-initials))
        (final-consonants (remove-if (lambda (s)(member s $vowels :test #'equalp))
                                     $core-finals)))
    (reduce #'append
            (loop for left in initial-consonants
               collect (loop for right in final-consonants
                          collect (list left right))))))

(defun available-consonant-patterns ()
  (subtract-lists (possible-consonant-patterns)
                  (core-consonant-patterns)))

(defparameter $forbidden-initial-consonants
  '("gh" "ng" "z" "zh"))

(defparameter $forbidden-final-consonants
  '("b" "p" "z"))

(defparameter $forbidden-final-vowels
  '("i"))

(defparameter $allowed-medial-consonants
  '())

(defun gen-cv (&optional (count 1))
  (flet ((gen ()
           (let ((initials (subtract-lists (consonants)
                                           $forbidden-initial-consonants))
                 (finals (subtract-lists $vowels
                                         $forbidden-final-vowels)))
             (concatenate 'string
                          (any initials)
                          (any finals)))))
    (loop for i from 0 below count collect (gen))))

(defun gen-cvc (&optional (count 1))
  (flet ((gen ()
           (let ((initials (subtract-lists (consonants)
                                           $forbidden-initial-consonants))
                 (finals (subtract-lists (consonants)
                                         $forbidden-final-consonants)))
             (concatenate 'string
                          (any initials)
                          (any $vowels)
                          (any finals)))))
    (loop for i from 0 below count collect (gen))))



(defun genword (&optional start)
  (let ((pat (if start
                 (let ((pats (remove-if-not (lambda (p)(equalp start (first p)))
                                            (available-consonant-patterns))))
                   (any pats))
                 (any (available-consonant-patterns))))
        (v (any $vowels)))
    (concatenate 'string
                 (first pat)
                 v
                 (second pat))))


(defun genwords (n &optional start)
  (loop for i from 0 below n collect (genword start)))

;;; ---------------------------------------------------------------------
;;; particles
;;; ---------------------------------------------------------------------

(defparameter $cv-particle-initials
  '("p" "b" "m" "f" "v" "th" "dh" "t" "d" "n" "r" "l" "s" "sh" "y" "c" "g" "w" "h"
                                             "br" "bl"                     "bw"
                                             "fr" "fl"                     "fw"
                                             "vr" "vl"                     "vw"
                                            "thr" "thl"                    "thw"
                                            "dhr" "dhl"                    "dhw"
                                            "tr"                           "tw"
                                            "dr"                           "dw"
                                            "sr"   "sl"                    "sw"
                                           "shr"   "shl"                   "shw"
                                            "cr"   "cl"                    "cw"
                                            "gr"   "gl"                    "gw"
                                            "hr"   "hl"                    "hw"))

(defparameter $cv-particle-finals
  '("m" "v" "th" "dh" "t" "d" "n" "r" "l" "s" "z" "sh" "zh" "c" "g" "w"
    "rm" "lm"
    "rv" "lv"
    "nth" "rth" "lth"
    "ndh" "rdh" "ldh"
    "nt" "rt" "st" "lt"
    "nd" "rd" "ld"
    "rn"
    "rs" "ts" "cs"
    "rz" "dz" "gz"
    "rsh" "tsh" "csh"
    "rsh" "dzh" "gzh"
    "nc" "rc" "sc"
    "ng" "rg" "zg"))


(defun gen-cv-particle (&optional start)
  (let ((parts $all-cv-particles))
    (if start
        (any (remove-if-not (lambda (p)(prefix-match start p))
                            parts))
        (any parts))))

(defun gen-cv-particles (n &optional start)
  (loop for i from 0 below n collect (gen-cv-particle start)))


(defun gen-vc-particle (&optional start)
  (let ((parts $all-vc-particles))
    (if start
        (any (remove-if-not (lambda (p)(prefix-match start p))
                            parts))
        (any parts))))



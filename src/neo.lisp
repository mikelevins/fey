;;; an experiment in building Fe vocabulary from the ground up
;;;notes:
;;; switch to CV as the base semantic unit for constructing words
;;; CVC for particles and suffixes
;;; then we get:
;;;
;;; Heirovan == hei ro -van
;;; Imbrelin == im bre -lin
;;; Baelveg == ba el -veg
;;; Onda == on -da
;;; Latheya == la the -ya
;;; Fallalhatha == fa lla lha -tha

(in-package :fi)

;;; ---------------------------------------------------------------------
;;; vowels
;;; ---------------------------------------------------------------------

(defparameter $short-vowels
  '("a" "e" "i" "i" "i"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $diphthongs
  '("a" "e" "i" "o" "u"
    "ae" "ai" "ao" "au"
    "ea" "ei" "eo" "eu"
    "ia" "ie" "io" "iu"
    "oa" "oe" "oi" "ou"
    "ua" "ue" "ui" "uo"))

(defparameter $vowels (append $short-vowels $long-vowels $diphthongs))

;;; ---------------------------------------------------------------------
;;; consonants
;;; ---------------------------------------------------------------------

;;; voicings
(defparameter $voicings '(:voiced :voiceless))

;;; places of articulation:
(defparameter $places
  '(:bilabial :labiodental :dental :alveolar  :alveolar-palatal :palatal :velar :labiovelar :glottal))

;;; manners of articulation:
(defparameter $manners
  '(:plosive :nasal :fricative :sibilant :trill :approximant))

;;; the consonants used in fe
(defparameter $consonants-with-classes
  '(("l" :alveolar :approximant :voiced)
    ("n" :alveolar :nasal :voiced)
    ("d" :alveolar :plosive :voiced)
    ("t" :alveolar :plosive :voiceless)
    ("z" :alveolar :sibilant :voiced)
    ("s" :alveolar :sibilant :voiceless)
    ("r" :alveolar :trill :voiced)
    ("zh" :alveolar-palatal :fricative :voiced)
    ("sh" :alveolar-palatal :fricative :voiceless)
    ("m" :bilabial :nasal :voiced)
    ("b" :bilabial :plosive :voiced)
    ("p" :bilabial :plosive :voiceless)
    ("dh" :dental :fricative :voiced)
    ("th" :dental :fricative :voiceless)
    ("h" :glottal :fricative :voiceless)
    ("v" :labiodental :fricative :voiced)
    ("f" :labiodental :fricative :voiceless)
    ("w" :labiovelar :approximant :voiced)
    ("y" :palatal :approximant :voiced)
    ("gh" :velar :fricative :voiced)
    ("ch" :velar :fricative :voiceless)
    ("ng" :velar :nasal :voiced)
    ("g" :velar :plosive :voiced)
    ("c" :velar :plosive :voiceless)))


(defun consonants (&rest kinds)
  (let ((found-kinds (if kinds
                         (remove-if-not (lambda (kind)
                                          (every (lambda (ck)(member ck kind))
                                                 kinds))
                                        $consonants-with-classes)
                         $consonants-with-classes)))
    (mapcar #'first found-kinds)))

(defun consonant-classes (consonant)
  (let ((found (assoc consonant $consonants-with-classes :test #'equalp)))
    (if found
        (cdr found)
        nil)))

;;; ---------------------------------------------------------------------
;;; particles
;;; ---------------------------------------------------------------------

(defparameter $initial-consonant-clusters
  '("br" "dr" "fw" "gl" "hr" "lh" "ll" "sw" "thw"))

(defparameter $final-consonant-clusters
  '())

(defparameter $forbidden-initial-consonants
  '("gh" "ng" "z" "zh"))

(defparameter $forbidden-final-consonants
  '("b" "p" "z"))

(defparameter $all-cv-particles
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for c in (subtract-lists (append $initial-consonant-clusters (consonants))
                                           $forbidden-initial-consonants)
               collect (loop for v in $vowels
                          collect (concatenate 'string c v))))
    :test #'equalp)
   #'string<))

(defparameter $all-vc-particles
  (sort
   (delete-duplicates
    (reduce #'append
            (loop for v in $vowels
               collect (loop for c in (consonants)
                          collect (concatenate 'string v c))))
    :test #'equalp)
   #'string<))

(defparameter $all-cvc-particles
  (sort
   (delete-duplicates
    (reduce #'append
            (reduce #'append
                    (loop for c1 in (subtract-lists (append $initial-consonant-clusters (consonants))
                                                    $forbidden-initial-consonants)
                       collect (loop for v in $vowels
                                  collect (loop for c2 in (subtract-lists (append $final-consonant-clusters (consonants))
                                                                          $forbidden-final-consonants)
                                             collect (concatenate 'string c1 v c2))))))
    :test #'equalp)
   #'string<))

;;; ---------------------------------------------------------------------
;;; word gen
;;; ---------------------------------------------------------------------
;;; assemble random particles into words
;;; only the first syllable can have an initial vowel

(defun genword (&optional (syllable-count 1))
  (let ((syl1 (any (append $vowels $all-cv-particles)))
        (rest-syls (if (> syllable-count 1)
                       (loop for i from 0 below (1- syllable-count)
                          collect (any $all-cv-particles))
                       nil)))
    (reduce (lambda (x y)(concatenate 'string x y))
            (cons syl1 rest-syls))))

(defun genwords (n &optional (syllable-count 1))
  (loop for i from 0 below n collect (genword syllable-count)))

(defun genparticle ()
  (any $all-cvc-particles))

(defun genparticles (n)
  (loop for i from 0 below n collect (genparticle)))

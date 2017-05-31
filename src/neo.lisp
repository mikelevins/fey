;;; an experiment in building Fe vocabulary from the ground up

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




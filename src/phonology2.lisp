(in-package :fi)

;;; ---------------------------------------------------------------------
;;; consonants
;;; ---------------------------------------------------------------------

;;; voicing
(defparameter $voicings '(:voiced :voiceless))

;;; places:
(defparameter $places
  '(:labial :bilabial :labiodental :dental :alveolar :palatoalveolar
    :retroflex :palatal :velar :labiovelar :uvular :pharyngeal
    :glottal))

;;; manners:
(defparameter $manners
  '(:nasal :plosive :affricate :fricative :sibilant :approximant :trill :flap
    :lateral-fricative :lateral-approximant :lateral-flap))

(defparameter $consonant-kinds
  '(("p" :voiceless :bilabial :plosive)
    ("b" :voiced :bilabial :plosive)
    ("mh" :voiceless :bilabial :nasal)
    ("m" :voiced :bilabial :nasal)
    ("f" :voiceless :labiodental :fricative)
    ("v" :voiced :labiodental :fricative)
    ("th" :voiceless :dental :fricative)
    ("dh" :voiced :dental :fricative)
    ("t" :voiceless :alveolar :plosive)
    ("d" :voiced :alveolar :plosive)
    ("nh" :voiceless :alveolar :nasal)
    ("n" :voiced :alveolar :nasal)
    ("rh" :voiceless :alveolar :trill)
    ("r" :voiced :alveolar :trill)
    ("lh" :voiceless :alveolar  :lateral-approximant)
    ("l" :voiced :alveolar  :lateral-approximant)
    ("s" :voiceless :alveolar :sibilant)
    ("z" :voiced :alveolar :sibilant)
    ("sh" :voiceless :alveolar :palatal :fricative)
    ("zh" :voiced :alveolar :palatal :fricative)
    ("yh" :voiceless :palatal :approximant)
    ("y" :voiced :palatal :approximant)
    ("ngh" :voiceless :velar :nasal)
    ("ng" :voiced :velar :nasal)
    ("c" :voiceless :velar :plosive)
    ("g" :voiced :velar :plosive)
    ("ch" :voiceless :velar :fricative)
    ("gh" :voiced :velar :fricative)
    ("wh" :voiceless :labiovelar :approximant)
    ("w" :voiced :labiovelar :approximant)
    ("h" :voiceless :glottal :fricative)))

(defparameter $forbidden-initial-consonants
  '((:velar :nasal)
    (:voiced :alveolar :palatal :fricative)
    (:voiced :velar :fricative)))

(defparameter $forbidden-initial-sequences
  '((:plosive)(:plosive)))

(defparameter $forbidden-final-consonants
  '((:voiceless :bilabial :plosive)
    (:voiced :bilabial :plosive)
    (:voiced :alveolar :sibilant)))

(defun consonants (&rest kinds)
  (let ((found-kinds (if kinds
                         (remove-if-not (lambda (kind)
                                          (every (lambda (ck)(member ck kind))
                                                 kinds))
                                        $consonant-kinds)
                         $consonant-kinds)))
    (mapcar #'first found-kinds)))

(defun consonant-kinds (consonant)
  (let ((found (assoc consonant $consonant-kinds :test #'equalp)))
    (if found
        (cdr found)
        nil)))

(defun initial-consonants ()
  (let ((forbidden-lists (mapcar (lambda (seq)(apply 'consonants seq))
                                 $forbidden-initial-consonants)))
    (subtract-lists (consonants)
                    (reduce #'append forbidden-lists))))

(defun final-consonants ()
  (let ((forbidden-lists (mapcar (lambda (seq)(apply 'consonants seq))
                                 $forbidden-final-consonants)))
    (subtract-lists (consonants)
                    (reduce #'append forbidden-lists))))

(defun any-consonant ()
  (any (consonants)))

(defun any-initial-consonant ()
  (any (initial-consonants)))

;;; ---------------------------------------------------------------------
;;; vowels
;;; ---------------------------------------------------------------------

(defparameter $short-vowels
  '("a" "e" "i" "o" "u"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $diphthongs
  '("a" "e" "i" "o" "u"
    "ae" "ai" "ao" "au"
    "ea" "ei" "eo" "eu"
    "ia" "ie" "io" "iu"
    "oa" "oe" "oi" "ou"
    "ua" "ue" "ui" "uo"))

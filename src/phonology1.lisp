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
  '(:nasal :plosive :affricate :fricative :approximant :trill :flap
    :lateral-fricative :lateral-approximant :lateral-flap))

(defparameter $consonant-kinds
  '(("p" :voiceless :bilabial :plosive)
    ("b" :voiced :bilabial :plosive)
    ("m" :voiced :bilabial :nasal)
    ("f" :voiceless :labiodental :fricative)
    ("v" :voiced :labiodental :fricative)
    ("th" :voiceless :dental :fricative)
    ("dh" :voiced :dental :fricative)
    ("t" :voiceless :alveolar :plosive)
    ("d" :voiced :alveolar :plosive)
    ("n" :voiced :alveolar :nasal)
    ("r" :voiced :alveolar :trill)
    ("l" :voiced :alveolar :lateral-approximant)
    ("s" :voiceless :alveolar :fricative)
    ("z" :voiced :alveolar :fricative)
    ("sh" :voiceless :palatoalveolar :fricative)
    ("zh" :voiced  :palatoalveolar :fricative)
    ("y" :voiced :palatal :approximant)
    ("ng" :voiced :velar :nasal)
    ("c" :voiceless :velar :plosive)
    ("g" :voiced :velar :plosive)
    ("ch" :voiceless :velar :fricative)
    ("gh" :voiced :velar :fricative)
    ("w" :voiced :labiovelar :approximant)
    ("h" :voiceless :glottal :fricative)))

(defun consonants (&rest kinds)
  (let ((found-kinds (if kinds
                         (remove-if-not (lambda (kind)
                                          (every (lambda (ck)(member ck kind))
                                                 kinds))
                                        $consonant-kinds)
                         $consonant-kinds)))
    (mapcar #'first found-kinds)))

(defun consonant->kinds (consonant)
  (let ((found (assoc consonant $consonant-kinds :test #'equalp)))
    (if found
        (cdr found)
        nil)))


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

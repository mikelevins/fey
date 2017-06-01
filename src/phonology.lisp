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

;;; voicing
(defparameter $voicings '(:voiced :voiceless))

;;; places:
(defparameter $places
  '(:bilabial :labiodental :dental :alveolar :postalveolar
    :retroflex :palatal :velar :labiovelar :uvular :pharyngeal :glottal))

;;; manners:
(defparameter $manners
  '(:plosive :nasal :trill :flap :affricate :fricative :lateral-fricative :approximant
    :lateral-approximant))

(defun all-consonant-classes ()
  (reduce #'append
          (reduce #'append
                  (loop for manner in $manners
                     collect (loop for place in $places
                                collect (loop for voicing in $voicings
                                           collect (list voicing place manner)))))))

(defparameter $all-consonant-classes (all-consonant-classes))

(defparameter $all-consonants
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
    ("sh" :voiceless :postalveolar :fricative)
    ("zh" :voiced  :postalveolar :fricative)
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
                                        $all-consonants)
                         $all-consonants)))
    (mapcar #'first found-kinds)))

(defun consonant->kinds (consonant)
  (let ((found (assoc consonant $all-consonants :test #'equalp)))
    (if found
        (cdr found)
        nil)))


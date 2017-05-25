(in-package :fi)

(defparameter $consonant-phonoclasses
  '(("p" :voiceless :labial :stop)
    ("b" :voiced :labial :stop)
    ("mh" :voiceless :labial :nasal)
    ("m" :voiced :labial :nasal)
    ("f" :voiceless :labial :dental :fricative)
    ("v" :voiced :labial :dental :fricative)
    ("th" :voiceless :dental :fricative)
    ("dh" :voiced :dental :fricative)
    ("t" :voiceless :alveolar :stop)
    ("d" :voiced :alveolar :stop)
    ("nh" :voiceless :alveolar :nasal)
    ("n" :voiced :alveolar :nasal)
    ("rh" :voiceless :alveolar :trill)
    ("r" :voiced :alveolar :trill)
    ("lh" :voiceless :alveolar :lateral :approximant)
    ("l" :voiced :alveolar :lateral :approximant)
    ("s" :voiceless :alveolar :sibilant)
    ("z" :voiced :alveolar :sibilant)
    ("sh" :voiceless :alveolar :palatal :fricative)
    ("zh" :voiced :alveolar :palatal :fricative)
    ("yh" :voiceless :palatal :approximant)
    ("y" :voiced :palatal :approximant)
    ("ngh" :voiceless :velar :nasal)
    ("ng" :voiced :velar :nasal)
    ("ch" :voiceless :velar :fricative)
    ("gh" :voiced :velar :fricative)
    ("wh" :voiceless :labial :velar :approximant)
    ("w" :voiced :labial :velar :approximant)
    ("h" :voiceless :glottal :fricative)))

(defun consonant (phonoclass)
  (first phonoclass))

(defun find-consonants (&rest phonoclasses)
  (let ((classes (if phonoclasses
                     (remove-if-not (lambda (class)
                                      (every (lambda (pc)(member pc class))
                                             phonoclasses))
                                    $consonant-phonoclasses)
                     $consonant-phonoclasses)))
    (mapcar #'consonant classes)))

(defun consonant-classes (consonant)
  (let ((found (assoc consonant $consonant-phonoclasses :test #'equalp)))
    (if found
        (cdr found)
        nil)))

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

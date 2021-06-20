(in-package :fi)

(defparameter $onset-consonants
  '("br" "c" "d" "h" "f" "l" "lh" "m" "lh" "n" "r" "rh" "th"))

(defparameter $onset-vowels
  '("ai" "e" "á" "é" "í" "ó" "ú"))

(defparameter $onsets (append $onset-consonants $onset-vowels))

(defparameter $coda-consonants
  '( "l" "m" "n" "nd" "r" "s" "sh" "th" "v" "w" "y"))

(defparameter $coda-vowels
  '("au" "eo" "ua" "ui" "á" "é" "í" "ó" "ú"))

(defparameter $codas (append $coda-consonants $coda-vowels))

(defparameter $medial-vowels
  '("ae" "au" "e" "i" "o" "ua" "ui" "uo" "á" "é" "í" "ó" "ú"))

(defparameter $short-vowels
  '("a" "e" "i" "o" "u"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $vowels
  (append $short-vowels
          $long-vowels))

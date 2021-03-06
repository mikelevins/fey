(in-package :fi)

(defparameter $onset-consonants
  '("ph" "f" "th" "s" "rh" "lh" "sh" "yh" "ch" "wh" "h"))

(defparameter $onset-vowels
  '("i" "u" "o" "e" "a" "í" "ú" "ó" "é" "á"))

(defparameter $onsets (append $onset-consonants $onset-vowels))

(defparameter $coda-consonants
  '("ph" "f" "th" "s" "rh" "lh" "sh" "yh" "ch" "wh" "h"))

(defparameter $coda-vowels
  '("i" "u" "o" "e" "a" "í" "ú" "ó" "é" "á"))

(defparameter $codas (append $coda-consonants $coda-vowels))

(defparameter $medial-vowels
  '("i" "u" "o" "e" "a" "í" "ú" "ó" "é" "á"))

(defparameter $short-vowels
  '("a" "e" "i" "o" "u"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $vowels
  (append $short-vowels
          $long-vowels))

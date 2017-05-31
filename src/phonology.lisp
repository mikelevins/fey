(in-package :fi)

;;; Fállálháthá => fá llá lhá -thá
;;; Heiróván    => hei ró vá -n
;;; Ímbrelín    => Ĩ bre llí -n
;;; Láthéyá     => llá thei -á
;;; Limnics     => lli mni -cs
;;; Baelveg     => bel ve -g
;;; Óndá        => õ -dá
;;; Urgónd      => úr gó -nd
;;; Dórgól      => dor gol -thá

;;; ---------------------------------------------------------------------
;;; consonants
;;; ---------------------------------------------------------------------

(defparameter $initial-consonants
  '("b" "bl" "br" "bw" "c" "cl" "cr" "cw" "d" "dr" "f" "fl" "fr" "fw" "g" "gl" "gr" "gw"
    "h" "hl" "hr" "hw" "lh" "ll" "lv" "m" "mn" "p" "pl" "pr" "pw" "r"  "t" "th" "thl" "thr" "w"))

(defparameter $final-consonants
  '("cs" "ch" "chs" "dh" "gh" "l" "lm" "lth" "lv" "m" "n" "ns" "nth" "s" "sc" "sh" "v" "wl" "wr" "y" "z" "zh"))

;;; ---------------------------------------------------------------------
;;; vowels
;;; ---------------------------------------------------------------------

(defparameter $short-vowels
  '("a" "e" "i" "o" "u"))

(defparameter $long-vowels
  '("á" "é" "í" "ó" "ú"))

(defparameter $nasal-vowels
  '("ã" "ẽ" "ĩ" "õ" "ũ"))

(defparameter $rhotic-vowels
  '("ar" "er" "ir" "or" "ur"))

(defparameter $lateralized-vowels
  '("al" "el" "il" "ol" "ul"))

(defparameter $diphthongs
  '("a" "e" "i" "o" "u"
    "ae" "ai" "ao" "au"
    "ea" "ei" "eo" "eu"
    "ia" "ie" "io" "iu"
    "oa" "oe" "oi" "ou"
    "ua" "ue" "ui" "uo"))

(defparameter $vowels (append $short-vowels $long-vowels $nasal-vowels $rhotic-vowels $lateralized-vowels $diphthongs))

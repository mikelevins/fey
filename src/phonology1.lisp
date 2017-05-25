(in-package :fi)

;;; ---------------------------------------------------------------------
;;; vowels
;;; ---------------------------------------------------------------------

(defparameter $short-vowels
  `("a" "e" "i" "o" "u"))

(defparameter $long-vowels
  `("a" "e" "i" "o" "u" "á" "é" "í" "ó" "ú"))

(defparameter $vowels
  (concatenate 'list $short-vowels $long-vowels))

;;; ---------------------------------------------------------------------
;;; diphthongs
;;; ---------------------------------------------------------------------

(defparameter $diphtongs
  '("ae" "ai" "ao" "au"
    "ea" "ei" "eo" "eu"
    "ia" "ie" "io" "iu"
    "oa" "oe" "oi" "ou"
    "ua" "ue" "ui" "uo"))

;;; ---------------------------------------------------------------------
;;; consonants
;;; ---------------------------------------------------------------------

(defparameter $base-consonants
  '("mh" "nh" "ngh" "f" "th" "s" "x" "ch" "h" "rh" "lh" "yh" "wh"))

(defparameter $voiced-consonants
  '("m" "n" "ng" "v" "dh" "z" "zh" "gh" "r" "l" "y" "w"))

(defparameter $stops
  '("p" "t" "c"))

(defparameter $voiced-stops
  '("b" "d" "g"))


;;; ---------------------------------------------------------------------
;;; initials
;;; ---------------------------------------------------------------------

(defparameter $initial-vowels (append $vowels $diphtongs))

(defparameter $initial-consonants
  '("mh" "nh" "f" "th" "s" "x" "ch" "h" "rh" "lh" "yh" "wh" "m" "n" "v" "dh" "r" "l" "y" "w"
    "fw" "thw" "sw" "xw" "chw" "hw" "rh" "lh" "yh" "wh" "mw" "nw" "vw" "dhw" "r" "l" "y" "w"
    "fr" "thr" "sr" "xr" "chr" "hr" "vr" "dhr"
    "fl" "thl" "sl" "xl" "chl" "vl" "dhl"))

(defparameter $final-consonants
  '("mh" "nh" "f" "th" "s" "x" "ch" "h" "rh" "lh" "yh" "wh" "m" "n" "ng" "v" "dh" "r" "l" "y" "w"
    "wf" "wth" "ws" "wx" "wch" "wm" "wn" "wng" "wv" "wdh" "wr" "wl" "wy"
    "rf" "rth" "rs" "rx" "rch" "rm" "rn" "rng" "rv" "rdh" "rl" "ry"
    "lf" "lth" "ls" "lx" "lch" "lm" "ln"  "lv" "ldh" "ly"))


(defparameter $medial-consonants
  (let ((consonants '("th" "s" "x" "ch" "m" "n" "ng" "v" "dh" "z" "zh" "gh" "r" "l" "y" "w")))
    (reduce #'append
            (loop for left in consonants
               collect (loop for right in consonants
                          collect (concatenate 'string left right))))))



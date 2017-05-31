(in-package :fi)


(defun all-medial-pairs ()
  (reduce #'append
          (loop for l in $core-finals
             collect (loop for r in $core-initials
                        collect (list l r)))))

(defparameter $all-medial-pairs
  (all-medial-pairs))

;;; (progn (terpri)(loop for m in (all-medials) do (format t "~A~%" m)))

#|
(with-open-file (out "/Users/mikel/Workshop/worldbuilding/Ymra/Languages/Fey/fey/src/medial-pairs.lisp"
                     :direction :output
                     :if-exists :supersede)
  (loop for m in (all-medials)
     do (format out "(~S nil)~%" m)))
|#

(defparameter $unique-finals
  (delete-duplicates
   (mapcar #'first (all-medials))
   :test #'equalp))

(defparameter $final->medial-table
  '(("ch" . "y")
    ("chs" . "z")
    ("cs" . "z")
    ("d" . "d")
    ("dh" . "dh")
    ("f" . "v")
    ("h" . "")
    ("l" . "l")
    ("m" . "m")
    ("n" . "n")
    ("ng" . "n")
    ("r" . "r")
    ("s" . "z")
    ("sh" . "zh")
    ("th" . "dh")
    ("v" . "v")
    ("w" . "w")
    ("y" . "y")))

(defparameter $unique-initials
  (delete-duplicates
   (mapcar #'second (all-medials))
   :test #'equalp))

;;; (progn (terpri)(loop for m in $unique-initials do (format t "(~S . nil)~%" m)))

(defparameter $initial->medial-table
  '(("b" . "b")
    ("bl" . "bl")
    ("br" . "br")
    ("bw" . "bw")
    ("c" . "g")
    ("ch" . "gh")
    ("cw" . "gw")
    ("d" . "d")
    ("dr" . "dr")
    ("f" . "v")
    ("fl" . "vl")
    ("fr" . "vr")
    ("fw" . "vw")
    ("g" . "g")
    ("gl" . "gl")
    ("h" . "")
    ("hr" . "r")
    ("l" . "l")
    ("m" . "m")
    ("n" . "n")
    ("p" . "b")
    ("pw" . "bw")
    ("r" . "r")
    ("s" . "z")
    ("sh" . "zh")
    ("sl" . "zl")
    ("sw" . "zw")
    ("t" . "d")
    ("th" . "dh")
    ("thw" . "dhw")
    ("v" . "v")
    ("w" . "w")
    ("y" . "y")))

(defun final->medial (final)
  (let ((found (assoc final $final->medial-table :test #'equalp)))
    (if found
        (cdr found)
        nil)))

(defun initial->medial (final)
  (let ((found (assoc final $initial->medial-table :test #'equalp)))
    (if found
        (cdr found)
        nil)))

(defparameter $voice-adjusted-medial-pairs
  (delete-duplicates
   (loop for pair in $all-medial-pairs
      collect (list (final->medial (first pair))
                    (initial->medial (second pair))))
   :test #'equalp))

;;; (progn (terpri)(loop for m in $voice-adjusted-medial-pairs do (format t "(~S . nil)~%" m)))

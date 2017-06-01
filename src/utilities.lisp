(in-package :fi)

(defmethod subtract-lists ((left null)(right null)) nil)
(defmethod subtract-lists ((left cons)(right null)) left)

(defmethod subtract-lists ((left cons)(right cons))
  (subtract-lists (remove (car right) left :test #'equalp)
                  (cdr right)))

(defmethod prefix-match ((prefix sequence)(candidate sequence))
  (let ((pref-len (length prefix)))
    (if (> pref-len (length candidate))
        nil
        (and (equalp prefix (subseq candidate 0 pref-len))
             prefix))))

(defmethod suffix-match ((suffix sequence)(candidate sequence))
  (let ((suff-len (length suffix))
        (cand-len (length candidate)))
    (if (> suff-len cand-len)
        nil
        (and (equalp suffix (subseq candidate (- cand-len suff-len) cand-len))
             suffix))))

(defmethod any ((s sequence))
  (elt s (random (length s))))

(defun compose (f1 f2)
  (function
   (lambda (x)
    (funcall f1 (funcall f2 x)))))

(defun unichar (&rest chars)
  (coerce (apply #'vector chars) 'string))

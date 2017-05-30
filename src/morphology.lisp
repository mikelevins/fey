(in-package :fi)


(defun vowel? (x)
  (member (string x)
          $vowels
          :test #'equalp))

(defun separate-clusters (str)
  (let ((vowel? (vowel? (elt str 0)))
        (change-indexes (list 0)))
    (loop for i from 0 below (length str)
       do (progn
            (if vowel?
                (unless (vowel? (elt str i))
                  (push i change-indexes)
                  (setf vowel? nil))
                (when (vowel? (elt str i))
                  (push i change-indexes)
                  (setf vowel? t)))))
    (loop for changes on (reverse change-indexes)
       collect (subseq str (first changes)(second changes)))))

(defparameter $core-syllables
  '("bael"
    "bech"
    "bey"
    "bís"
    "boam"
    "bouth"
    "bór"
    "bow"

    "blán"
    "bláth"
    "bler"
    "blés"
    "bléy"
    "bliw"
    "blúm"

    "braichs"
    "bréch"
    "brel"
    "breth"
    "bruar"
    "brún"
    "brús"

    "bwán"
    "bway"
    "bwel"
    "bwéth"
    "bwíng"
    "bwír"
    "bwís"
    "bwém"

    "caen"
    "cál"
    "cauth"
    "cer"
    "ciad"
    "codh"
    "cúch"
    "cúw"

    "chaul"
    "chaur"
    "cháy"
    "chéd"
    "chón"
    "chús"
    
    "cwaer"
    "cwaeth"
    "cwáf"
    "cwing"
    "cwóch"
    "cwól"

    "daich"
    "dál"
    "dauth"
    "dew"
    "diadh"
    "dócs"
    "dór"
    "dun"

    "dráw"
    "dráy"
    "dréng"
    "drín"
    "dról"
    "dróm"
    "drós"
    "drúch"

    "fál"
    "fauv"
    "feith"
    "fém"
    "féy"
    "fich"
    "fídh"
    "fíng"
    "fóh"
    "fór"
    "fuin"

    "flám"
    "flén"
    "fliodh"
    "fliw"
    "flóch"
    "fluor"
    "flúv"

    "frachs"
    "fráy"
    "frem"
    "fril"
    "fróch"
    "fróng"
    "frów"

    "fwal"
    "fwáw"
    "fwáy"
    "fwéd"
    "fwer"
    "fwéth"
    "fwev"
    "fwin"

    "gás"
    "gédh"
    "gém"
    "géy"
    "gíth"
    "gól"
    "gon"
    "gúd"

    "glács"
    "glaeth"
    "glés"
    "glév"
    "glích"
    "gloel"
    "glór"

    "hádh"
    "hál"
    "háy"
    "haow"
    "háth"
    "hecs"
    "heir"
    "hiud"
    "hoen"

    "hreif"
    "hréth"
    "hriw"
    "hroan"
    "hródh"
    "hrouch"
    "hruad"
    "hrúl"

    "lál"
    "lán"
    "lár"
    "láth"
    "lauv"
    "lecs"
    "leod"
    "léy"
    "liadh"
    "liew"
    "lim"
    "los"
    "luich"

    "mách"
    "main"
    "mál"
    "más"
    "maudh"
    "meam"
    "mer"
    "mith"
    "muif"
    "múw"

    "náf"
    "naum"
    "nial"
    "nics"
    "nín"
    "nís"
    "nuich"
    "núr"
    "núth"

    "pád"
    "pein"
    "pel"
    "píchs"
    "pith"
    "píw"
    "poar"
    "pos"

    "pwach"
    "pwádh"
    "pweal"
    "pwiad"
    "pwen"
    "pwer"
    "pwéy"
    "pwíth"
    "pwóh"

    "rash"
    "ráth"
    "raun"
    "rés"
    "riar"
    "rícs"
    "ríng"
    "roul"
    "róv"

    "sáw"
    "séy"
    "siol"
    "soath"
    "sóchs"
    "sóm"
    "song"
    "súv"

    "sháchs"
    "sháh"
    "sháv"
    "shein"
    "sheth"
    "shéy"
    "shuar"
    "shúf"

    "slaith"
    "sleid"
    "slem"
    "slén"
    "slích"
    "slíng"
    "sliw"
    "sloucs"

    "swaich"
    "swáth"
    "swéf"
    "sweis"
    "swél"
    "swín"
    "swúr"

    "táng"
    "táth"
    "tauch"
    "táw"
    "tay"
    "tiadh"
    "tion"
    "tol"
    "tuir"
    "tuiv"

    "thách"
    "thán"
    "thaum"
    "thév"
    "théy"
    "thial"
    "thing"
    "thúw"

    "thwaer"
    "thwaes"
    "thwecs"
    "thwém"
    "thwel"
    "thwéy"
    "thwiad"

    "vádh"
    "ván"
    "vaum"
    "véch"
    "víw"
    "vuil"
    "vuir"
    "vuith"

    "waich"
    "weidh"
    "wém"
    "wes"
    "wév"
    "wey"
    "wol"
    "won"
    "wuad"
    "wur"

    "yen"
    "yédh"
    "yer"
    "yés"
    "yim"
    "yuel"
    "yung"
    "yúth"
    ))

(defparameter $core-initials
  (sort
   (delete-duplicates
    (mapcar (compose #'car #'separate-clusters)
            $core-syllables)
    :test #'equalp)
   #'string<))

(defparameter $core-finals
  (sort
   (delete-duplicates
    (mapcar (compose (compose #'first #'last) #'separate-clusters)
            $core-syllables)
    :test #'equalp)
   #'string<))

(defun core-consonant-patterns ()
  (sort
   (mapcar (lambda (ls)
             (remove-if (lambda (s)(member s $vowels :test #'equalp))
                        ls))
           (mapcar #'separate-clusters $core-syllables))
   (lambda (x y)(equalp (first x)(first y)))))

(defun possible-consonant-patterns ()
  (let ((initial-consonants (remove-if (lambda (s)(member s $vowels :test #'equalp))
                                       $core-initials))
        (final-consonants (remove-if (lambda (s)(member s $vowels :test #'equalp))
                                     $core-finals)))
    (reduce #'append
            (loop for left in initial-consonants
               collect (loop for right in final-consonants
                          collect (list left right))))))

(defun available-consonant-patterns ()
  (subtract-lists (possible-consonant-patterns)
                  (core-consonant-patterns)))

(defparameter $forbidden-initial-consonants
  '("gh" "ng" "z" "zh"))

(defparameter $forbidden-final-consonants
  '("b" "p" "z"))

(defparameter $forbidden-final-vowels
  '("i"))

(defparameter $allowed-medial-consonants
  '())

(defun gen-cv (&optional (count 1))
  (flet ((gen ()
           (let ((initials (subtract-lists (consonants)
                                           $forbidden-initial-consonants))
                 (finals (subtract-lists $vowels
                                         $forbidden-final-vowels)))
             (concatenate 'string
                          (any initials)
                          (any finals)))))
    (loop for i from 0 below count collect (gen))))

(defun gen-cvc (&optional (count 1))
  (flet ((gen ()
           (let ((initials (subtract-lists (consonants)
                                           $forbidden-initial-consonants))
                 (finals (subtract-lists (consonants)
                                         $forbidden-final-consonants)))
             (concatenate 'string
                          (any initials)
                          (any $vowels)
                          (any finals)))))
    (loop for i from 0 below count collect (gen))))




(defun genword (&optional start)
  (let ((pat (if start
                 (let ((pats (remove-if-not (lambda (p)(equalp start (first p)))
                                            (available-consonant-patterns))))
                   (any pats))
                 (any (available-consonant-patterns))))
        (v (any $vowels)))
    (concatenate 'string
                 (first pat)
                 v
                 (second pat))))


(defun genwords (n &optional start)
  (loop for i from 0 below n collect (genword start)))


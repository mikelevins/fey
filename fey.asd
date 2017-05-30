;;;; fey.asd

(asdf:defsystem #:fey
  :description "Describe fey here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "utilities")
                                     ;; words
                                     (:file "phonology")
                                     (:file "morphology")
                                     ;; grammar
                                     ;; interpreter internals
                                     (:file "unify")
                                     (:file "prolog")
                                     ;; interpreter toplevel
                                     (:file "fey")))))


;;; (asdf:load-system :fey)

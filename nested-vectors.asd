;;;; nested-vectors.asd

(asdf:defsystem #:nested-vectors
  :serial t
  :description "Array like storage, but in terms of nested vectors"
  :author "Mirko Vukovic <mirko.vukovic@gmail.com>"
  :license "MIT"
  :depends-on ("lisp-unit"
               "alexandria"
	       "anaphora"
               "antik"
	       "iterate"
	       "unified-sequences")
  :components
  ((:module init
	    :pathname #P"./"
	    :serial t
	    :components ((:file "nested-vectors-package-def")))
   (:module nested-vectors
	    :pathname #P"./"
	    :serial t
	    :components ((:file "nested-vectors-class-definition")
			 (:file "nested-vectors-initializing")
			 (:file "nested-vectors-accessors")
			 (:file "nested-vectors-push-extend")))
   (:module nth-row
	    :pathname #P"./"
	    :serial t
	    :components ((:file "nth-row-class-definition+accessors")))
   (:module iterators
	    :pathname #P"./"
	    :serial t
	    :components ((:file "iterators")))))


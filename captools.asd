(asdf:defsystem #:captools
  :description "Capchase Tools"
  :author "marce@capchase.com"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :depends-on (#:lparallel #:arnesi #:cl-dbi #:cl-ppcre #:com.inuoe.jzon #:iterate #:cl-cram #:cl-ansi-text)
  :components ((:file "src/config")
               (:file "src/db")
               (:file "captools")))

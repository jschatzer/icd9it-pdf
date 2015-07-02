;;;; icd9it-pdf.asd

(asdf:defsystem #:icd9it-pdf
  :serial t
  :description "Describe icd9 here"
  :version "0.1.0"
  :author "Schatzer Johann <j.schatzer@tin.it>"
  :license "Code is free, about initfiles not shure"
    :defsystem-depends-on (:asdf-package-system)
  :depends-on (#:onlisp #:cl-pack #:fare-utils
               perlre stdutils 
               )
  :components ((:file "package")
               (:file "initicd")
               (:file "initdg")
               (:file "initth")
               (:file "icd")
               (:file "diagnosi")
               (:file "interventi")
               (:file "icd9it-pdf")
;               (:file "tests")
               ))

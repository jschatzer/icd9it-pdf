;;;; package.lisp


(defpackage icd
  (:use #:cl)
  (:export 
   ;UTILITIES
   key
   aftl
   afts
   re-fns
   string-l
reduce-space
   ;W1
   #:pdf-to-pages
   ;W2
   #:pad-text
   #:split-page
   #:edit-single-page-helper
   ;W3
   #:optimize-text
;   #:edit-uc-header
   #:uc-header
   #:tag-items
   #:split-into-items
   ;W4
   #:load-ht
   #:connect-first2lines-if-
   #:defoffbar
   #:defmanbar  
   #:bar-h
   ;W5
   #:complete-code-h
   ;W6
   #:accented-chrs
   #:grklammer-fns
   #:longcode-fns
   ;W7
   #:insert-key
   #:insert-h1
   ;W8
   #:create-perlarry-file
   #:create-perlarry-file-all
   ;W9
create-lisptree-file
   ;for tests
   #:key-from-bar-all))

(defpackage #:icd9dg
;  (:use #:cl #:icd)
  (:use #:cl)
  (:export #:pdf-to-perlarray))

(defpackage #:icd9th
;  (:use #:cl #:icd)
  (:use #:cl)
  (:export #:pdf-to-perlarray))

;(defpackage #:icd9
(defpackage #:icd9it-pdf
;  (:use #:cl #:icd)
  (:use #:cl)
  (:export #:icd-dg-th
perlarray-to-lisptree
   ))

#|
#;(defpackage #:icd9tests
  (:use #:cl #:icd))
|#

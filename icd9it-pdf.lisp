;idc9.lisp
;(in-package #:icd9)
(in-package icd9it-pdf)

; for testing  only ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
(defun t-icd-dg-th ()
  (create-perlarry-file-all (icd9dg::pdf-to-items) 
                            (icd9th::pdf-to-items)
                            "/tmp/dt"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,


(defun icd-dg-th ()
  (icd:create-perlarry-file-all (icd9dg::pdf-to-items) 
                            (icd9th::pdf-to-items)
;                            "~/Programming/Projects/IcdIt2007/dataDgTh1"))
                            "~/src/lisp/icd9it-pdf/data/dataDgTh8"))

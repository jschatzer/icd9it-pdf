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
                            "~/src/lisp/icd9it-pdf/data/dataDgTh40"))



(defun perlarray-to-lisptree ()
  (icd:create-lisptree-file "~/src/lisp/icd9it-pdf/data/dataDgTh35" 
                            "~/src/lisp/icd9it-pdf/data/dataIcd9itLisp3"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END
;(defun icd-dg-th-to-lisp ()
#;(defun icd9it-lisp ()
  (icd:create-lisptree-file (icd9it-pdf:icd-dg-th) "dataIcd9itLisp"))

;run (icd9it-pdf:create-lisptree-file "~/src/lisp/icd9it-pdf/data/dataDgTh9" "icdtreetest-without-path")
#;(defun create-lisptree-file (infile outfile)
  "edit a copy of the perl array file"
  (clesh:script (format nil "sed '1,2d; $d; s/,$//' ~a > temp1" infile))
  (with-open-file (o outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format o "~s" 
            (funcall (o:ttrav #'cons (lambda (stg) (if (stringp stg) (#~s'[^|]+\|'' stg) stg)))
                     (with-open-file (i "temp1") (make-tree i))))))

                  

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
  (create-perlarry-file-all (icd9dg::pdf-to-items) 
                            (icd9th::pdf-to-items)
;                            "~/Programming/Projects/IcdIt2007/dataDgTh1"))
                            "~/src/lisp/icd9it-pdf/Data/dataDgTh1"))

@END

;6.11.13, diagnosi fails, interventi pass
(o:deftest compare-sets ()
           (o:check
             (= (+ 1 2) 3)
             ;diagnosi
             (subsetp (o:hash-keys icd9dg::off-ht)
                      (mapcar #'icd::key-from-bar-all (icd9dg::pdf-to-items))
                      :test #'equal)            
             ;interventi
             (subsetp (o:hash-keys icd9th::off-ht)
                      (mapcar #'icd::key-from-bar-all (icd9th::pdf-to-items))
                      :test #'equal)            
             ))

(o:deftest diagnosi-test ()
           (o:check
             (= (+ 1 2) 3)
             ;diagnosi
             (subsetp (o:hash-keys icd9dg::off-ht)
                      (mapcar #'icd::key-from-bar-all (icd9dg::pdf-to-items))
                      :test #'equal)            
             ))

(o:deftest interventi-test ()
           (o:check
             (= (+ 1 2) 3)
             ;interventi
             (subsetp (o:hash-keys icd9th::off-ht)
                      (mapcar #'icd::key-from-bar-all (icd9th::pdf-to-items))
                      :test #'equal)            
             ))






;;END ;;;;
;;  drafts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



#;(defun icd-dg-th ()
  (create-perlarry-file-all (icd9dg::return-all-entries) 
                            (icd9th::return-all-entries)
                            "~/Programming/Projects/IcdIt2007/dataDgTh"))



#;(defun icd-dg-th ()
  (create-perlarry-file-all (icd9dg::return-all-entries) 
                            (icd9th::return-all-entries)
                            "~/Programming/Projects/IcdIt2007/dataIcd9CM2007It"))


;;; tests, 22.10.13, both pass
#;(o:deftest compare-sets ()
           (o:check
             (= (+ 1 2) 3)
             ;diagnosi
             (subsetp (o:hash-keys icd9dg::control-ht)
                      (mapcar #'icd::key-from-bar-all (icd9dg::return-all-entries))
                      :test #'equal)            
             ;interventi
             (subsetp (o:hash-keys icd9th::ctrlhash-interventi)
                      (mapcar #'icd::key-from-bar-all (icd9th::return-all-entries))
                      :test #'equal)            
             ))



;diagnsi t
;(time ; 108 sec beide
;  (compare-sets))



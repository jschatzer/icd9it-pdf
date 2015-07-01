;;;;; for testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :icd9tests)

;from admin.lisp
;(defun fehlende-codes ()
(defun new-codes (items official-ht)
  (let ((l1 (o:hash-keys official-ht))
        (l2 (mapcar #'o:key items)))
    (remove nil
            (mapcar (lambda (x)
                      (unless (find x l2 :test #'equal) x)) 
                    l1))))

(defun fehlende-codes (items official-ht)
  (let ((l1 (o:hash-keys official-ht))
        (l2 (mapcar #'key-from-bar-all items)))
    (remove nil
            (mapcar (lambda (x)
                      (unless (find x l2 :test #'equal) x)) 
                    l1))))
;
;(fehlende-codes)



;::::::::::::::::::::
;from diagnosi.lisp
;::::::::::::::::::::
;
(defun codes-not-found (items ht)
  "print codes not found in entry-list; siehe list notfound in icd"
    (dolist (i (o:hash-keys ht))
      (unless (find i items :test #'equal :key #'o:key)
        (print i))))

(defun codes-not-found (items ht)
  "print codes not found in entry-list; siehe list notfound in icd"
    (loop for i in (o:hash-keys ht)
          collect (unless (find i items :test #'equal :key #'key-from-bar-all) i)))




#;(defun codes-not-found (items ht)
  "print codes not found in entry-list; siehe list notfound in icd"
    (o:while (i (o:hash-keys ht))
      (unless (find i items :test #'equal :key #'o:key)
        (print i))))



#|
(defun codes-not-found (entries)
  "print codes not found in entry-list; siehe list notfound in icd"
    (dolist (e (o:hash-keys control-ht))
      (unless (find e entries :test #'equal :key #'o:key)
        (print e))))


(defun list-of-all-headers (lst file)
  "print all headers"
  (with-open-file (strm file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar (lambda (x y)
              (if y
                (format strm "~s~%" x)))
            lst 
            (maplist #'header-p lst))))

(defun list-of-multi-line-headers (lst file)
  "print all headers, use icd-c"
  (with-open-file (strm file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar (lambda (x y)
              (if (and y (find #\newline x :test #'equal))
                (format strm "~s~%" x)))
            lst 
            (maplist #'header-p lst))))

(defun multi-line-h3h4 (lst file)
  "print only h3 h4 headers"
  (with-open-file (strm file :direction :output :if-does-not-exist :create :if-exists :supersede)
    (mapcar (lambda (x y)
              (if (and y (find #\newline x :test #'equal))
                (format strm "~s~%" x)))
            lst 
            (maplist #'h3h4-p lst))))

(defun h3h4-p (lst)
  "test if single-node-entry in ICD9CMitDg is header or code,
  use it with maplist"
  (let ((node (key (car lst)))
        (peeknode (key (cadr lst))))
    (cond ((h3 node peeknode) t)
          ((h4 node peeknode) t)
          (t nil))))

;dzt nur für Tests gebraucht
(defun header-p (lst)
  "test if single-node-entry in ICD9CMitDg is header or code,
  use it with maplist"
  (let ((node (key (car lst)))
        (peeknode (key (cadr lst))))
    (cond ((h1 node) t)
          ((h2 node) t)
          ((h3 node peeknode) t)
          ((h4 node peeknode) t)
          (t nil))))

;::::::::::::::::::::
;;; from interventi.lisp
;::::::::::::::::::::
;







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ad tests  

(defun code-idem (s c)
  (if (>= (length s) (length c))
    (string-equal c (subseq s 0 (length c)))))


;show non matching entries
(defun bb (lst)
  (let ((nr 0))
    (dolist (e lst nr) 
      (o:aif (gethash (o:key e) icd9dg::ctr-code-ht)
             (cond ((not (code-idem e (edit-c o:it))) (setf nr (1+ nr)) (print e)))))
    nr))

(defun edit-c (strg)
  (icd::accs-to-chrs
    (substitute #\' #\’ 
      (ppcre:regex-replace-all ",(?! )" strg ", "))))

(defun edit-c (strg)
  (eval `(funcall (o:compose ,@(icd::subs-functions *replacements*)) ,(icd::accs-to-chrs strg))))
(o:p *replacements*
     '((",(?! )" . ", ")
       ("’" . "'")
       ("SAZARY" . "Sezary")))


;;;;
;interventi ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
(defun interventi (lst)
  (let ((nr 0))
    (dolist (e lst nr) 
      (o:aif (gethash (o:key e) icd9th::ctrlhash-interventi)
             (cond ((not (code-idem e (edit-c o:it))) (setf nr (1+ nr)) (print e)))))
    nr))

(defun edit-c (strg)
  (eval `(funcall (o:compose ,@(icd::subs-functions *replacements*)) ,(icd::accs-to-chrs strg))))

(o:p *replacements*
     '((",(?! )" . ", ")
       ("’" . "'")
       ("\\"" . "")  ;"
       )))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;diagnosi
;in 12 sollten alle matchen
;----------------
;1    7
;2   12  viele SAZARY, korrektur geht voererst nicht
;3    8
;4    3
;5  139 
;6   69
;7    8
;8   20
;9   81
;10  21
;11 256 <--
;12   0 <==
;13  93
;14  29
;15  22
;16   6
;17  36
;18  39

(o:p entries (icd9dg::return-entries-t 18))

(bb entries)

(gethash "202.20" icd9dg::ctr-code-ht)

(dribble "/tmp/x")

(dribble)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; intervetni ,----------------------------------------

;0
;1
;2  0
;3  0
;4  0
;5  0
;6  0
;7
;8  0
;9  8
;10 2
;11 0
;12 2
;13 0
;14
;15 0
;16


; error: The function ICD9TH::STRG is undefined
;0
;1
;7
;14
;16

|#

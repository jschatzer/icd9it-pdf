;interventi.lisp
(in-package #:icd9th)

;;; for tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pages ()
  (icd:pdf-to-pages "Interventi.pdf" *chapters* 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 21.3.15 for testing only
(defun t-pdf-to-perlarray (&optional (chapter 20))
  (let ((strg (if (= 20 chapter) 
                (pdf-to-items chapter)
                (pdf-to-items-t chapter))))
    (create-perlarry-file strg "/tmp/i")))

;--------------
;MAIN FUNCTION
;--------------
(defun pdf-to-perlarray (&optional (chapter 20))
  (let ((strg (if (= 20 chapter) 
                (pdf-to-items chapter)
                (pdf-to-items-t chapter))))
;    (create-perlarry-file strg "~/src/lisp/icd9it-pdf/Data/dataInterventi")))
    (icd:create-perlarry-file strg "~/src/lisp/icd9it-pdf/data/dataInterventi_24")))    ; -----<------------

(defun items ()
  (column-to-items 
    (pages-to-column 
      (icd:pdf-to-pages "Interventi.pdf" *chapters* 20))))

(defparameter items (stdutils:memoize #'items))

(defun pdf-to-items ()
  (insert-path ;W7
    (tune-items  ;W6
      (complete-code ;W5
        (mark-comments (funcall items))))))  ;W4



#;(defun pdf-to-items (&optional (chapter 20))
  (insert-path ;W7
    (tune-items  ;W6
      (complete-code ;W5
        (mark-comments  ;W4
          (column-to-items   ;W3
            (pages-to-column     ;W2
              (icd:pdf-to-pages "Interventi.pdf" *chapters* chapter)))))))) ;W1

;without complete-code 
(defun pdf-to-items-t (&optional (chapter 20))
  (insert-path ;W7
    (tune-items  ;W6
      (mark-comments  ;W4
        (column-to-items   ;W3
          (pages-to-column     ;W2
            (icd:pdf-to-pages "Interventi.pdf" *chapters* chapter))))))) ;W1

;------------
;WORKFLOW 1 pdf-to-pages
;------------
;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pages-to-column (lst)
   (stdutils:list-to-delimited-string (icd:aftl (#'icd:split-page #'edit-single-page #'edit-single-page2 #'icd:pad-text) lst)))

(defun edit-single-page (page) (icd:edit-single-page-helper page *tagged-entries*))

;zusätzliche edits
(defun edit-single-page2 (page)
  "reduce spaces between key and text, to enable page-splitting"
  (funcall (stdutils:compose #'c85) page))

(defun c85 (strg) (#~s'9(?=85 Interventi sulla mammella)'' strg))

;------------
;WORKFLOW 3 column-to-items
;------------
(defun column-to-items (stg)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (icd:uc-header (icd:optimize-text (icd:reduce-space stg))))))

(defun string-to-list (stg) (icd:split-into-items (icd:tag-items *tag-re* stg))) ;re for regular-expressions
(defun edit-column (stg) (icd:afts (icd:re-fns *column-edits*) stg))

;------------
;WORKFLOW 4 mark comments
;------------
(defun mark-comments (lst) (mapcar #'insert-bar lst))
(defun insert-bar (i) (or (code-bar i) (header-bar i)))

(defparameter code-ht (make-hash-table :test #'equal))
(icd:load-ht code-ht "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv")

(defun code-bar (i)
	(lol:aif (gethash (icd:key i) code-ht)
					 (or (ignore-errors (icd::rm-nsb (#~s/(format nil "(?<=~a)$" (ppcre:quote-meta-chars lol:it))/"|"/im (#~s' $'' i))))
							 (cond ((member (icd:key i) *cbar3* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
										 ((member (icd:key i) *cbar2* :test #'string=) (icd::rm-nsb (#~s'(\n.+)\n'\1|' i)))
										 (t (#~s'\n'|' i))))))

(defun header-bar (i)
	(cond	((member (icd:key i) *hbar4* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+\n.+)\n'\1|' i)))  
				((member (icd:key i) *hbar3* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
				((member (icd:key i) *hbar2* :test #'string=) (icd::rm-nsb (#~s'(\n.+)\n'\1|' i)))
				(t (#~s'\n'|' i))))

;------------
;WORKFLOW 5 complete-entries
;------------
(defun complete-code (items)
  (icd:complete-code-h items *new-codes* *c* code-ht))

;------------
;WORKFLOW 6 tune items
;------------
(defun tune-items (lst)
  (icd:aftl (#'longcode #'grklammer #'icd:accented-chrs) lst))

(defun grklammer (item)
  (icd:afts (icd:grklammer-fns *grkl*) item))

(defun longcode (item)
  (icd:afts (icd:longcode-fns *longcode*) item))

;------------
;;WORKFLOW 7 insert-path
;------------
(defun insert-path (lst)
  (funcall (stdutils:compose #'icd:insert-h1 #'icd:insert-key) lst))

;------------
;WORKFLOW 8 create-perlarry-file
;------------

;------------
;benchmark
;------------
;(defun pdf-to-items (&optional (chapter 20))
;  (insert-path ;W7
;    (tune-items  ;W6


(defun time-complete-code ()
 (time      (defparameter x (complete-code ;W5
        (mark-comments  ;W4
          (column-to-items   ;W3
            (pages-to-column     ;W2
              (icd:pdf-to-pages "Interventi.pdf" *chapters* 20))))))))



(defun benchmark-interventi ()
  (print "int/1")
;(time (o:p pdf-to-pages (icd9th::pdf-to-pages "Interventi.pdf" icd9th::*chapters* 20)))
(time (o:p pdf-to-pages (icd:pdf-to-pages "Interventi.pdf" icd9th::*chapters* 20)))
  (print "int/2")
(time (o:p pages-to-column (icd9th::pages-to-column pdf-to-pages)))
(time (o:p column-to-items (icd9th::column-to-items pages-to-column)))
(time (o:p mark-comments (icd9th::mark-comments column-to-items)))
(time (o:p complete-code (icd9th::complete-code mark-comments)))   ; geth nicht ???
(time (o:p tune-items (icd9th::tune-items complete-code)))
(time (o:p insert-path (icd9th::insert-path tune-items)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END
#;(defun column-to-items (stg)
  "convert a single-column-string to a list of items"
  (string-to-list (connect-first2lines-if- (edit-column (uc-header (optimize-text (icd:reduce-space stg)))))))





(defun insert-bar (item)
  (or (offbar item)
      (manbar item)
      (defmanbar item)))



#;(defun column-to-items (stg)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (icd:uc-header (icd:optimize-text (icd:reduce-space (icd:connect-first2lines-if- stg)))))))

;idem wie oben
(defun column-to-items (stg)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (icd:uc-header (icd:optimize-text (icd:reduce-space-first-line (icd:connect-first2lines-if- stg)))))))




(defun insert-bar (item)
  (or (offbar item)
      (manbar item)
      (icd:defmanbar item)))






;;debugger invoked on a TYPE-ERROR in thread #<THREAD "main thread" RUNNING {1002D4E6C3}>: The value NIL is not of type UNSIGNED-BYTE
#;(defun insert-bar (i)
  (or (code-bar i)
      (head-bar i)
      (icd:defmanbar (icd:connect-first2lines-if- i))))

;(ICD:COMPLETE-CODE-H #<unavailable argument> ("90.0" "90.1" "90.2" "90.3" "90.4" "90.5" "90.6" "90.7" "90.8
#;(defun insert-bar (i)
  (or (code-bar i)
      (head-bar i)
      (icd:defmanbar (icd:connect-first2lines-if- i))
      (#~s'$'XXX' i)))

#;(defun insert-bar (i)
  (or (code-bar i)
      (head-bar i)
      (icd:default-head-bar (icd:connect-first2lines-if- i))
      (#~s'$'XXX' i)))


;;; 17.7.15 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;beide gehen
;(defun ins-bar (i c) (let ((p (length c))) (lol:mkstr (subseq i 0 p) #\| (subseq i p))))
;(defun ins-bar (i c &aux (p (length c))) (lol:mkstr (subseq i 0 p) #\| (subseq i p)))  ; insert hash-controlled bar
;(defun insert-hash-controlled-bar (i c &aux (p (length c))) (lol:mkstr (subseq i 0 p) #\| (subseq i p)))  ; insert hash-controlled bar



#|
 (defun hash-bar (i h)
  (let ((ctrl-length (length (gethash (key i) h))))
    (if (plusp ctrl-length) 
      (#~s/i/(lambda (x) / i))
      (setf (elt i (decf ctrl-length)) #\|))
    i))


(defun code-bar (i h)
  (let ((ctrl-length (length (gethash (key i) h))))
    (if (plusp ctrl-length) 
      (#~s'$'|' i)
      i)))

;geht
(defun code-bar (i h)
  (if (gethash (key i) h)
    (#~s'$'|' i)
    i))

 (defun header-bar (i h)
  (if (gethash (key i) h)
    (#~s'$'|' i)
    i))

(defun sub-s1 (s p) (subseq s 0 p))
(defun sub-s2 (s p) (subseq s p))
;(defun ins-bar (i c) (let ((p (length c))) (#~s/(format nil "(~a)(~a)" (sub-s1 i p) (sub-s2 i p))/"\\1|\\2"/ "abc")))
(defun ins-bar (i c) (let ((p (length c))) (lol:mkstr (sub-s1 i p) #\| (sub-s2 i p))))


(ins-bar "abc" "ab") ; "ab|c"

|#





(defun insert-bar (item)
  (or (offbar item)
      (manbar item)
      (icd:defmanbar (icd:connect-first2lines-if- item))))

(defun offbar (item)
  (icd:bar-h item off-ht #'manbar))

(defun manbar (item)
  (icd:bar-h item man-ht #'icd:defmanbar))


;;;;;;;;;;;;;;;;;;;;;;;;;;
#;(defun insert-bar (i)
  (or (code-bar i)
      (head-bar i)
      (icd:default-head-bar (icd:connect-first2lines-if- i))))

(defun insert-bar (i)
;  (or (code-bar i)
  (or (code-bar (icd:connect-first2lines-if- i))
  i))


#;(defun insert-bar (i)
  (code-bar i))


(defun code-bar (i) (icd:insert-hash-controlled-bar i off-ht))
(defun head-bar (i) (icd:insert-hash-controlled-bar i man-ht))

;default-code-bar
;default-head-bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter off-ht (make-hash-table :test #'equal) "official-code-ht")

#;(icd:string-l (icd:afts (icd:re-fns *edit-official-code*) (stdutils:slurp-file "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv"))
  (setf (gethash (icd:key stdutils:it) off-ht) stdutils:it))  ; it kommt von awhen2

(with-open-file (i "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv")
(loop for l = (read-line i nil) 
while l do (setf (gethash (icd:key l) off-ht) l)))




(defparameter man-ht (make-hash-table :test #'equal) "manual-bar-ht")
(icd:load-ht man-ht *man-ht*)

#;(defun complete-code (items)
  (icd:complete-code-h items *new-codes* *c* off-ht))

#;(with-open-file (s "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv")
	(loop for i = (read-line s nil) 
				while i do 
				(setf (gethash (icd:key i) code-ht) (#~s'.*(.\S)\s*'\1' i))))  ;some lines end with space, -- there may be only 1 char: "002.1 Paratifo A"



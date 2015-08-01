;;;; diagnosi.lisp
(in-package #:icd9dg)

;--------------
;MAIN FUNCTION
;--------------
(defun pdf-to-items ()
	(insert-path ;W7
		(tune-items  ;W6
			(complete-code ;W5
				(mark-comments ;W4
					(column-to-items   ;W3
						(pages-to-column     ;W2
							(icd:pdf-to-pages "Diagnosi.pdf" *chapters*)))))))) ;W1 

;------------
;WORKFLOW 1 pdf-to-pages
;------------
;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pages-to-column (lst)
  (stdutils:list-to-delimited-string (mapcar (stdutils:compose #'icd:split-page #'edit-single-page #'icd:pad-text) (c655.2 lst))))

(defun edit-single-page (page)
  "reduce spaces between key and text, to enable page-splitting"
    (icd:edit-single-page-helper page *tagged-entries*))

(defun c655.2 (lst) ;; 9.7.15 subst only this page
  "655.2 - im txt from pdf ist eine neue Zeile sowie ' strange char zwischen code und Beschreibung, malattia...che puo' colpire...."
  (setf (nth 221 lst) (#~s/"655.2[\\n\\s']+"/"655.2 "/ (nth 221 lst))) 
  lst)

;------------
;WORKFLOW 3 column-to-items
;------------
(defun column-to-items (stg)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (icd:uc-header (icd:optimize-text (icd:reduce-space stg))))))

(defun string-to-list (stg)
  (icd:split-into-items (icd:tag-items *tag-re* stg))) ;re for regular-expressions

(defun edit-column (stg)
  (icd:afts (icd:re-fns *column-edits* :m t) stg))

;------------
;WORKFLOW 4 mark-comments
;------------
(defun mark-comments (lst) (mapcar #'insert-bar lst))
(defun insert-bar (i) (or (icd:code-bar i code-ht *cbar2* *cbar3*) (icd:header-bar i *hbar2* *hbar3* *hbar4*)))
(defparameter code-ht (make-hash-table :test #'equal))
(icd:load-ht code-ht "~/Programming/Projects/IcdIt2007/icd9cm_24.csv")

;------------
;WORKFLOW 5 complete-code
;------------
(defun complete-code (items)
  (icd:complete-code-h items *new-codes* *c* code-ht))

;------------
;WORKFLOW 6 tune-items
;------------
(defun tune-items (lst)
  (mapcar (stdutils:compose #'icd:accented-chrs #'grklammer #'longcode1 #'longcode2) lst))

(defun grklammer (item)
  (icd:afts (icd:grklammer-fns *grkl*) item))

(defun longcode1 (item)
  (icd:afts (icd:longcode-fns *longcode*) item))

(defun longcode2 (item)
  (longcode-h item))

;1) remove the header-string, 2) then remove an eventually comma
;802.2 ..(osso).. macht metachar probleme
(let ((rmtxt "")) 
  (defun longcode-h (item)
    (cond ((or (l1k item) (l2k item)) (setf rmtxt (subseq item 6 (1- (length item)))) item)
          (t (#~s' , ' ' (#~s/(ppcre:quote-meta-chars rmtxt)/""/ item))))))

(defun l1k (item)
  (find (icd:key item) *l1keys* :test #'equal))

(defun l2k (item)
  "return true if 123.4 - false if 123 or 123.45"
  (let ((key (icd:key item)))
    (and (= 5 (length key))
         (find (subseq key 0 3) *l2keys* :test #'equal))))

;------------
;WORKFLOW 7 insert-path
;------------
; chapt 4 u 14 haben keine h2
(defun insert-path (lst) (icd:insert-h1 (insert-h2a (insert-h2 (icd:insert-key lst)))))

(defun insert-h2 (lst)
  (let ((h2 ""))
    (mapcar (lambda (x)
              (cond ((#~m'^\d{2}(?=\|\d{1,2}\. )' x) (setf h2 "") x)  ; h2 "" brauchts  für chapt 4, das keine h2 hat !!
                    ((pre:whenmatch (#~m'^([V\d]{6})(?=\|)' x) (setf h2 (#~s'$'.' $1))) x) ; point hier sonst wird "04..280.0|280.0 output
                    (t (lol:mkstr h2 x))))
            lst)))

(defun insert-h2a (lst)
  "chapt 5 hat überzähligen header, PSICOSI (290-299), 5.11.13 geht, ebenso chapt 17 fratture, ferite.
  h2a for lack of a better name, ev h2bis"
  (mapcar (lambda (x)
            (cond ((#~m'^290294|^295299' x) (lol:mkstr "290299." x)) ; psicosi
                  ((#~m'^800804|^805809|^810819|^820829' x) (lol:mkstr "800829." x))   ;fratture
                  ((#~m'^870879|^880887|^890897' x) (lol:mkstr "870897." x))   ;ferite
                  (t x)))
          lst))

;------------
;WORKFLOW 8 create-perlarry-file
;------------

;benchmark
(defun benchmark-diagnosi ()
;(time (o:p pdf-to-pages (icd9dg::pdf-to-pages "Diagnosi.pdf" icd9dg::*chapters* 20)))
(time (o:p pdf-to-pages (icd:pdf-to-pages "Diagnosi.pdf" icd9dg::*chapters* 20)))
(time (o:p pages-to-column (icd9dg::pages-to-column pdf-to-pages)))
(time (o:p column-to-items (icd9dg::column-to-items pages-to-column)))
(time (o:p mark-comments (icd9dg::mark-comments column-to-items)))
(time (o:p complete-code (icd9dg::complete-code mark-comments)))
(time (o:p tune-items (icd9dg::tune-items complete-code)))
(time (o:p insert-path (icd9dg::insert-path tune-items)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ad testfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END
;;;;;;;;;;;;
;;; test, 15.11.13
;;; ("V02.69" "574.71" "574.70" "574.41" "574.40" "574.31" "574.30" "Setticemia")   ;; <--------- 
(defun pages ()
  (icd:pdf-to-pages "Diagnosi.pdf" *chapters* 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,


(defun pdf-to-perlarray (&optional (chapter 20))
  (let ((strg (if (= 20 chapter) 
                (pdf-to-items)
                (pdf-to-items-t chapter))))
;    (create-perlarry-file strg "~/src/lisp/icd9it-pdf/Data/dataDiagnosi")))
    (icd:create-perlarry-file strg "~/src/lisp/icd9it-pdf/data/dataDiagnosi_9")))


(defun items ()
  (column-to-items 
    (pages-to-column 
      (icd:pdf-to-pages "Diagnosi.pdf" *chapters* 20))))

(defparameter items (stdutils:memoize #'items))

(defun pdf-to-items ()
  (insert-path ;W7
    (tune-items  ;W6
      (complete-code ;W5
        (mark-comments (funcall items))))))  ;W4

(defun pdf-to-items-t (chapter)
  "for single chapter testing, without complete code" 
  (insert-path ;W7
    (tune-items  ;W6
      (mark-comments  ;W4
        (column-to-items   ;W3
          (pages-to-column     ;W2
            (icd:pdf-to-pages "Diagnosi.pdf" *chapters* chapter))))))) ;W1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END

#;(defun code-bar (i)
	(lol:aif (gethash (icd:key i) code-ht)
					 (or (ignore-errors (icd::rm-nsb (#~s/(format nil "(?<=~a)$" (ppcre:quote-meta-chars lol:it))/"|"/im (#~s' $'' i))))
							 (cond ((member (icd:key i) *cbar3* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
										 ((member (icd:key i) *cbar2* :test #'string=) (icd::rm-nsb (#~s'(\n.+)\n'\1|' i)))
										 (t (#~s'\n'|' i))))))

#;(defun header-bar (i)
	(cond	((member (icd:key i) *hbar4* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+\n.+)\n'\1|' i)))  
				((member (icd:key i) *hbar3* :test #'string=) (icd::rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
				((member (icd:key i) *hbar2* :test #'string=) (icd::rm-nsb (#~s'(\n.+)\n'\1|' i)))
				(t (#~s'\n'|' i))))

;(defun insert-bar (i) (or (code-bar i) (header-bar i)))



;geht nicht, warum?
#;(defun insert-h2a (lst)
  "chapt 5 hat überzähligen header, PSICOSI (290-299), 5.11.13 geht, ebenso chapt 17 fratture, ferite.
  h2a for lack of a better name, ev h2bis"
  (mapcar (lambda (x)
            (or (#~s'^290294|^295299'290299.' x) ; psicosi
                (#~s'^800804|^805809|^810819|^820829'800829.' x)   ;fratture
                (#~s'^870879|^880887|^890897'870897.' x)   ;ferite
                x))
          lst))
;geht auch nicht
#;(defun insert-h2a (lst)
  "chapt 5 hat überzähligen header, PSICOSI (290-299), 5.11.13 geht, ebenso chapt 17 fratture, ferite.
  h2a for lack of a better name, ev h2bis"
  (mapcar (lambda (x)
            (or (#~s'(^290294|^295299)'290299.' x) ; psicosi
                (#~s'(^800804|^805809|^810819|^820829)'800829.' x)   ;fratture
                (#~s'(^870879|^880887|^890897)'870897.' x)   ;ferite
                x))
          lst))







#;(defun pages-to-column (lst)
  (stdutils:list-to-delimited-string (icd:aftl (#'icd:split-page #'edit-single-page #'icd:pad-text) (c655.2 lst))))

(defun tune-items (lst)
  (icd:aftl (#'icd:accented-chrs #'grklammer #'longcode1 #'longcode2) lst))
#;(defun insert-path (lst)
  (funcall (stdutils:compose #'icd:insert-h1 #'insert-h2a #'insert-h2 #'icd:insert-key) lst))



;interventi.lisp
(in-package #:icd9th)

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
							(icd:pdf-to-pages "Interventi.pdf" *chapters*)))))))) ;W1 

;------------
;WORKFLOW 1 pdf-to-pages
;------------
;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pages-to-column (lst)
   (stdutils:list-to-delimited-string (mapcar (stdutils:compose #'icd:split-page #'edit-single-page #'edit-single-page2 #'icd:pad-text) lst)))

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
(defun insert-bar (i) (or (icd:code-bar i code-ht *cbar2* *cbar3*) (icd:header-bar i *hbar2* *hbar3* *hbar4*)))
(defparameter code-ht (make-hash-table :test #'equal))
(icd:load-ht code-ht "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv")

;------------
;WORKFLOW 5 complete-entries
;------------
(defun complete-code (items)
  (icd:complete-code-h items *new-codes* *c* code-ht))

;------------
;WORKFLOW 6 tune items
;------------
(defun tune-items (lst)
  (mapcar (stdutils:compose #'longcode #'grklammer #'icd:accented-chrs) lst))

(defun grklammer (item)
  (icd:afts (icd:grklammer-fns *grkl*) item))

(defun longcode (item)
  (icd:afts (icd:longcode-fns *longcode*) item))

;------------
;;WORKFLOW 7 insert-path
;------------
(defun insert-path (lst) (icd:insert-h1 (icd:insert-key lst)))

;------------
;WORKFLOW 8 create-perlarry-file
;------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END

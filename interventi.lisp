;interventi.lisp
(in-package #:icd9th)

;;; for tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pages ()
  (pdf-to-pages "Interventi.pdf" *chapters* 20))
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
    (create-perlarry-file strg "~/src/lisp/icd9it-pdf/Data/dataInterventi")))

(defun pdf-to-items (&optional (chapter 20))
  (insert-path ;W7
    (tune-items  ;W6
      (complete-code ;W5
        (mark-comments  ;W4
          (column-to-items   ;W3
            (pages-to-column     ;W2
              (pdf-to-pages "Interventi.pdf" *chapters* chapter)))))))) ;W1

;without complete-code 
(defun pdf-to-items-t (&optional (chapter 20))
  (insert-path ;W7
    (tune-items  ;W6
      (mark-comments  ;W4
        (column-to-items   ;W3
          (pages-to-column     ;W2
            (pdf-to-pages "Interventi.pdf" *chapters* chapter))))))) ;W1

;------------
;WORKFLOW 1 pdf-to-pages
;------------
;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pages-to-column (lst)
   (o:lststg (o:aftl (#'split-page #'edit-single-page #'edit-single-page2 #'pad-text) lst)))

(defun edit-single-page (page)
  (edit-single-page-helper page *tagged-entries*))

;zusätzliche edits
(defun edit-single-page2 (page)
  "reduce spaces between key and text, to enable page-splitting"
  (funcall (o:compose #'c85) page))

(defun c85 (strg)
  (ppcre:regex-replace "985 (Interventi sulla mammella)" strg "85 \\1"))

;------------
;WORKFLOW 3 column-to-items
;------------
(defun column-to-items (str)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (uc-header (optimize-text str)))))

(defun string-to-list (strg)
  (split-into-items (tag-items *tag-re* strg))) ;re for regular-expressions

(defun edit-column (strg)
  (o:afts (o:re-fns *column-edits*) strg))

;------------
;WORKFLOW 4 mark comments
;------------
(defun mark-comments (lst)
  (mapcar #'insert-bar lst))

(defun insert-bar (item)
  (or (offbar item)
      (manbar item)
      (defmanbar (connect-first2lines-if- item))))

(defun offbar (item)
  (bar-h item off-ht #'manbar))

(defun manbar (item)
  (bar-h item man-ht #'defmanbar))

(o:p off-ht (make-hash-table :test #'equal) "official-code-ht")
(o:string-l (o:afts (o:re-fns *edit-official-code*) (o:file "~/Programming/Projects/IcdIt2007/icd9cm_24Interventi.csv"))
  (setf (gethash (o:key o:it) off-ht) o:it))

(o:p man-ht (make-hash-table :test #'equal) "manual-bar-ht")
(load-ht man-ht *man-ht*)

;------------
;WORKFLOW 5 complete-entries
;------------
(defun complete-code (items)
  (complete-code-h items *new-codes* *c* off-ht))

;------------
;WORKFLOW 6 tune items
;------------
(defun tune-items (lst)
(o:aftl (#'longcode #'grklammer #'accented-chrs) lst))

(defun grklammer (item)
    (o:afts (grklammer-fns *grkl*) item))

(defun longcode (item)
    (o:afts (longcode-fns *longcode*) item))

;------------
;;WORKFLOW 7 insert-path
;------------
(defun insert-path (lst)
  (funcall (o:compose #'insert-h1 #'insert-key) lst))

;------------
;WORKFLOW 8 create-perlarry-file
;------------

;;; END ;;;;;;;;;;;;;;;


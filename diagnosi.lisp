;;;; diagnosi.lisp
(in-package #:icd9dg)

;;;;;;;;;;;;
;;; test, 15.11.13
;;; ("V02.69" "574.71" "574.70" "574.41" "574.40" "574.31" "574.30" "Setticemia")   ;; <--------- 
(defun pages ()
  (pdf-to-pages "Diagnosi.pdf" *chapters* 20))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;--------------
;MAIN FUNCTION
;--------------
(defun pdf-to-perlarray (&optional (chapter 20))
  (let ((strg (if (= 20 chapter) 
                (pdf-to-items)
                (pdf-to-items-t chapter))))
    (create-perlarry-file strg "~/src/lisp/icd9it-pdf/Data/dataDiagnosi")))

(defun items ()
  (column-to-items 
    (pages-to-column 
      (pdf-to-pages "Diagnosi.pdf" *chapters* 20))))

(o:p items (o:memoize #'items))

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
            (pdf-to-pages "Diagnosi.pdf" *chapters* chapter))))))) ;W1

;------------
;WORKFLOW 1 pdf-to-pages
;------------
;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pages-to-column (lst)
  (o:lststg (o:aftl (#'split-page #'edit-single-page #'edit-single-page2 #'pad-text) lst)))

(defun edit-single-page (page)
  "reduce spaces between key and text, to enable page-splitting"
    (edit-single-page-helper page *tagged-entries*))

(defun edit-single-page2 (page)
  "zusätzliche edits"
  (funcall (o:compose #'c655.2) page))

(defun c655.2 (page)
  "655.2 - im txt from pdf ist eine neue Zeile sowie ' strange char zwischen code und Beschreibung"
  (ppcre:regex-replace "655.2[\\n\\s']+(Malattia eredit)" page "655.2 \\1"))

;------------
;WORKFLOW 3 column-to-items
;------------
(defun column-to-items (str)
  "convert a single-column-string to a list of items"
  (string-to-list (edit-column (uc-header (optimize-text str)))))

(defun string-to-list (strg)
  (split-into-items (tag-items *tag-re* strg))) ;re for regular-expressions

(defun edit-column (strg)
  (o:afts (o:re-fns *column-edits* :m t) strg))

;------------
;WORKFLOW 4 mark-comments
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
(o:string-l (o:afts (o:re-fns *edit-official-code*) (o:file "~/Programming/Projects/IcdIt2007/icd9cm_24.csv"))
  (setf (gethash (o:key o:it) off-ht) o:it))

(o:p man-ht (make-hash-table :test #'equal) "manual-bar-ht")
(load-ht man-ht *man-ht*)

;------------
;WORKFLOW 5 complete-code
;------------
(defun complete-code (items)
  (complete-code-h items *new-codes* *c* off-ht))

;------------
;WORKFLOW 6 tune-items
;------------
(defun tune-items (lst)
  (o:aftl (#'accented-chrs #'grklammer #'longcode1 #'longcode2) lst))

(defun grklammer (item)
  (o:afts (grklammer-fns *grkl*) item))

(defun longcode1 (item)
  (o:afts (longcode-fns *longcode*) item))

(defun longcode2 (item)
  (longcode-h item))

;1) remove the header-string, 2) then remove an eventually comma
;802.2 ..(osso).. macht metachar probleme
(let ((rmtxt "")) 
  (defun longcode-h (item)
    (cond ((or (l1k item) (l2k item)) (setf rmtxt (subseq item 6 (1- (length item)))) item)
          (t (ppcre:regex-replace " , " (ppcre:regex-replace (ppcre:quote-meta-chars rmtxt) item "") " ")))))

(defun l1k (item)
  (find (o:key item) *l1keys* :test #'equal))

(defun l2k (item)
  "return true if 123.4 - false if 123 or 123.45"
  (let ((key (o:key item)))
    (and (= 5 (length key))
         (find (subseq key 0 3) *l2keys* :test #'equal))))

;------------
;WORKFLOW 7 insert-path
;------------
; chapt 4 u 14 haben keine h2
(defun insert-path (lst)
  (funcall (o:compose #'insert-h1 #'insert-h2a #'insert-h2 #'insert-key) lst))

(defun insert-h2 (lst)
  (let ((h2 ""))
    (mapcar (lambda (x)
              (o:acond ((ppcre:scan "^\\d{2}(?=\\|\\d{1,2}\\. )" x) (setf h2 "") x)  ; h2 "" brauchts  für chapt 4, das keine h2 hat !!
                       ((ppcre:scan-to-strings "^[V\\d]{6}(?=\\|)" x) (setf h2 (format nil "~a." o:it)) x) ; point hier sonst wird "04..280.0|280.0 output
                       (t (o:stg h2 x))))
            lst)))

(defun insert-h2a (lst)
  "chapt 5 hat überzähligen header, PSICOSI (290-299), 5.11.13 geht, ebenso chapt 17 fratture, ferite.
  h2a for lack of a better name, ev h2bis"
  (mapcar (lambda (x)
            (cond ((ppcre:scan "^290294|^295299" x) (o:stg "290299." x)) ; psicosi
                  ((ppcre:scan "^800804|^805809|^810819|^820829" x) (o:stg "800829." x))   ;fratture
                  ((ppcre:scan "^870879|^880887|^890897" x) (o:stg "870897." x))   ;ferite
                  (t x)))
          lst))

;------------
;WORKFLOW 8 create-perlarry-file
;------------

@END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

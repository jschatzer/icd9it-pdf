(in-package #:icd)

;------------
; UTILITIES, ev einige ad onlips überlegen
;------------
;test 24.10.13
(lol:defmacro! string-l (filename &body body)
	"iterate over the lines in a string"
	`(with-input-from-string (,g!str ,filename)
		 (stdutils:awhile2 (read-line2 ,g!str)	,@body)))

;helper 
(let ((g (gensym)))
	(defun read-line2 (&optional (str *standard-input*))
		(let ((val (read-line str nil g)))
			(unless (equal val g) (values val t)))))

; 10.11.13
; from icd9
;(defun afts (strg fns)
(defun afts (fns strg)
  "for apply functions to scalar"
    (eval `(funcall (stdutils:compose ,@fns) ,strg)))

;usage:  (icd::aftl '(1 2 3) (#'1+ #'1+))
;(defmacro aftl (lst fns)
;analog: mapcar fn lst
(defmacro aftl (fns lst)
  "for apply functions to list"
  `(mapcar (stdutils:compose ,@fns) ,lst))

(defun re-fns (alist &key m)
  "return a list of string-replace-functions, i.e reg expr functions,
  they want a string as input
  geht mit multiline und singlelinemode, do :m t"
  (mapcar (lambda (alist-elt)
            (lambda (strg)
              (if m
                (ppcre:regex-replace-all (ppcre:create-scanner (car alist-elt) :multi-line-mode t :single-line-mode t) strg (cdr alist-elt) :simple-calls t)
                (ppcre:regex-replace-all (car alist-elt) strg (cdr alist-elt) :simple-calls t))))
          alist))

(defun insert-after-pos (lst index newelt)
  "insert after position, before-position can be done with (1- pos)
  from http://stackoverflow.com/questions/4387570/in-common-lisp-how-can-i-insert-an-element-into-a-list-in-place"
  (push newelt (cdr (nthcdr index lst)))
  lst)

#|
;geht anscheinend wegen #'key nicht ??, warum??
(defun key (s)
  "return the key of an entry" ;(key "123 CodeText") ; "123"
  (#~s'\s.*'' s))
|#

(defun key (line)
	  "return the key of an entry" ;(key "123 CodeText") ; "123"
	  (subseq line 0 (position #\space line)))

(defun key-from-bar-all (line)
  "return the key from 1.bar to space, >with< points, parenthesis and slash"
  (subseq line (1+ (position #\| line)) (position #\space line)))

; ev use o:re-fns, ev function als arg überlegen <--- 
(defun s-fns (alist) 
  "return a list of substitute-functions"
  (mapcar (lambda (le) ; le for list element
            (lambda (strg)
              (substitute (car le) (cdr le) strg)))
          alist))

(defun rm-nsb (entry)
  "remove linebreaks (newlines) and excessive space before the barline"
  (let ((pos (position #\| entry)))
    (concatenate 'string
                 (ppcre:regex-replace-all "\\s+" (rm-linebreaks (subseq entry 0 pos)) " ")
                 (subseq entry pos))))

(defun rm-linebreaks (strg)
  (afts (re-fns *rlb*) strg))

(defun edit-entries-space (lst)
  (mapcar #'edit-entry-X lst))

(defun edit-entry-X (strg)
  "33   Altri interventi - there are often 3-4 spaces between key and text"
  (ppcre:regex-replace  "^([V\\d\\.]+)\\s{2,}" strg "\\1 "))

;------------
;WORKFLOW 1 pdf-to-pages
;------------
(defun pdf-to-pages (pdf lst ch)
 "convert a single or all chapters (use 20) to a list of pages"
  (let ((range (eval `(case ,ch ,@lst))))
    (funcall (stdutils:compose #'split-into-pages #'trim-text)
             (pdf-to-txt pdf (car range) (cdr range)))))

(defun pdf-to-txt (pdf-file from to)
  "create text from Pdf from-page to-page"
  (clesh:script
    ;(let ((path "~/Programming/Projects/IcdIt2007"))
    (let ((path "~/src/lisp/icd9it-pdf/pdf"))
      (format nil "pdftotext -layout -eol unix -f ~a -l ~a ~a/~a -" from to path pdf-file))))

(defun trim-text (strg)
  "remove first and last line of string, with sed and head, da es mit regex nicht gelingt"
  (alexandria:write-string-into-file strg "/tmp/pdf0" :if-exists :supersede :if-does-not-exist :create)
  (alexandria:write-string-into-file (rm-first-and-last-line-from-file "/tmp/pdf0") "/tmp/pdf1" :if-exists :supersede :if-does-not-exist :create))

;helper
(defun rm-first-and-last-line-from-file (file)
  (clesh:script (format nil "sed '1d' ~a | head -n -1" file)))

(defun split-into-pages (text)
  (ppcre:split (ppcre:create-scanner "^\\014.+?$" :multi-line-mode t) text))

;------------
;WORKFLOW 2 pages-to-column
;------------
(defun pad-text (txt)
  "pad text to enable correct page splitting"
  (with-output-to-string (out)
    (do ((in (make-string-input-stream txt)))
      ((not (listen in)))
      (format out "~90,,0a~%" (read-line in)))))

(defun edit-single-page-helper (page reg-lst)
  (dolist (regex reg-lst page)
    (destructuring-bind (code text) (ppcre:split "\\s+" regex :limit 2)
             (setf page (ppcre:regex-replace (format nil "(~a)\\s{3,}(~a)" code text) page  "\\1 \\2")))))

(defun split-page (page &optional (width 75) (w 2))
  "split a 2 column page"
  (do ((s (make-string-input-stream page))
       (col1 "") (ss "") (col2 ""))
    ((not (listen s)) (if (string= ss "") (return-from split-page (format nil "~a~%~a~%" col1 col2))))  ; "A~aA2A*" uppercase A removes indents
    (multiple-value-bind (c1 e c2) (cl-pack:unpack (format nil "a~aA~aa*" width w) (read-line s))
              (setf col1 (lol:mkstr col1 #\newline c1)
                    col2 (lol:mkstr col2 #\newline c2)
                    ss (concatenate 'string ss e))))
  (cond ((zerop width) (format t "Page Split not possible~%~a" page))
        (t (split-page page (1- width)))))

(defun optimize-text (strg)
  (afts (re-fns *rm-ts-el* :m t) (afts (re-fns *opt-txt*) strg)))

(defun uc-header (strg)
  "remove linebreaks from upper case header"
  (let ((regex1 "\\s*\\n\\s*(?=\\([\\dV]?\\d\\d-[\\dV]?\\d\\d\\))|\\s*\\n\\s*(?=\\(00\\))") ;alternative is only for chapt 0 interventi
        (lookbehind "(?<!ECHO)(?<!NIA)(?<! [ABC])(?<![a-z]')(?<! DNA)(?<!IV)(?<!II)(?<!- I)(?<!SAI)(?<=[A-Z,'])")
        (regex2 "\\s*-?\\n\\s*(?=[A-Z,'])"))
    (ppcre:regex-replace "CON NETTIVO"
      (ppcre:regex-replace-all (ppcre:create-scanner (format nil "~a~a|~a~a" lookbehind regex1 lookbehind regex2) :multi-line-mode t) strg " ")
      "CONNETTIVO")))

(defun tag-items (lst strg)
  "return a tagged string"
  (flet ((fns ()
           (mapcar (lambda (x)
                     (lambda (s)
                       (if (eql :h2 (car x)) ;in case of h2 also invert text and key
                         (ppcre:regex-replace-all (ppcre:create-scanner (cadr x) :multi-line-mode t) s "§\\2 \\1")
                         (ppcre:regex-replace-all (ppcre:create-scanner (cadr x) :multi-line-mode t) s "§\\1"))))
                   lst)))
    (eval `(funcall (stdutils:compose ,@(fns)) ,strg))))

(defun split-into-items (strg)
  "split removing the trailing newline and reducing excessive space between key and text, e.g. 40.11"
  (edit-entries-space
    (cdr (ppcre:split "\\n?§" strg)))) ;(ppcre:split "§" "§a §b) ; ("" "a " "b ")

;------------
;WORKFLOW 4 mark comments
;------------
(defun bar-h (item ht fn)
  "official hash-controlled bar insertion"
  (if (gethash (key item) ht)
    (lol:aif (ctrbar item (accs-to-chrs (gethash (key item) ht)))
           lol:it
      (funcall fn item))))

(defun accs-to-chrs (strg)
 (eval `(funcall (stdutils:compose ,@(s-fns *ac*)) ,strg)))

(defun ctrbar (strg ctrl)
  "returns a string with a bar or nil, if there is no bar in the string"
  (flet ((eoltest (line)
           (if (< (length ctrl) (length (subseq line (position-if #'stdutils:constituent line))))
             line
             (if (string-equal
                  (subseq line (position-if #'stdutils:constituent line))
                  (subseq ctrl (- (length ctrl) (length (subseq line (position-if #'stdutils:constituent line))))))
               (lol:mkstr line #\|)
               line))))
    (let ((item (format nil "~{~&~a~}" (mapcar #'eoltest (ppcre:split "\\n" strg)))))
      (if (find #\| item) (rm-nsb item)))))

(defun connect-first2lines-if- (str)
  (ppcre:regex-replace "(?<!\\d)-\\n\\s*" str ""))

(defun defoffbar (item)
    "default code bar, on end of item"
      (rm-nsb (lol:mkstr item #\|)))

(defun defmanbar (item)
  "default header bar, on end of 1. line"
  (ppcre:regex-replace "\\n" item "|"))

(defun load-ht (ht lst)
  (mapc (lambda (i)
          (setf (gethash (key i) ht) i))
        lst))

;------------
;WORKFLOW 5 complete-items
;------------
(defun complete-code-h (items nc ce ht)
  "destructively insert new codes"
  (dolist (cod nc items)
    (let ((pos (position cod items :test #'equal :key #'key)))
      (dolist (n (reverse ce))
        (setf items (insert-after-pos items pos (defoffbar (gethash (lol:mkstr cod n) ht))))))))

;------------
;WORKFLOW 6 tune items
;------------
(defun accented-chrs (item)
  (afts (re-fns *acc-chrs*) item)) 

(defun grklammer-fns (alist) 
  (mapcar (lambda (ai) ; alist-item
            (lambda (item)
              (if (string= (car ai) (key item)) 
                (ppcre:regex-replace (ppcre:create-scanner "(?<=\\|).*" :single-line-mode t) item (cdr ai))
                item)))
          alist))

(defun longcode-fns (alist) 
  (mapcar (lambda (ai) ; alist-item
            (lambda (strg)
              (if (ppcre:scan (format nil "^~a\\d" (car ai)) strg)
                (ppcre:regex-replace (cdr ai) strg "")
                strg)))
          alist))

;------------
;WORKFLOW 7 insert-path
;------------
(defun insert-key (lst)
  (mapcar (lambda (x)
            (lol:mkstr (k2n (key x)) #\| x))
          lst))

(defun k2n (k)
  "key to node[s]"
  (afts (re-fns *re-ikey*) k))

;damit point idem wie h2, in h2 notwendig
(defun insert-h1 (lst)
  (let ((h1 ""))
    (mapcar (lambda (x)
              ; match 01|1. text  - i.e. 2 digits, bar, 1-2 digits, period and space, (h2i does match 01| too)
              (fare-utils:acond ((ppcre:scan-to-strings "^\\d{2}(?=\\|\\d{1,2}\\. )" x) (setf h1 (format nil "~a." fare-utils:it)) x)  
                       (t (lol:mkstr h1 x))))
            lst)))

;------------
;WORKFLOW 8 create-perlarry-file
;------------
(defun create-perlarry-file (lst file)
  (with-open-file (strm file :direction :output :if-does-not-exist :create :if-exists :supersede)
  (format strm "use utf8;~%@icd = (~%~{~s,~%~})" lst)))  ; damit gehen accented chars

;damit geht utf8, accented chars
(defun create-perlarry-file-all (diagnosi interventi file)
  (let ((dg (stdutils:mklist "d|diagnosi|"))
        (diag (mapcar (lambda (x) (lol:mkstr "d." x)) diagnosi))
        (th (stdutils:mklist "i|interventi|"))
        (ther (mapcar (lambda (x) (lol:mkstr "i." x)) interventi)))
    (with-open-file (strm file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (format strm "use utf8;~%@icd = (~%~{~s,~%~})" (append dg diag th ther)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
@END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;  drafts ;;;;;;;;;;;;;;;;;;;
#;#;(defun trim-text (strg)
  "remove first and last line of string, with sed and head, da es mit regex nicht gelingt"
  (o:file-write "/tmp/pdf0" strg)
  (o:file-write "/tmp/pdf1" (rm-first-and-last-line-from-file "/tmp/pdf0")))



;damit point idem wie h2, in h2 notwendig
#;(defun insert-h1 (lst)
  (let ((h1 ""))
    (mapcar (lambda (x)
              ; match 01|1. text  - i.e. 2 digits, bar, 1-2 digits, period and space, (h2i does match 01| too)
              (stdutils:acond ((ppcre:scan-to-strings "^\\d{2}(?=\\|\\d{1,2}\\. )" x) (setf h1 (format nil "~a." stdutils:it)) x)
                       (t (lol:mkstr h1 x))))
            lst)))

#;(defun trim-text (strg)
  "remove first and last line of string, with sed and head, da es mit regex nicht gelingt"
  (o:file-write "/tmp/pdf0" strg)
  (o:file-write "/tmp/pdf1" (rm-first-and-last-line-from-file "/tmp/pdf0")))



#;(defun trim-text (strg)
  "remove first and last line of string, with sed and head, da es mit regex nicht gelingt"
  (alexandria:write-string-into-file strg "/tmp/pdf0" :if-does-not-exist :create :if-exists :overwrite)
  (alexandria:write-string-into-file (rm-first-and-last-line-from-file "/tmp/pdf0") "/tmp/pdf1" :if-does-not-exist :create :if-exists :overwrite))

#|
;- - - - -
;(defmacro! do-file-l (filename &body body)
#;(defmacro! file-l (filename &body body)
	"iterate over the lines in a file"
	`(with-open-file (,g!str ,filename)
		 (awhile2 (read-line2 ,g!str)	,@body)))
|#


|#

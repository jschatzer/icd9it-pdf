(in-package #:icd)
; i item, 

;------------
; UTILITIES, ev einige ad onlips überlegen
;------------
;test 24.10.13
(lol:defmacro! string-l (stg &body body)
	"iterate over the lines in a string"
	`(with-input-from-string (,g!str ,stg)
		 (stdutils:awhile2 (read-line2 ,g!str)	,@body)))

;helper 
(let ((g (gensym)))
	(defun read-line2 (&optional (str *standard-input*))
		(let ((val (read-line str nil g)))
			(unless (equal val g) (values val t)))))

;geht nicht -- wieso??

; beide idem, gehen nicht richtig
;(defmacro afts (fns stg) `(funcall (stdutils:compose ,@fns) ,stg))
;(defmacro afts (fns stg) `(funcall ,(cons 'stdutils:compose fns) ,stg))

;diese gehen
;orig
(defun afts (fns stg) "for apply functions to scalar" (eval `(funcall (stdutils:compose ,@fns) ,stg)))
;(defmacro afts (fns stg) `(funcall ,(cons 'stdutils:compose (eval fns)) ,stg))
;(defmacro afts (fns stg) `(let ((fs ,fns)) `(funcall (stdutils:compose ,@fs) ,,stg)))
;(defmacro afts (fns stg) ``(let ((fs ,,fns)) (funcall (stdutils:compose ,@fs) ,,stg)))
;(defmacro afts (fns stg) `(let ((fs (eval ,fns))) `(funcall (stdutils:compose ,@fs) ,,stg)))


(defun key (s) "return the key of an entry" (#~s'\s.*''s s)) ;(key "123 CodeText") ; "123"
 
(defun re-fns (alist &key m)
	"return a list of string-replace-functions, i.e reg expr functions,
	they want a string as input
	geht mit multiline und singlelinemode, do :m t"
	(mapcar (lambda (alist-elt)
						(lambda (strg)
							(if m
								(#~s/(car alist-elt)/(cdr alist-elt)/gems strg)
								(#~s/(car alist-elt)/(cdr alist-elt)/ge strg))))
					alist))

; ev use o:re-fns, ev function als arg überlegen <--- 
(defun s-fns (alist) 
  "return a list of substitute-functions"
  (mapcar (lambda (le) ; le for list element
            (lambda (strg)
              (substitute (car le) (cdr le) strg)))
          alist))

(defun insert-after-pos (lst index newelt)
  "insert after position, before-position can be done with (1- pos)
  from http://stackoverflow.com/questions/4387570/in-common-lisp-how-can-i-insert-an-element-into-a-list-in-place"
  (push newelt (cdr (nthcdr index lst)))
  lst)

(defun rm-nsb (entry)
  "remove linebreaks (newlines) and excessive space before the barline"
  (let ((pos (position #\| entry)))
    (concatenate 'string
;                 (ppcre:regex-replace-all "\\s+" (rm-linebreaks (subseq entry 0 pos)) " ")
                 (#~s'\s+' 'g (rm-linebreaks (subseq entry 0 pos)))
                 (subseq entry pos))))

(defun rm-linebreaks (strg) (afts (re-fns *rlb*) strg))
(defun edit-entries-space (lst) (mapcar #'reduce-space lst))
;(defun reduce-space (i) "33   Altri interventi - there are often 3-4 spaces between key and text" (#~s'\s+' ' i))  ; ev use this in completet chords


;from  
;http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/c7610a7f89419c0b/5ddd667e717b4b99#5ddd667e717b4b99  -- Michael Kappert
(defun make-tree (f &optional (node-key ""))
  "make tree from txt-file, cll"  ; insert url
	(loop
		for line = (peek f) for line-key = (key-bar line)
		while (prefix-p node-key line-key)
		collect (cons (consume f) (make-tree f line-key))))
(let ((line nil))
	(defun peek (f) (or line (setf line (read f nil nil))))
	(defun consume (f) (prog1 (peek f) (setf line nil))))
(defun key-bar (line) (subseq line 0 (position #\| line)))
(defun prefix-p (k1 k2) (eql (mismatch k1 k2) (length k1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; for tests only
(defun count-bars (s)
  (stdutils:count-string-char-if (lambda (x) (char= #\| x)) s))

(defun bars-in-entries (file)  ; lisptree-file
  (mapcar 'count-bars (o:flatten (with-open-file (f file) (read f)))))  ;~/src/lisp/icd9it-pdf/data/dataDgTh9"))

;------------
;WORKFLOW 1 pdf-to-pages
;------------
(defun pdf-to-pages (pdf lst &optional (ch 20))
 "convert a single or all chapters (use 20) to a list of pages"
  (let ((range (eval `(case ,ch ,@lst))))
    (funcall (stdutils:compose #'split-into-pages #'trim-text) (pdf-to-txt pdf (car range) (cdr range)))))

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
(defun rm-first-and-last-line-from-file (file) (clesh:script (format nil "sed '1d' ~a | head -n -1" file)))
(defun split-into-pages (text) (ppcre:split (ppcre:create-scanner "^\\014.+?$" :multi-line-mode t) text))

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
      (setf page (#~s/(format nil "(~a)\\s{3,}(~a)" code text)/"\\1 \\2"/ page)))))

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
    (#~s'CON NETTIVO'CONNETTIVO' (#~s/(format nil "~a~a|~a~a" lookbehind regex1 lookbehind regex2)/" "/gm strg))))

(defun tag-items (lst strg)
  "return a tagged string"
  (flet ((fns ()
           (mapcar (lambda (x)
                     (lambda (s)
                       (if (eql :h2 (car x)) ;in case of h2 also invert text and key
                         (#~s/(cadr x)/"§\\2 \\1"/gm s)
                         (#~s/(cadr x)/"§\\1"/gm s))))
                   lst)))
    (eval `(funcall (stdutils:compose ,@(fns)) ,strg))))

(defun split-into-items (strg)
  "split removing the trailing newline and reducing excessive space between key and text, e.g. 40.11"
  (edit-entries-space
    (cdr (ppcre:split "\\n?§" strg)))) ;(ppcre:split "§" "§a §b) ; ("" "a " "b ")

;------------
;WORKFLOW 3 column-to-items
;------------
(defun reduce-space (i) 
  "remove ms-word spaces, excluding indented lines"
  (with-output-to-string (l)
    (string-l i
     (if (not (#~m'^\s+' stdutils:it))
       (format l "~a~%" (#~s' +' 'g stdutils:it))
       (format l "~a~%" stdutils:it)))))

;scheint gleich gut zu gehen
#;(defun reduce-space-first-line (i) 
  (#~s'\s+(?=.*\n)' ' i))

;------------
;WORKFLOW 4 mark comments
;------------
(defun load-ht (h f) ;file
	(with-open-file (s f)
		(loop for i = (read-line s nil) 
					while i do 
					(setf (gethash (key i) h) (#~s'.*(.\S)\s*'\1' i)))))  ;some lines end with space, -- there may be only 1 char: "002.1 Paratifo A"

(defun code-bar (i h b2 b3)
	(lol:aif (gethash (key i) h)
					 (or (ignore-errors (rm-nsb (#~s/(format nil "(?<=~a)$" (ppcre:quote-meta-chars lol:it))/"|"/im (#~s' $'' i))))
							 (cond ((member (key i) b3 :test #'string=) (rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
										 ((member (key i) b2 :test #'string=) (rm-nsb (#~s'(\n.+)\n'\1|' i)))
										 (t (#~s'\n'|' i))))))

(defun header-bar (i b2 b3 b4)
	(cond	((member (key i) b4 :test #'string=) (rm-nsb (#~s'(\n.+\n.+\n.+)\n'\1|' i)))  
				((member (key i) b3 :test #'string=) (rm-nsb (#~s'(\n.+\n.+)\n'\1|' i)))
				((member (key i) b2 :test #'string=) (rm-nsb (#~s'(\n.+)\n'\1|' i)))
				(t (#~s'\n'|' i))))

;------------
;WORKFLOW 5 complete-items
;------------
(defun defoffbar (i) "default code bar, on end of item" (rm-nsb (#~s'$'|' i))) ; 534.91 ULCERA GASTRODIGIUNALE NON  SPECIFICATA  <--- das ist der einzige space für das rm-nsb hier gebraucht wird
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

;;;;;;;; das geht auch ;;;;;;;;;;;;;;;;;;;;;;
#;(defun accented-chrs (i) (re% *acc-chrs* i))
; das geht
#;(defun re% (al i)
	(dolist (x al i)
		(setf i (#~s/(car x)/(cdr x)/g i))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grklammer-fns (alist) 
  (mapcar (lambda (ai) ; alist-item
            (lambda (item)
              (if (string= (car ai) (key item)) 
                (#~s/"(?<=\\|).*"/(cdr ai)/s item)
                item)))
          alist))

(defun longcode-fns (alist) 
  (mapcar (lambda (ai) ; alist-item
            (lambda (strg)
              (if (#~m/(format nil "^~a\\d" (car ai))/ strg)
                (#~s/(cdr ai)/""/ strg)
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
              (pre:ifmatch (#~m'^(\d\d)\|\d{1,2}\. ' x)
                (progn (setf h1 (format nil "~a." $1)) x)
                (lol:mkstr h1 x)))
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

;------------
;WORKFLOW 9 create-lisptree-file
;------------
; STDUTILS:ON-TREES <-----
; run (icd::create-lisptree-file "~/src/lisp/icd9it-pdf/data/dataDgTh9" "icdtreetest-without-path")
(defun create-lisptree-file (infile outfile)
  "edit a copy of the perl array file"
  (clesh:script (format nil "sed '1,2d; $d; s/,$//' ~a > temp1" infile))
  (with-open-file (o outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format o "~s" 
            (funcall (o:ttrav #'cons (lambda (stg) (if (stringp stg) (#~s'[^|]+\|'' stg) stg)))
                     (with-open-file (i "temp1") (make-tree i))))))

(defun create-lisptree-file (infile outfile)
  "edit a copy of the perl array file"
  (clesh:script (format nil "sed '1,2d; $d; s/,$//' ~a > temp1" infile))
  (with-open-file (o outfile :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format o "~s" 

            (funcall (o:ttrav #'cons (lambda (stg) (if (stringp stg) (#~s'[^|]+\|'' stg) stg)))
                  (list (cons "|icd|"   (with-open-file (i "temp1") (make-tree i))))))))
;                  (list (cons "|dummy"   (with-open-file (i "temp1") (make-tree i))))))))


;    (list "icd|"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@END

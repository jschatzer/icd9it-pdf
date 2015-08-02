;;; initicd.lisp

(in-package #:icd)
;------------
; UTILITIES
;------------
(defparameter *rlb* 
     '(("(?<!\\d)-\\n\\s*" . "")
       ("(?<!-)\\n\\s*" . " ")))

;------------
;W 3  column-to-items
;------------
; reverse damit die Reihenfolge der edits stimmt!
(defparameter *opt-txt* 
     (reverse
       '(("'" . "") ; FIRST remove all single quotes, werden nur für accented char gebraucht
         ;--------------------------------------------------------
         ;     15.3                          '
         ;            Interventi su due o piu muscoli extraoculari
         ;--------------------------------------------------------
         ("(?<=\\d)\\s+'\\s*\\n\\s*(?=\\w)" . " ")  ; SECOND remove all single quotes, followed by space and nl
         ("’’|‘‘|’" . "'") ; THIRD after removing all single quotes, replace strange quotes with single quote, they cause problems in Tk output
         ("INDICE ALFABETICO|ELENCO SISTEMATICO|(?<!0. )PROCEDURE|DIAGNOSI" . "")  ; 0. PROCEDURE ED INTERVENTI  in chapt 0 h1, SENZA INDICAZIONE DELLA DIAGNOSI (V70-V82)
         ("¤|°|Ø|«" . "") ;  "second replace strange quotes with single quote"
         ("\\|\\s*'" . "ì")
         ("\\^" . "-"))))

(defparameter *rm-ts-el* '(("\\s*$" . "") ("^\\s*\\n" . "")) "remove trailing space and emtpy lines")

;------------
;W 4 mark-comments
;------------
(defparameter *ac* '((#\a . #\À) (#\o . #\Ò) (#\i . #\Ì) (#\u . #\Ù) (#\e . #\È) (#\e . #\É)))

;------------
;W 6 tune-items
;------------
(defparameter *acc-chrs* '(
  ;1) ¤
  ;">perche< >e< conosciuto il punto di origine, >cioe< la punta" in comments tumori
  ;("è  wie ??")  ; ev subst single found locations  <---
  ("cioe" . "cioè")
  ("se stessi" . "sé stessi") ; V62
  ("ne trattamento" . "né trattamento")  ;V68.81
;  ("perche" . "perché")
  ("(?<!\w)perche" . "perché")    ; Iperchératosi
  ("purche" . "purché") ;  391
  ("nonche" . "nonché") ; 16.
  ("Sezary" . "Sézary") ;   ;d 202.24
  ("Bouffee" . "Bouffée") ;                    298.3
  ("Dejerine" . "Déjérine") ;  356.0
  ("Guillain-Barre" . "Guillain-Barré")
  ("Iride bombe" . "Iride bombé")
  ("iride bombe" . "iride bombé") ;   365.61
  ("Barre-Lieou" . "Barré-Liéou") ; 723.2
  ("Garre" . "Garré 730.1")
  ("Legg-Calve-Perthes" . "Legg-Calvé-Perthes") ; 732.1
  ("Dupre" . "Dupré 781.6")
  ("defaillance" . "défaillance") ; 997.5
  ;2) °
  ("Behcet" . "Behçet") ; 616.51
  ;3) Ø
  ("CAVITA" . "CAVITÀ")  ;comments tumori
  ("E necessario" . "È necessario")  ;531   ; von diesen sind mehrere
  ("MORBOSITA E MORTALITA" . "MORBOSITÀ E MORTALITÀ")  ; (797-799)
  ("NECESSITA" . "NECESSITÀ") ; V06.9
  ;in interventi sind nur diese
  ;4) «  ;keien in diagnosi
  ("Malstrom" . "Malmström")  ; 72.7
  ("Duhrssen" . "Dührssen")  ;73.93
  ("Crede" . "Credé")))  ;73.59

;------------
;W 7 insert-path
;------------
;bug?? (icd::insert-key '("12.34 test"))  --> "123.4|12.34 test"  <---????
(defparameter *re-ikey* '(
  ("^(\\d{2,3}\\.\\d)(\\d)$" . "\\1.\\2") ; transform [0]12.34 to [0]12.3.4 - geht für dg u th
  ("^(\\d{2,3}\\.?\\d?)$" . "\\1") ; 12 123 12.3 123.4 --> to itself                          
  ("^.(...)-(...).$" . "\\1\\2") ; (123-456)  --> 123456                                      
  ("^(\\d{1,2})\\.$" . pad-h1)
;  ("^(\\d{1,2})\\.$" . 'pad-h1) ; geht nicht
;  ("^(\\d{1,2})\\.$" . #'pad-h1)     ;, mit s///e   <-------2.8.15
	))  ; 1. --> 01 and 11. --> 11, siehe cll

(defun pad-h1 (s r)
  (declare (ignore s))
  (format nil "~2,,,'0@a" r))




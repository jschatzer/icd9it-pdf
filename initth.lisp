(in-package #:icd9th)

;------------
;W 1 pdf-to-pages
;------------
(defparameter *chapters* 
     '((20 '(1 . 136))  ; vorerst 20 für alle
       (0 '(1 . 6)) (1 '(7 . 12)) (2 '(13 . 14)) (3 '(15 . 22)) (4 '(23 . 26)) (5 '(27 . 32)) (6 '(33 . 38)) (7 '(39 . 54)) (8 '(55 . 56)) 
       (9 '(57 . 74)) (10 '(75 . 80)) (11 '(81 . 84)) (12 '(85 . 90)) (13 '(91 . 92)) (14 '(93 . 112)) (15 '(113 . 118)) (16 '(119 . 136))))

;------------
;W 2 pages-to-column
;------------
(defparameter *tagged-entries* '("01.3 Incisione" "02.1 Trattamento" "03 Interventi" "04 Interventi" "05 Interventi" "07 Interventi" "07.6 Ipofisectomia" "08.4 Riparazione" "10 Interventi" "12 Interventi" "12.5 Interventi" "14 Interventi" "15 Interventi" "16.3 Eviscerazione" "16.9 Altri" "19 Interventi" "20.4 Mastoidectomia" "20.97 Impianto" "21.6 Turbinectomia" "27.4 Asportazione" "29 Interventi" "31.5 Asportazione" "33 Altri" "33.4 Interventi" "34.3 Asportazione" "34.9 Altri" "35.3 Interventi" "35.6 Riparazione" "35.9 Altri" "36.04 Infusione" "37 Altri" "37.3 Pericardiectomia" "37.64 Rimozione" "37.8 Inserzione" "37.91 Massaggio" "38.2 Procedure" "38.5 Legatura" "38.80 Altra" "39.3 Sutura" "39.53 Riparazione" "39.71 Impianto" "39.92 Iniezione" "41 Interventi" "41.4 Asportazione" "42.4 Asportazione" "43 Incisione" "44.3 Gastroenterostomia" "45 Incisione" "45.3 Asportazione" "46 Altri" "46.6 Fissazione" "48 Interventi" "48.7 Riparazione" "49.4 Interventi" "51 Interventi" "51.5 Altra" "52 Interventi" "53 Riparazione" "54 Altri" "54.4 Asportazione" "54.96 Iniezione" "55.3 Asportazione" "56.1 Papillotomia" "57 Interventi" "57.9 Altri" "58.4 Uretroplastiche" "59.8 Cateterizzazione" "60.4 Adenomectomia" "62 Interventi" "64 Interventi" "64.98 Altri" "65.6 Salpingo" "67 Interventi" "68 Altra" "69 Altri" "70 Interventi" "71.1 Procedure" "74 Taglio" "75.4 Rimozione" "76.5 Artroplastica" "77.1 Altra" "77.6 Asportazione" "78 Altri" "78.4 Altri" "78.8 Procedure" "79.1 Riduzione" "79.5 Riduzione" "80 Incisione" "80.4 Incisione" "81 Interventi" "81.2 Artrodesi" "81.6 Altre" "82 Interventi" "82.5 Trapianto" "83.1 Sezione" "84 Altri" "84.2 Reimpianto" "84.62 Inserzione" "84.9 Altri" "85.4 Mastectomia" "86.02 Iniezione" "86.3 Altra" "86.8 Altra" "86.97 Inserzione" "87.3 Radiografia" "88.2 Radiografia" "88.6 Flebografia" "89 Anamnesi" "89.4 Test" "90 Esame" "92.2 Radiologia" "93.3 Altre" "93.8 Altra" "94.3 Psicoterapia" "96 Intubazioni" "96.4 Irrigazione" "97 Sostituzione" "98 Rimozione" "99 Altre" "99.3 Vaccinazioni" "99.6 Cardioversione" "99.9 Altre"))

;------------
;W 3 column-to-items
;------------
;in 81.6 comments darf 81.64 per segnalare ... nicht erfasst werden
;in 96.72 darf 96 ore consecutive ... nicht erfasst werden, hat genügend, 17 spaces
;in 36.1  darf 00.43 con i codici ... nicht erfasst werden, hat nur 10 spaces!! <--
;in 81.6  darf 81.64 ... nicht erfasst werden, hat nur 9 spaced, --> add 6 spaces <---
; 72.21 hat 14 spaces!
(defparameter *tag-re*
  '((h1  "^\\s*(\\d{1,2}\\. [A-Z\\s,'(\\d)-]+?)")  ; 2. TUMORI (140 - 239) <-- space before and after -  !! geht trotzdem nicht??
    (h2  "^\\s{0,8}(\\d\\d\\s+\\w)") ; error: reference to non-existent register 2 in replacement string
    ;(h2  "^\\s{0,8}(\\d\\d\\s+\\w)()")  ;dummy empty () wegen diagnosi, 2.11.13
    ;(:h2  "^\\s{0,8}(\\d\\d\\s+(\\w))")  ;dummy empty () wegen diagnosi, 2.11.13
    (hc3 "^\\s*(\\d\\d\\.\\d\\s+\\w)")   ; siehe 151.5
    (c4  "^\\s{0,14}(\\d\\d\\.\\d\\d\\s+\\w)")))

(defparameter *column-edits*
  '(  ;new codes from comments in pdf------------------------------------------
    ("(Infusione di agenti vasopressori)" . "00.17 \\1")
    ("(Imaging intravascolare di altri vasi non specifi-\\n\\s*cati)" . "00.29 \\1")
    ("(Inserzione di due stent vascolari)" . "00.46 \\1")
    ("(Inserzione di tre stent vascolari)" . "00.47 \\1")
    ("(Inserzione di quattro o piu stent vascolari)" . "00.48 \\1")
    ("(Riduzione aperta di frattura mandibolare)" . "76.76 \\1")
    ;---------------------------------------------------
;    ("(ED INTERVENTI NON CLASSIFICATI ALTROVE)\\s*\\n\\s*\\(00\\)" . "PROCEDURE \\1 (00)") ; "0. PROCEDURE ... (00) wird von regex nicht erkannt"; brauchts nicht mehr, 30.10.13
    ("(00.43 con i codici della serie 36.10-36.19)" . "     \\1")  ; indent 5 more spaces, "hier wird sonst 00.40 fälschlicherweise als entry erfaßt, voerst simply more indent"
    ("(81.64 per segnalare il numero totale di vertebre)" . "      \\1")  ; indent 6 more spaces,   "hier wird sonst 81.64 fälschlicherweise als entry erfaßt"
    ("(Legatura dei vasi delle meningi):" . "\\1")  ; 02.13  zu viel space between code and text   "controlcode is without colon"
    ("(Altra anastomosi intestinale tenue)(crasso)" . "\\1-\\2")  ; 45.93 - wrid mit linebreaks entfernt
    ("(Altra resezione del retto con pull)(through)" . "\\1-\\2") ; 48.49 - wrid mit linebreaks entfernt 
    ("Altre infezioni intestinali da Esche\\n\\s*richia coli" . "Altre infezioni intestinali da Escherichia coli") ;008.09
; how to insert a newline ??
    ("(02.05 Inserzione di placca cranica)" . "\\1                
") ;  "siehe code in csv file"
    ("(cervello e del cranio) (Pneumocister-)" . "\\1
\\2") ;87.02
    ("(Tenotomia della mano) (Sezione di)" . "\\1
\\2") ;82.11
    ))

;------------
;W 4 mark-comments    ... last, because of long man-ht
;------------
(defparameter *hbar2* 
'("02.1" "02.4" "02.9" "03" "04.0" "08.2" "08.6" "09.7" "12" "14.0" "14.1" "16.6" "21.3" "32.0" "34" "35.1" "35.3" "35.5" "35.7" "35.8" "37.3" "37.6" "37.8" "41.0"
	"41.4" "41.9" "48" "48.8" "49.3" "51.6" "52.2" "55.0" "55.1" "58.3" "66.2" "66.3" "75.3" "76.6" "77" "77.6" "78" "78.2" "78.4" "78.5" "78.8" "79.0" "79.1" "79.2"
	"79.3" "80.4" "81.4" "81.5" "81.7" "81.8" "82" "82.3" "82.9" "83" "83.3" "83.8" "83.9" "84.9" "85.2" "85.8" "87.0" "87.1" "89.0" "89.1" "89.2" "89.3" "89.5" "91.5"
	"91.6" "93.5" "98.0" "98.1" "98.2" "98.5" "99.1" "99.2"))
(defparameter *hbar3* '("33.7" "35.6" "90.4" "91.3" "91.4" "96.4" "99.7")) 
(defparameter *hbar4* '("37.7")) 

(defparameter *cbar2* '("52.11" "96.72"))
(defparameter *cbar3* '())

;------------
;W 5 complete-entries
;------------
(defparameter *c* '(1 2 3 4 5 6 9))

(defparameter *new-codes* '("90.0" "90.1" "90.2" "90.3" "90.4" "90.5" "90.6" "90.7" "90.8" "90.9"
"91.0" "91.1" "91.2" "91.3" "91.4" "91.5" "91.6" "91.7" "91.8" "91.9"))

;------------
;WORKFLOW 6 tune-items
;------------
; if car match, replace string after bar with cdr
(defparameter *grkl* '(
("72.4" . "testklammer")
))

;if car match, rm cdr from text
(defparameter *longcode* '(
;chapt13
("75.5" . "Riparazione di lacerazione ostetrica recente ")  ; for testing, include space <----
;("72" . "Parto con forcipe")    ; test <--------------
;("35.0" (0 1 2 3 4) "Valvulotomia a cuore chiuso")
;("35.0" . "Valvulotomia a cuore chiuso")
))


;populate hash table
;(defparameter *reg-lst* *alst*)
;diese liste muß anscheinend wegen dem macro string-l ? hier vorne sein, nach unten verschieben geht nicht! ?? 27.10.13
;alle dem pdf angepasst
; (regex . replacement)
(defparameter *edit-official-code* '(
("’" . "'") 
;("\\"" . "")     ;"; colon for vim color, remove all double quotes
;("\"" . "")     ;"; colon for vim color, remove all double quotes ; 3.7.15, scheint auch zu gehen
;((lol:mkstr #\") . "")     ;"; colon for vim color, remove all double quotes
(#\" . "")     ;remove all double quotes
;(#"""# . "")     ;"; colon for vim color, remove all double quotes
("(20.71 ANASTOMOSI ENDOLINFATIC)A" . "\\1O")
("(27.22 BIOPSIA DELL'UGOLA)" . "\\1 E DEL PALATO MOLLE")
("(32.28 ASPORTAZIONE O DEMOLIZIONE ENDOSCOPICA DI LESIONE O TESSUTO) POLMONARE" . "\\1 DEL POLMONE")
("(33.6 TRAPIANTO COMBINATO CUORE)-(POLMONE)" . "\\1 \\2")
("(55.02 NEFROSTOMIA) CHIRURGICA" . "\\1")
("(55.11 PIELECTOMIA) O PIELOLITOTOMIA" . "\\1")
("(55.4 NEFRECTOMIA PARZIALE) \\(SENZA URETERECTOMIA\\)" . "\\1")  ; ev comments vorerst hier
;("44.32 GASTRODIGIUNOSTOMIA PERCUTANEA [ENDOSCOPICA]" . "44.32 Gastrodigiunostomia percutanea [endo-scopica]")     ; [  problem ??   <-- geht nicht
("(55.61 AUTOTRAPIANTO)" . "\\1 di rene")
("(57.11 PRELIEVO PERCUTANEO DI URINA)" . "\\1 dalla vescica")
("57.95 RIPOSIZIONAMENTO DI CATETERE VESCICALE" . "57.95 Sostituzione di catetere vescicale")
("(57.96 IMPIANTO DI STIMOLATORE VESCICALE) ELETTRICO" . "\\1 elettronico")
("(57.98 RIMOZIONE DI STIMOLATORE VESCICALE) ELETTRICO" . "\\1 elettronico")
("58.22 URETROSCOPIA" . "58.22 Altra uretroscopia")
("58.43 CHIUSURA DI FISTOLA URETRALE" ."58.43 Chiusura di altra fistola uretrale")
("58.93 IMPIANTO O RIPOSIZIONAMENTO DI PROTESI SFINTERIALI" . "58.93 Impianto di sfinteri artificiali urinari")
("59.11 ALTRE LISI DI ADESIONI PERIVESCICALI" . "59.11 Altre lisi di aderenze perivescicali")
("(81.55 REVISIONE DI SOSTITUZIONE DEL GINOCCHIO)" . "\\1, non altrimenti specificata")
))

@END

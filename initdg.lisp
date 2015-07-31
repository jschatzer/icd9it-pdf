;initdg.lisp
(in-package #:icd9dg)

;------------
;W 1  pdf-to-pages
;------------
(defparameter *chapters*
     '((20 '(1 . 421))
       (1 '(1 . 36))   (4 '(77 . 84))    (7 '(139 . 158))  (10 '(189 . 204))  (13 '(251 . 272))  (16 '(301 . 314))
       (2 '(37 . 64))  (5 '(85 . 102))   (8 '(159 . 168))  (11 '(205 . 242))  (14 '(273 . 288))  (17 '(315 . 394))
       (3 '(65 . 76))  (6 '(103 . 138))  (9 '(169 . 188))  (12 '(243 . 250))  (15 '(289 . 300))  (18 '(395 . 421))))

;------------
;W 2  pages-to-column
;------------
(defparameter *tagged-entries* '("365.1 Glaucoma" "743.5 Anomalie" "V15.1 Anamnesi" "098.4 Infezione" "239.3 Tumori" "304.1 Dipendenza" "360.2 Malattie" "379.2 Malattie" "380.2 Altre" "673.1 Embolia" "813.2 Frattura" "839.1 Lussazione" "692.4 Dermatite"))

;------------
;W 3  column-to-items
;------------
;keyword wegen eql :h2 in tag-item
(defparameter *tag-re*
     '((c5 "^\\s{0,15}([\\dV]\\d\\d\\.\\d\\d\\s+\\w)")   ; in 300.22 soll 300.21 nicht erfaßt werden, hat 17 spaces; 645.10 hat 13 spaces
       (ch4 "^\\s{0,12}([\\dV]\\d\\d\\.\\d\\s+\\w)")   ; siehe 151.5, 291.3 hat 6 spaces, 012.0 hat 12 spaces, als einiges
       ;(:ch4 "^\\s{0,8}([\\dV]\\d\\d\\.\\d\\s+\\w)")   ; siehe 151.5, 291.3 hat 6 spaces, 012.0 hat 12 spaces, als einiges, einige (3) keys in comments haben 9 spaces, 3.11.13
       (ch3 "^\\s{0,6}([\\dV]\\d\\d\\s+\\w)")   ; um 199 u 239 in comments h1 Tumori auszuschalten , 202 hat 6 spaces
       (:h2 "^\\s*([A-Z\\s,']+?) (\\([\\dV]\\d\\d-[\\dV]\\d\\d\\))") ; commas in en 140-149  <----  use this and ONLY THIS as keyword, wird in if statement gebraucht, tag-item in icd.lispt <----
       (h1 "^\\s*(\\d{1,2}\\. [A-Z\\s,']+? \\([\\dV]\\d\\d-[\\dV]\\d\\d\\))\\s*$")))  ; für h1 Vcodes angepasst

(defparameter *column-edits* '( 
  ;      9           + 4 = 13 spaces, ch4 werden bis 12 spaces erfasst
  ("(\\s{9}574.3)" . "    \\1")
  ("(\\s{9}574.4)" . "    \\1")
  ("(\\s{9}574.7)" . "    \\1")
  ("\\n\\s+(713.5)" . " \\1")

  ("(CLASSIFICAZIONE SUPPLEMENTARE)" . "18. \\1") ;  insert header nr. 18 for V-codes
  ("493.91( Asma, tipo non specificato, con riacu-\\s+tizzazione)" . "493.92\\1") ;to corrige 2x 493.91 entry
  ("\\(140 - 239\\)" . "(140-239)") ;h1 (140 - 239) remove space, because space is used as key delimiter
  ("\\(V83- V84\\)" . "(V83-V84)") ;h2 (V83- V84) remove space, because space is used as key delimiter
  ("\\(V01 - V06\\)" . "(V01-V06)")   ;h2 (V01 - V06) remove space, because space is used as key delimiter
  ("(599.0)[\\n\\s]+(Infezione del sistema urinario)" . "\\1 \\2")  ;in 599.0 irritiert newline
  ("(813.4)[\\n\\s']+(Frattura chiusa)" . "\\1 \\2")   ;in 813.4 irritiert das ' eines à; der pdf look ist aber normal
  ("(813.5)[\\n\\s']+(Frattura esposta)" . "\\1 \\2")   ;in 813.5 irritiert das ' eines à; der pdf look ist aber normal
  ("(813.4)[\\n\\s']+(Malattia ereditaria)" . "\\1 \\2")  ;in 655.2 irritiert das ' eines à; der pdf look ist aber normal
  ("      (Disturbi specifici del sonno)" . "307.4 \\1")  ;307.4 fehlt, d.h. Titel ohne code, Disturbi specifici del sonno di origine non organica
  ("29 (Forme e complicazioni)" . "429 \\1")  ;429 - hier fehlt im pdf die vorangehende 4
  ("(Altre complicazioni del cordone)" . "663.8 \\1")  ;663.8 - im pdf ist nur Text ohne Code, 663.7 scheint komplett zu fehlen
  ("      941.4 (Necrosi profonda dei tessuti sottostanti)" . "942.4 \\1")  ; 1x statet es bei \n, das 2x mit spaces, 941.4 - im pdf 2x vorhanden, auf S.786 und S.788, 942.4 fehlt, ;941.4 Necrosi profonda dei tessuti sottostanti   ; --->the second to 942.4
  ("(790-)\\n\\s+(796)" . "\\1\\2") ;von header (V70-V82) wird 796 fälschlicherweise als code erfaßt und V70 dafür nicht
  ("696.61 (Avvelenamneto da derivati)" . "965.61 \\1")
  ("696.69 (Avvelenamento da altri antireumatici)" . "965.69 \\1")
  ("     (Altre manifestazioni altrimenti classificabili,)" . "648.94 \\1")
  ("          (Pioderma gangrenoso)" . "686.01 \\1")  ; do not remove the space!!, ist im roten Buch als comment unter 868.00, im ikis und in engl. Version als code mit dieser Nummer
  ("(ASSISTENZA IN RELAZIO) (NE A CURE PREGRESSE)" . "\\1\\2")
  ;("(003.2 Infezioni localizzate da Salmonella)" . "\\1 XX") ;insert newline - hat als einzige fehlenden 2 bar - nicht verständlich; macht deshalb schwierigkeiten mit pos bar
  ("(641.1 Emorragia da placenta )9    (previa)" . "\\1\\2") ; wird von pdf-to-text falsch gelesen

  ("FRATTURA( \\(800-829\\).*)(?=800 Frattura della volta cranica)" . "FRATTURE\\1FRATTURE DEL CRANIO (800-804)
") ; das nicht ändern wegen notwendiger newline ; 9.1.14, plural, scheint besser; im Buch Singular
))

;------------
;W 4 mark-comments    ... last, because of long man-ht
;------------
;674.3     ostetriche 9   ??
(defparameter *hbar2*   ; cbar   headerbar codebar
'("010.8" "013.9" "014.8" "017.0" "017.1" "041" "045.0" "049" "077" "078.8" "079.5" "079.9" "098.3" "099.5" "145" "152" "184" "192" "194" "196" "197" "202.9"
	"208.0" "208.1" "208.2" "210" "213" "215" "225" "227" "235" "238.7" "250.7" "270" "271" "273" "276" "278" "290.2" "293" "294" "294.1" "296.4" "296.5" "296.6"
	"296.9" "299.8" "299.9" "307.5" "310" "312.0" "312.1" "312.2" "313" "313.2" "323.0" "323.4" "323.5" "323.6" "327.0" "327.1" "327.5" "330" "333" "347.1" "360.5"
	"360.6" "362.5" "363.2" "364.6" "364.7" "375.3" "379.6" "380.0" "385.3" "386" "410.0" "410.1" "410.2" "410.3" "410.4" "410.5" "410.8" "410.9" "429.7" "433.9"
	"465" "478.1" "517" "524.0" "524.3" "524.6" "550.1" "550.9" "552" "553" "564.8" "567" "574.3" "574.5" "574.6" "574.7" "574.8" "574.9" "583" "588.8" "597"
	"599.8" "614" "616" "616.8" "620" "626" "639" "641.3" "642.0" "642.2" "642.6" "642.7" "642.9" "646" "646.6" "646.8" "649.0" "649.1" "649.2" "649.3" "649.4"
	"651.7" "652.2" "652.8" "653.0" "653.5" "653.7" "654.2" "654.4" "654.9" "655" "655.0" "655.3" "655.4" "655.8" "658" "658.2" "658.8" "659.7" "659.8" "660.0"
	"660.1" "660.2" "660.6" "660.7" "661.4" "663.1" "663.2" "667.1" "668" "669.4" "669.5" "669.6" "674.3" "674.9" "675" "676" "681" "686" "707.1" "711.1" "711.4"
	"711.8" "712.1" "713" "714" "715.3" "730.8" "733.9" "736.7" "737.4" "738" "744" "744.0" "747.6" "750" "754" "761" "762" "763" "764.0" "764.1" "764.2" "765"
	"771.8" "775" "777" "781" "782" "783.4" "789.3" "792" "793" "793.9" "795" "795.0" "805" "805.0" "806" "812.4" "813.0" "813.4" "819" "827" "847" "851.8" "852"
	"854" "862" "869" "878" "885" "886" "891" "895" "910" "917" "918" "919" "928.2" "948" "965" "965.6" "968" "984" "988" "995.2" "995.6" "996" "996.0" "996.4"
	"996.5" "996.6" "996.7" "997" "997.6" "998.1" "998.3" "998.5" "" "999" "V01.7" "V01.8" "V04" "V05" "V09.7" "V09.8" "V09.9" "V10.0" "V10.1" "V10.2" "V10.4"
	"V10.5" "V10.7" "V10.8" "V12.0" "V12.5" "V12.6" "V13.2" "V15.4" "V16.4" "V16.5" "V23.4" "V25.4" "V29" "V45" "V45.6" "V53.3" "V54.0" "V54.1" "V56" "V58" "V58.1"
	"V58.7" "V65.1" "V71" "V74"))
(defparameter *hbar3*
'("041.0" "041.1" "041.8" "079" "345.4" "345.5" "531.9" "532.9" "533.3" "533.5" "533.6" "533.7" "533.9" "534.9" "583.8" "642.1" "647" "648" "652.1" "656.2"
	"660.3""668.0" "760" "760.7" "794.0" "794.1" "800.2" "800.4" "800.7" "800.9" "801.2" "801.4" "801.7" "801.9" "803.9" "804.0" "804.1" "804.2" "804.3" "804.4"
	"804.5" "804.6" "804.8" "806.3" "828" "851.4" "851.6" "852.0" "852.2" "852.4" "853.0" "853.1" "854.0" "862.2" "863.8" "941.5" "942.4" "942.5" "943.4" "943.5"
	"944.4" "944.5" "945.4" "945.5" "975" "976" "V15.0" "V37.0" "V39.0" "V53.0" "V77.9")) 
(defparameter *hbar4* '("804.7" "804.9"))

(defparameter *cbar3* 
'("304.80" "304.81" "305.70" "410.00" "795.06" "862.29" "862.39" "862.8" "863.29" "863.49" "863.89" "869.0" "V26.22" "V85.52" "V85.53" "V85.54"))
(defparameter *cbar2* 
'("275.40" "292.11" "292.82" "292.83" "292.84" "301.11" "304.00" "304.10" "304.11" "304.12" "304.13" "304.20" "305.20" "305.41" "305.42" "305.80" "313.1"
	"360.20" "368.40" "368.63" "371.70" "376.10" "493.22" "493.92" "525.10" "525.12" "558.9" "627.9" "628.0" "713.6" "719.56" "719.86" "719.96" "730.35" "730.75"
	"799.9" "862.21" "862.22" "862.31" "862.32" "862.9" "863.0" "863.1" "863.39" "863.59" "863.99" "867.0" "867.1" "867.2" "867.3" "867.4" "867.5" "867.6"
	"867.7" "867.8" "867.9" "869.1" "V23.0" "V23.3" "V70.1" "V85.51")) ;"038.10"
(defparameter *cbar1* 
'("V77.8" "V61.5" "V42.89" "V26.51" "V26.21" "V21.1" "V19.7" "820.19" "758.89" "756.89" "756.59" "756.19" "755.69" "755.59" "754.89" "754.69" "754.59"
	"753.29" "752.19" "749.25" "747.89" "745.8" "745.69" "745.19" "728.5" "719.46" "719.36" "718.25" "700" "611.72" "611.4" "564.81" "564.09" "564.02" "529.2"
	"527.4" "527.3" "527.1" "527.0" "526.9" "526.89" "526.81" "526.1" "525.19" "525.13" "525.11" "521.09" "493.02" "474.02" "438.6" "389.8" "374.9" "374.85"
	"374.83" "374.81" "371.89" "370.02" "366.8" "357.6" "352.9" "351.9" "350.9" "334.3" "331.19" "327.09" "318.0" "314.01" "313.82" "309.29" "308.0" "305.30"
	"302.89" "301.89" "301.59" "300.14" "300.09" "296.82" "292.89" "292.81" "292.0" "082.41")) ;"031.2"

;------------
;W 5  complete-entries
;------------
(defparameter *c* '(0 1))

(defparameter *new-codes* 
'("531.0" "531.1" "531.2" "531.3" "531.4" "531.5" "531.6" "531.7" "531.9" 
  "532.0" "532.1" "532.2" "532.3" "532.4" "532.5" "532.6" "532.7" "532.9" 
  "533.0" "533.1" "533.2" "533.3" "533.4" "533.5" "533.6" "533.7" "533.9" 
  "534.0" "534.1" "534.2" "534.3" "534.4" "534.5" "534.6" "534.7" "534.9"
  "535.0" "535.1" "535.2" "535.3" "535.4" "535.5" "535.6"))

;------------
;WORKFLOW 6 tune-items
;------------
;format noch zu testen
;bars gehen nicht gut, ev mit tabs zu testen wegen distorsion
(defparameter *grkl* '(
;
("674.3" . "
Ematoma   > 
Emorragia > taglio cesareo o sutura perineale 
Infezione >
Escl.:   lesioni da strumenti da parto (664.0-665.9)")
;
("641.1" . "
           Placenta ad inserzione   >
              bassa                 >
           Placenta previa:         > SAI o con 
              incompleta            > emorragia
              marginale             > (intraparto)
              parziale              >
              totale                >
           Escl.: emorragia dai vasi previ (663.5)")
;
))

;ev from individual to bulk edit
;vorerst alle 3 belassen
; 1) alist
;if car match, rm cdr from text
(defparameter *longcode* '(
;chapt11
("634.0" ."Aborto spontaneo complicato da infezione del tratto genitale e pelvico, ") ; siehe comma <--
("634.1" . "Aborto spontaneo complicato da emorragia tardiva o eccessiva, ")
("634.2" . "Aborto spontaneo complicato da danno agli organi o tessuti pelvici, ")
))

;2) level1 keys
(defparameter *l1keys* '("640.0" "640.8" "641.0" "641.1"))
;3) level2 keys
(defparameter *l2keys* '("634" 
"800" "801" "802" "804"
))

(defparameter *l1keys* '())
;(defparameter *l2keys* '())

;ch11
;"635" "636" "637" "638" "642" "643" "644" "645" "646" "647" "648" 
;regex fehler ab hier
;"649" "651" "652"
;"653" 
;"654" "655" "656" "657" "658" "659" 
;"660" "661" "662" "663" "664" "665" "666" "667" "668" "669" "670" "671" "672" "673" "674" "675" "676" 
;ch17


;;;;;;;;;;;;;;;;;;;;
(defparameter *edit-official-code* '(  ; there are no " quotes as in interventi
("’" . "'") 
(",(?! )" . ", ") ; insert space after comma, ABORTO,EPISODIO
;chapter 1 ;ungemein viele unnütze editierungen!! 27.10.13
("(031.2 MALATTIA DISSEMINATA) DOVUTA A MICOBATTERI" . "\\1")
("(038.10 SETTICEMIA STAFILOCOCCICA, NON SPECIFICAT)A" . "\\1O")
("038.19 SETTICEMIA DA STAFILOCOCCO" . "038.19 Setticemia da altri stafilococchi")
;("038.41 SETTICEMIA DA HAEMOPHILUS INFLUEN" . "038.41 Setticemia da Haemophilus influenzae (H. influenzae)")
("(038.41 SETTICEMIA DA HAEMOPHILUS INFLUEN)" . "\\1zae (H. influenzae)")
("(041.82 ALTRE INFEZIONI BATTERICHE SPECIFICATE IN MANIFESTAZIONI MORBOSE CLASSIFICATE ALTROVE E DI SEDE NON SPECIFICATA, )" . "\\1Bacterioides fragilis")
("(079.82 SARS-CORONAVIRUS ASSOCIATO) " . "\\1")
("082.40 EHRLICHIOSI, NON SPECIFICATA" . "082.40 Erlichiosi non specificata")
 ("082.41 EHRLICHIA CHAFEENSIS (E.CHAFEENSIS)" . "082.41 Erlichiosi Chaffensis")  ; geht nicht
;chapter 2
("199 TUMORI MALIGNI, SENZA INDICAZIONE DELLA SEDE" . "199 Tumori maligni senza indicazione della sede")
 ("202.26 MALATTIA DI SAZARY, LINFONODI PELVICI" . "202.26 Malattia di Sezary, linfonodi pelvici")
 ("SAZARY" . "Sézary")
(" OCCHIO PEGGIORE:" . "; occhio peggiore:") ;369.01
(" L'ALTRO OCCHIO:" . "; l'altro occhio:")  ; 369.61
("(TRE O PIÙ) " . "\\1") ; V34.00

;; chapt 5 
("(290.10 DEMENZA PRESENILE, NON)" . "\\1 complicata")
))


;what is a conde?  ---> an entry in csv or complete code
; ev only lines: eg:

; 003, 004 :default -> end of 1. line
; 639 - 2 lines

@END

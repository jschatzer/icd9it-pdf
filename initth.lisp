(in-package #:icd9th)
;------------
;W 1 pdf-to-pages
;------------
(o:p *chapters* 
     '((20 '(1 . 136))  ; vorerst 20 für alle
       (0 '(1 . 6)) (1 '(7 . 12)) (2 '(13 . 14)) (3 '(15 . 22)) (4 '(23 . 26)) (5 '(27 . 32)) (6 '(33 . 38)) (7 '(39 . 54)) (8 '(55 . 56)) 
       (9 '(57 . 74)) (10 '(75 . 80)) (11 '(81 . 84)) (12 '(85 . 90)) (13 '(91 . 92)) (14 '(93 . 112)) (15 '(113 . 118)) (16 '(119 . 136))))

;------------
;W 2 pages-to-column
;------------
(o:p *tagged-entries* '("01.3 Incisione" "02.1 Trattamento" "03 Interventi" "04 Interventi" "05 Interventi" "07 Interventi" "07.6 Ipofisectomia" "08.4 Riparazione" "10 Interventi" "12 Interventi" "12.5 Interventi" "14 Interventi" "15 Interventi" "16.3 Eviscerazione" "16.9 Altri" "19 Interventi" "20.4 Mastoidectomia" "20.97 Impianto" "21.6 Turbinectomia" "27.4 Asportazione" "29 Interventi" "31.5 Asportazione" "33 Altri" "33.4 Interventi" "34.3 Asportazione" "34.9 Altri" "35.3 Interventi" "35.6 Riparazione" "35.9 Altri" "36.04 Infusione" "37 Altri" "37.3 Pericardiectomia" "37.64 Rimozione" "37.8 Inserzione" "37.91 Massaggio" "38.2 Procedure" "38.5 Legatura" "38.80 Altra" "39.3 Sutura" "39.53 Riparazione" "39.71 Impianto" "39.92 Iniezione" "41 Interventi" "41.4 Asportazione" "42.4 Asportazione" "43 Incisione" "44.3 Gastroenterostomia" "45 Incisione" "45.3 Asportazione" "46 Altri" "46.6 Fissazione" "48 Interventi" "48.7 Riparazione" "49.4 Interventi" "51 Interventi" "51.5 Altra" "52 Interventi" "53 Riparazione" "54 Altri" "54.4 Asportazione" "54.96 Iniezione" "55.3 Asportazione" "56.1 Papillotomia" "57 Interventi" "57.9 Altri" "58.4 Uretroplastiche" "59.8 Cateterizzazione" "60.4 Adenomectomia" "62 Interventi" "64 Interventi" "64.98 Altri" "65.6 Salpingo" "67 Interventi" "68 Altra" "69 Altri" "70 Interventi" "71.1 Procedure" "74 Taglio" "75.4 Rimozione" "76.5 Artroplastica" "77.1 Altra" "77.6 Asportazione" "78 Altri" "78.4 Altri" "78.8 Procedure" "79.1 Riduzione" "79.5 Riduzione" "80 Incisione" "80.4 Incisione" "81 Interventi" "81.2 Artrodesi" "81.6 Altre" "82 Interventi" "82.5 Trapianto" "83.1 Sezione" "84 Altri" "84.2 Reimpianto" "84.62 Inserzione" "84.9 Altri" "85.4 Mastectomia" "86.02 Iniezione" "86.3 Altra" "86.8 Altra" "86.97 Inserzione" "87.3 Radiografia" "88.2 Radiografia" "88.6 Flebografia" "89 Anamnesi" "89.4 Test" "90 Esame" "92.2 Radiologia" "93.3 Altre" "93.8 Altra" "94.3 Psicoterapia" "96 Intubazioni" "96.4 Irrigazione" "97 Sostituzione" "98 Rimozione" "99 Altre" "99.3 Vaccinazioni" "99.6 Cardioversione" "99.9 Altre"))

;------------
;W 3 column-to-items
;------------
;in 81.6 comments darf 81.64 per segnalare ... nicht erfasst werden
;in 96.72 darf 96 ore consecutive ... nicht erfasst werden, hat genügend, 17 spaces
;in 36.1  darf 00.43 con i codici ... nicht erfasst werden, hat nur 10 spaces!! <--
;in 81.6  darf 81.64 ... nicht erfasst werden, hat nur 9 spaced, --> add 6 spaces <---
; 72.21 hat 14 spaces!
(o:p *tag-re*
  '((h1  "^\\s*(\\d{1,2}\\. [A-Z\\s,'(\\d)-]+?)")  ; 2. TUMORI (140 - 239) <-- space before and after -  !! geht trotzdem nicht??
    (h2  "^\\s{0,8}(\\d\\d\\s+\\w)") ; error: reference to non-existent register 2 in replacement string
    ;(h2  "^\\s{0,8}(\\d\\d\\s+\\w)()")  ;dummy empty () wegen diagnosi, 2.11.13
    ;(:h2  "^\\s{0,8}(\\d\\d\\s+(\\w))")  ;dummy empty () wegen diagnosi, 2.11.13
    (hc3 "^\\s*(\\d\\d\\.\\d\\s+\\w)")   ; siehe 151.5
    (c4  "^\\s{0,14}(\\d\\d\\.\\d\\d\\s+\\w)")))

(o:p *column-edits*
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
;W 5 complete-entries
;------------
(o:p *c* '(1 2 3 4 5 6 9))

(o:p *new-codes* '("90.0" "90.1" "90.2" "90.3" "90.4" "90.5" "90.6" "90.7" "90.8" "90.9"
"91.0" "91.1" "91.2" "91.3" "91.4" "91.5" "91.6" "91.7" "91.8" "91.9"))

;------------
;WORKFLOW 6 tune-items
;------------
; if car match, replace string after bar with cdr
(o:p *grkl* '(
("72.4" . "testklammer")
))

;if car match, rm cdr from text
(o:p *longcode* '(
;chapt13
("75.5" . "Riparazione di lacerazione ostetrica recente ")  ; for testing, include space <----
;("72" . "Parto con forcipe")    ; test <--------------
;("35.0" (0 1 2 3 4) "Valvulotomia a cuore chiuso")
;("35.0" . "Valvulotomia a cuore chiuso")
))

;------------
;W 4 mark-comments    ... last, because of long man-ht
;------------
;populate hash table
;(o:p *reg-lst* *alst*)
;diese liste muß anscheinend wegen dem macro string-l ? hier vorne sein, nach unten verschieben geht nicht! ?? 27.10.13
;alle dem pdf angepasst
; (regex . replacement)
(o:p *edit-official-code* '(
("’" . "'") 
;("\\"" . "")     ;"; colon for vim color, remove all double quotes
(#"""# . "")     ;"; colon for vim color, remove all double quotes
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

(o:p *man-ht* '(
"00.0 Terapia ad ultrasuoni"
"00.2 Imaging intravascolare dei vasi sanguigni"
"00.3 Chirurgia computer-assistita (CAS)"
"00.4 Procedure aggiuntive relative al sistema vascolare"
"00.8 Altre procedure sul ginocchio o sull'anca"
"01 Incisioni ed asportazioni di patologia del cranio, del cervello e delle meningi cerebrali"
"01.2 Craniotomia e craniectomia"
"02 Altri interventi sul cranio, sul cervello e sulle meningi cerebrali"
"02.0 Cranioplastica"
"02.3 Anastomosi ventricolare extracranico"
"02.9 Altri interventi su cranio, cervello e meningi cerebrali"
"03 Interventi sul midollo spinale e sulle strutture del canale vertebrale"
"03.0 Esplorazione e decompressione di strutture del canale vertebrale"
"03.3 Procedure diagnostiche sul midollo spinale e sulle strutture del canale vertebrale"
"03.5 Interventi di plastica sulle strutture del midollo"
"03.7 Anastomosi spinale"
"03.9 Altri interventi sul midollo e sulle strutture del canale vertebrale"
"04.0 Incisione, sezione ed asportazione dei nervi cranici e periferici"
"04.1 Procedure diagnostiche sul sistema nervoso periferico"
"04.4 Separazione di aderenze e decompressione dei nervi cranici e periferici"
"04.8 Iniezione nei nervi periferici"
"05 Interventi sui nervi o sui gangli simpatici"
"05.3 Iniezione in nervi o gangli simpatici"
"06 Interventi sulla tiroide e sulle paratiroidi"
"06.0 Incisione della regione tiroidea"
"06.1 Procedure diagnostiche sulla tiroide e sulle paratiroidi"
"07 Interventi su altre ghiandole endocrine"
"07.0 Esplorazione della regione surrenalica"
"07.4 Altri interventi su nervi, vasi e ghiandole surrenali"
"08 Interventi sulla palpebra"
"08.2 Escissione o demolizione di lesione o tessuto della palpebra"
"08.6 Ricostruzione della palpebra con lembo o innesto"
"08.7 Altra ricostruzione della palpebra"
"09.2 Asportazione di lesione o tessuto della ghiandola lacrimale"
"09.4 Interventi sulle vie lacrimali"
"09.8 Fistolizzazione del tratto lacrimale verso la cavita nasale"
"11.4 Asportazione o demolizione di tessuto o altra lesione della cornea"
"11.6 Trapianto di cornea"
"12 Interventi sull'iride, sul corpo ciliare, sulla sclera, sulla camera anteriore"
"12.0 Rimozione di corpo estraneo intraoculare dal segmento anteriore dell'occhio"
"12.1 Iridotomia ed iridectomia semplice"
"12.2 Procedure diagnostiche sull'iride, sul corpo ciliare, sulla sclera e sulla camera anteriore"
"12.6 Fistolizzazione sclerale"
"12.7 Altri interventi chirurgici per ridurre l'ipertono oculare"
"12.8 Interventi sulla sclera"
"12.9 Altri interventi sull'iride, sul corpo ciliare e sulla camera anteriore"
"13.0 Rimozione di corpo estraneo dal cristallino"
"13.1 Estrazione intracapsulare del cristallino"
"13.4 Estrazione extracapsulare della cataratta con tecnica di frammentazione ed aspirazione"
"13.5 Altra estrazione extracapsulare del cristallino"
"13.6 Altra estrazione di cataratta"
"13.7 Inserzione di protesi endoculare"
"14 Interventi sulla retina, sulla coroide, sul corpo vitreo e sulla camera posteriore"
"14.0 Rimozione di corpo estraneo dal segmento posteriore dell'occhio"
"14.1 Procedure diagnostiche sulla retina, sulla coroide, sul corpo vitreo e sulla camera posteriore"
"14.2 Trattamento di lesione retinica e coroideale"
"14.3 Riparazione di lacerazione della retina"
"14.4 Riparazione di distacco retinico mediante indentazione sclerale con impianto"
"14.5 Altra riparazione di distacco retinico"
"15.0 Procedure diagnostiche su muscoli e tendini extraoculari"
"15.1 Interventi su un muscolo extraoculare che richiedono distacco temporaneo dal bulbo"
"16 Interventi sull'orbita e sul globo oculare"
"16.6 Interventi secondari dopo rimozione del bulbo oculare"
"16.8 Riparazione di lesione del bulbo oculare e dell'orbita"
"16.9 Altri interventi sull'orbita e sul bulbo oculare"
"18 Interventi sull'orecchio esterno"
"18.0 Incisione dell'orecchio esterno"
"18.3 Altra asportazione dell'orecchio esterno"
"18.7 Altra riparazione plastica dell'orecchio esterno"
"19.1 Stapedectomia"
"20 Altri interventi sull'orecchio medio e sull'orecchio interno"
"20.3 Procedure diagnostiche sull'orecchio medio e sull'orecchio interno"
"20.4 Mastoidectomia"
"20.5 Altra asportazione dell'orecchio medio"
"21 Interventi sul naso"
"21.3 Asportazione o demolizione locale di lesione del naso"
"21.8 Interventi di riparazione e di plastica del naso"
"22.6 Altra senectomia nasale"
"24.1 Procedure diagnostiche su denti, gengive e alveoli"
"26 Interventi sulle ghiandole e sui dotti salivari"
"26.1 Procedure diagnostiche sulle ghiandole e dotti salivari"
"27 Altri interventi sulla bocca e sulla faccia"
"27.3 Asportazione di lesione o tessuto del palato osseo"
"27.5 Riparazione plastica della bocca"
"29 Interventi sul faringe"
"31.7 Interventi di riparazione e di plastica sulla trachea"
"32 Asportazione del polmone e dei bronchi"
"32.0 Asportazione o demolizione locale di lesione o tessuto dei bronchi"
"33 Altri interventi sul polmone e sui bronchi"
"33.2 Procedure diagnostiche sul polmone e sui bronchi"
"33.5 Trapianto del polmone"
"34 Interventi sulla parete toracica, sulla pleura, sul mediastino e sul diaframma"
"34.0 Incisione della parete toracica e della pleura"
"34.2 Procedure diagnostiche su parete toracica, pleura, mediastino e diaframma"
;"35 Interventi sulle valvole e sui setti del cuore"
;"35.0 Valvulotomia a cuore chiuso"
;"35.2 Sostituzione di valvola cardiaca"
"35.6 Riparazione dei setti interatriale e interventricolare con innesto tissutale (sintetico o biologico)"
"35.3 Interventi su strutture adiacenti le valvole cardiache"
"35.5 Riparazione con protesi dei setti interatriale e interventricolare"
"35.7 Altra e non specificata riparazione dei setti interatriale e interventricolare"
;"35.9 Altri interventi su valvole e setti del cuore"
;"36 Interventi sui vasi del cuore"
;"36.1 Bypass per rivascolarizzazione cardiaca"
;"36.9 Altri interventi sui vasi del cuore"
;"37 Altri interventi sul cuore e sul pericardio"
;"37.1 Cardiotomia e pericardiotomia"
"37.3 Pericardiectomia ed escissione di lesione del cuore"
"37.6 Impianto di sistemi di assistenza cardiaca e di circolazione assistita"
"37.7 Inserzione, revisione, sostituzione, rimozione di elettrodi; inserzione di sistema di pacemaker temporaneo; o revisione di tasca per dispositivo cardiaco"
"38 Incisione, asportazione ed occlusione di vasi"
;"38.0 Incisione di vasi"
;"38.1 Endoarteriectomia"
;"38.2 Procedure diagnostiche sui vasi sanguigni"
;"38.3 Resezione di vasi con anastomosi"
;"38.4 Resezione di vasi con sostituzione"
;"38.5 Legatura e stripping di vene varicose"
;"38.6 Altra asportazione di vasi"
;"38.8 Altra occlusione chirurgica di vasi"
;"38.9 Puntura di vasi"
;"39 Altri interventi sui vasi"
;"39.2 Altre anastomosi o bypass vascolari"
;"39.3 Sutura di vasi"
"39.6 Circolazione extracorporea e procedure ausiliarie per la chirurgia cardiaca"
;"39.7 Riparazione endovascolare di vaso"
"39.8 Interventi sul glomo carotideo e su altri glomi vascolari"
;"40.2 Asportazione semplice di strutture linfatiche"
"40.4 Asportazione radicale di linfonodi cervicali"
"40.5 Asportazione radicale di altri linfonodi"
"41.3 Procedure diagnostiche sul midollo osseo e sulla milza"
"41.9 Altri interventi sulla milza e sul midollo osseo"
"42.3 Asportazione o demolizione locale di lesione o tessuto dell'esofago"
"42.4 Asportazione dell'esofago"
"42.5 Anastomosi intratoracica dell'esofago"
"42.6 Anastomosi presternale dell'esofago"
"43 Incisione ed asportazione dello stomaco"
"44 Altri interventi sullo stomaco"
"44.4 Controllo di emorragia e sutura di ulcera gastrica o duodenale"
"45 Incisione, asportazione e anastomosi dell'intestino"
"45.0 Enterotomia"
"45.1 Procedure diagnostiche sull'intestino tenue"
"45.2 Procedure diagnostiche sull'intestino crasso"
"45.4 Asportazione o demolizione locale di lesione o tessuto dell'intestino crasso"
"45.5 Isolamento di segmento intestinale"
"45.6 Altra asportazione dell'intestino tenue"
"45.7 Asportazione parziale dell'intestino crasso"
"45.9 Anastomosi intestinale"
"46 Altri interventi sull'intestino"
"46.0 Esteriorizzazione dell'intestino"
"46.1 Colostomia"
"46.2 Ileostomia"
"46.3 Altra enterostomia"
"46.5 Chiusura di orifizio intestinale artificiale"
"46.7 Altra riparazione dell'intestino"
"46.8 Dilatazione e manipolazione intraaddominale dell'intestino"
"47 Interventi sull'appendice"
"47.0 Appendicectomia"
"48 Interventi sul retto, sul rettosigmoide e sui tessuti perirettali"
"48.3 Asportazione o demolizione locale di lesione o tessuto del retto"
"48.4 Resezione del retto con pull-through"
"48.6 Altra resezione del retto"
"48.7 Riparazione del retto"
"49 Interventi sull'ano"
"49.1 Incisione o asportazione di fistola anale"
"49.2 Procedure diagnostiche sull'ano e sui tessuti perianali"
"49.3 Asportazione o demolizione locale di altra lesione o tessuto dell'ano"
"49.7 Riparazione dell'ano"
"49.9 Altri interventi sull'ano"
"50 Interventi sul fegato"
"50.2 Asportazione o demolizione locale di tessuti o lesioni del fegato"
"50.5 Trapianto del fegato"
"50.9 Altri interventi sul fegato"
"51 Interventi sulla colecisti e sulle vie biliari"
"51.1 Procedure diagnostiche sulle vie biliari"
"51.3 Anastomosi della colecisti o del dotto biliare"
"51.4 Incisione del dotto biliare per rimozione di occlusione"
"51.5 Altra incisione del dotto biliare"
"51.6 Asportazione o demolizione locale di lesione o tessuto del dotto biliare e dello sfintere di Oddi"
"51.8 Altri interventi sullo sfintere di Oddi e sui dotti biliari"
"52 Interventi sul pancreas"
"52.5 Pancreatectomia parziale"
"52.8 Trapianto del pancreas"
"53 Riparazione di ernia"
"53.4 Riparazione di ernia ombelicale"
"53.8 Riparazione di ernia diaframmatica, per via toracica"
"54 Altri interventi sulla regione addominale"
"54.5 Lisi di aderenze peritoneali"
"54.7 Altra riparazione della parete addominale e del peritoneo"
"54.9 Altri interventi sulla regione addominale"
"55 Interventi sul rene"
"55.0 Interventi sul parenchima renale (nefrotomia e nefrostomia)"
"55.1 Interventi sulla pelvi renale (pielotomia o pielostomia)"
"55.5 Nefrectomia"
"55.6 Trapianto renale"
"55.9 Altri interventi sul rene"
"56 Interventi sull'uretere"
"56.4 Ureterectomia"
"56.7 Altre anastomosi ureterali"
"57 Interventi sulla vescica"
"57.1 Cistotomia e cistostomia"
"57.2 Vescicostomia"
"57.4 Asportazione o demolizione transuretrale di tessuto vescicale"
"57.5 Altra chirurgia vescicale"
"57.7 Cistectomia totale"
"57.8 Interventi di riparazione vescicale"
"58 Interventi sull'uretra"
"58.4 Uretroplastiche"
"59 Altri interventi sull'apparato urinario"
"59.9 Altri interventi sull'apparato urinario"
"60 Interventi sulla prostata e sulle vescicole seminali"
"60.1 Procedure diagnostiche sulla prostata e vescicole seminali"
"60.2 Prostatectomia transuretrale"
"61.1 Procedure diagnostiche sullo scroto e tunica vaginale"
"61.9 Altri interventi sullo scroto e sulla tunica vaginale"
"62.4 Orchiectomia bilaterale"
"62.6 Riparazione dei testicoli"
"63 Interventi sul cordone spermatico, epididimo e sui dotti deferenti"
"64 Interventi sul pene"
"64.4 Interventi di riparazione e di plastica del pene"
"64.9 Altri interventi sull'apparato genitale maschile"
"65 Interventi sull'ovaio"
"65.0 Ovariotomia"
"65.2 Asportazione o demolizione locale di lesione o tessuto ovarico"
"65.7 Riparazione dell'ovaio"
"66 Interventi sulle tube di Falloppio"
"66.2 Demolizione od occlusione endoscopica bilaterale delle tube"
"66.3 Altra demolizione od occlusione bilaterale delle tube"
"66.5 Salpingectomia totale bilaterale"
"66.6 Altra salpingectomia"
"67 Interventi sulla cervice uterina"
"67.3 Altra asportazione o demolizione di lesione o tessuto della cervice"
"67.6 Altra riparazione della cervice"
"68 Altra incisione od asportazione dell'utero"
"68.1 Procedure diagnostiche sull'utero e strutture di sostegno"
"68.3 Isterectomia addominale subtotale"
"68.4 Isterectomia addominale totale"
"68.5 Isterectomia vaginale"
"68.6 Isterectomia addominale radicale"
"68.7 Isterectomia vaginale radicale"
"69 Altri interventi sull'utero e sulle strutture di sostegno"
"69.0 Dilatazione e raschiamento dell'utero"
"69.4 Riparazione uterina"
"69.5 Raschiamento dell'utero mediante aspirazione"
"70 Interventi sulla vagina e sul cul-de-sac"
"70.2 Procedure diagnostiche sulla vagina e sul cul-de-sac"
"70.3 Asportazione o demolizione locale della vagina e del cul-de-sac"
"70.7 Altra riparazione della vagina"
"71 Interventi sulla vulva e sul perineo"
"71.7 Riparazione della vulva e del perineo"
"72.7 Estrazione mediante ventosa"
"74 Taglio cesareo ed estrazione del feto"
"75.5 Riparazione di lacerazione ostetrica recente dell'utero"
"75.6 Riparazione di altra lacerazione ostetrica recente"
"76 Interventi sulle ossa e sulle articolazioni della faccia"
"76.0 Incisione delle ossa della faccia senza sezione"
"76.4 Asportazione e/o ricostruzione di ossa della faccia"
"76.7 Riduzione di frattura facciale"
"77 Incisione, asportazione e sezione di altre ossa"
"77.1 Altra incisione dell'osso, osteotomia"
"77.2 Resezione ossea cuneiforme"
"77.3 Altra sezione dell'osso"
"77.5 Asportazione di borsite e correzione di altre deformita delle dita"
"77.6 Asportazione locale di lesione o di tessuto osseo"
"77.8 Altra osteotomia parziale"
"77.9 Ostectomia totale"
"78 Altri interventi sulle ossa ad eccezione di quelle facciali"
"78.0 Innesto osseo"
"78.1 Applicazione di fissatore esterno"
"78.2 Interventi di accorciamento delle ossa degli arti"
"78.3 Interventi di allungamento delle ossa degli arti"
"78.4 Altri interventi di riparazione o plastica su osso"
"78.5 Fissazione interna di osso senza riduzione di frattura"
"78.6 Rimozione di dispositivi impiantati"
"78.9 Inserzione di stimolatore di crescita ossea"
"79 Riduzione di frattura e di lussazione"
"79.0 Riduzione chiusa di frattura, senza fissazione interna"
"79.3 Riduzione cruenta di frattura con fissazione interna"
"79.4 Riduzione incruenta di epifisiolisi"
"79.5 Riduzione cruenta di epifisiolisi"
"79.6 Sbrigliamento in sede di frattura esposta"
"79.7 Riduzione incruenta di lussazione"
"79.8 Riduzione cruenta di lussazione"
"80 Incisione ed asportazione di strutture articolari"
"80.0 Artrotomia per rimozione di protesi"
"80.1 Altra artrotomia"
"80.3 Biopsia delle strutture articolari"
"80.4 Incisione di capsula articolare, legamenti o cartilagine"
"80.7 Sinoviectomia"
"80.8 Altra asportazione o demolizione locale di lesione dell'articolazione"
"80.9 Altra asportazione dell'articolazione"
"81 Interventi di riparazione e plastica sulle strutture articolari"
"81.0 Artrodesi vertebrale"
"81.1 Artrodesi e artroeresi del piede e della caviglia"
"81.2 Artrodesi di altra articolazione"
"81.3 Rifusione della colonna vertebrale"
"81.4 Altra riparazione di articolazione delle estremita inferiori"
"81.5 Sostituzione di articolazione delle estremita inferiori"
"81.6 Altre procedure sulla colonna vertebrale"
"81.7 Artroplastica e riparazione della mano, delle dita e del polso"
"81.8 Artroplastica e riparazione della spalla e del gomito"
"82 Interventi sui muscoli, sui tendini e sulle fasce della mano"
"82.0 Incisione di muscoli, tendini, fasce e borse della mano"
"82.1 Sezione di muscoli, tendini e fasce della mano"
"82.2 Asportazione di lesione di muscoli, tendini e fasce della mano"
"82.3 Altra asportazione dei tessuti molli della mano"
"82.4 Sutura di muscoli, tendini e fasce della mano"
"82.6 Ricostruzione del pollice"
"82.7 Intervento di plastica sulla mano con innesto o impianto"
"82.9 Altri interventi su muscoli, tendini e fasce della mano"
"83 Interventi sui muscoli, sui tendini, sulle fasce o sulle borse, ad eccezione della mano"
"83.2 Procedure diagnostiche su muscoli, tendini, fasce e borse, incluse quelle della mano"
"83.3 Asportazione di lesione dei muscoli, tendini, fasce e borse"
"83.4 Altra asportazione di muscoli, tendini e fasce"
"83.7 Ricostruzione di muscoli e tendini"
"83.8 Altri interventi di plastica su muscoli, tendini e fasce"
"83.9 Altri interventi su muscoli, tendini, fasce e borse"
"84.0 Amputazione dell'arto superiore"
"84.1 Amputazione dell'arto inferiore"
"84.5 Impianto di altri dispositivi o materiali muscoloscheletrici"
"84.6 Sostituzione di disco vertebrale"
"84.7 Codici aggiuntivi per fissatori esterni"
"85 Interventi sulla mammella"
"85.2 Asportazione o demolizione di tessuto della mammella"
"85.3 Mammoplastica riduttiva e mammectomia sottocutanea"
"85.5 Mammoplastica di ingrandimento"
"85.8 Altri interventi di riparazione e plastica sulla mammella"
"86 Interventi sulla cute e sul tessuto sottocutaneo"
"86.6 Innesto cutaneo libero"
"86.7 Impianto di lembi peduncolati"
"86.8 Altra riparazione e ricostruzione di cute e tessuto sottocutaneo"
"87.0 Radiologia dei tessuti molli della faccia, del capo e del collo"
"87.1 Altra radiografia della faccia, del capo e del collo"
"87.3 Radiografia dei tessuti molli del torace"
"87.4 Altra radiografia del torace"
"87.7 Radiografia diretta dell'apparato urinario"
"88 Altre procedure diagnostiche radiologiche e relative tecniche"
"88.0 Radiografia dei tessuti molli dell'addome"
"88.2 Radiografia degli arti e della pelvi"
"88.4 Arteriografia con mezzo di contrasto"
"88.5 Angiocardiografia con mezzo di contrasto"
"88.6 Flebografia"
"88.7 Diagnostica a ultrasuoni"
"88.9 Altre tecniche di produzione di immagini diagnostiche"
"89.0 Anamnesi, valutazione, consulto e valutazione diagnostica"
"89.1 Misure anatomiche e fisiologiche ed esami manuali - sistema nervoso e organi di senso"
"89.2 Misure anatomiche e fisiologiche ed esami manuali - Sistema genitourinario"
"89.3 Altre misure anatomiche e fisiologiche ed esami manuali"
"89.4 Test cardiologici da sforzo e controllo di pace-maker e di defibrillatore"
"89.5 Altri test funzionali diagnostici non invasivi cardiologici e vascolari"
"89.6 Monitoraggio circolatorio"
"90 Esame microscopico - I"
"90.0 Esame microscopico di campione del sistema nervoso centrale e liquido spinale"
"90.1 Esame microscopico di campione delle ghiandole endocrine, non citate altrove"
"90.3 Esame microscopico di campione di orecchio, naso, gola e laringe"
"90.6 Esame microscopico di campione di milza e midollo osseo"
"90.7 Esame microscopico di campione di linfonodi e linfa"
"90.8 Esame microscopico di campione di tubo gastroenterico superiore e del vomito"
"90.9 Esame microscopico di campione di tubo gastroenterico inferiore e feci"
"91 Esame microscopico - II"
"91.0 Esame microscopico di campione di fegato, vie biliari e pancreas"
"91.2 Esame microscopico di campione di rene, uretere, tessuto perirenale e periureterale"
"91.3 Esame microscopico di campione di vescica, uretra, prostata, vescicole seminali, tessuto perivescicale, e dell'urina e del seme"
"91.4 Esame microscopico di campione di vie genitali femminili, feto, sacco amniotico"
"91.5 Esame microscopico di campione di sistema muscoloscheletrico e liquido articolare"
"91.6 Esame microscopico di campione di cute e altri tegumenti"
"91.7 Esame microscopico di campione di ferita operatoria"
"91.8 Esame microscopico di campione di altre sedi"
"91.9 Esame microscopico di campione di sede non specificata"
"92.2 Radiologia terapeutica e medicina nucleare"
"92.3 Radiochirurgia stereotassica"
"93 Terapia fisica, terapia respiratoria, riabilitazione e procedure correlate"
"93.5 Altre immobilizzazioni, pressioni e cure a ferite"
"93.7 Riabilitazione del linguaggio e della lettura e riabilitazione del non vedente"
"93.9 Terapia respiratoria"
"94.6 Riabilitazione e disintossicazione da alcool e farmaci"
"95.2 Test funzionali obiettivi dell'occhio"
"96.0 Intubazioni gastrointestinali e del tratto respiratorio non operatorie"
"96.1 Altre inserzioni non operatorie"
"96.4 Irrigazione, pulizia e instillazione locale di altri organi dell'apparato digerente e genitourinario"
"96.7 Altra ventilazione meccanica continua"
"97.0 Sostituzione non operatoria di sussidio gastrointestinale"
"97.1 Sostituzione non operatoria di sussidio per il sistema muscoloscheletrico e tegumentario"
"97.3 Rimozione non operatoria di apparecchiature terapeutiche da testa e collo"
"97.6 Rimozione non operatoria di dispositivo terapeutico dell'apparato urinario"
"97.7 Rimozione non operatoria di dispositivo terapeutico dell'apparato genitale"
"97.8 Altra rimozione non operatoria di dispositivi terapeutici"
"98.0 Rimozione di corpo estraneo intraluminale dell'apparato digerente senza incisione"
"98.1 Rimozione di corpo estraneo intraluminale da altre sedi senza incisione"
"98.2 Rimozione di altro corpo estraneo senza incisione"
"98.5 Litotripsia extracorporea per onda d'urto (ESWL)"
"99.0 Trasfusione di sangue e componenti ematici"
"99.6 Cardioversione del ritmo cardiaco"
))

# tobias meißner
# 29.04.2009

# todo + ideen:

# statusleiste, mit anzeige bzgl. analyse usw... mal schauen was da machbar ist..
# speichern dialog + button, wie umgang mit temporär erzeugten dateien? speicherverbrauch? 
# qualitätskontrolle mit eingaben abgleichen und gegebenfalls hinweise ausgeben
# hilfe dialog
# hilfe button mit erläuterungen zu den feldern -.-> manual
# gep-analyse tab soll focus direkt nach start haben
# bedingt ausschalten über 'x' das gleiche wie über button? werden temp. dateien gelöscht?
# evtl. batch mode zum vorverarbeiten mehrere cel files mit anschließender befundung?, ist das in dieser umgebung so realisierbar?
# quit-button nach rechts
# feste größe für eingabefelder? in pixeln? oder dynamisch lassen?
# automatische abfrage ob wirklich beendet werden soll, wurde gespeichert??
# überprüfung das alle felder (richtig) ausgefüllt sind
# datenbank anbindun um automatisch patientendaten zu übernehmen
# yaqc() durch qc() ersetzen

# KNOWN BUGS

# beim laden kommt folgender fehler: (R:27935): Gtk-CRITICAL **: gtk_text_buffer_set_text: assertion `text != NULL' failed  --> noch keine zuordnung.. aber auch kein größeres problem erkennbar, liegt vermutlich daran das evtl. einige textfelder leer sind
# überprüfung der identitycontrol funtioniert nicht
# speichern soll nicht möglich sein wenn noch keine analyse gelaufen ist!
# statusleiste zeigt nicht unbedingt das an was sie soll

require(gWidgets)
options("guiToolkit"="RGtk2")
# source("scripts/befund.R") # funktion zur berechnung des befundes run.befund()

# toolbar Handler
quitHandler = function(h, ...) {
	dispose(win)
	system("rm *.pdf *.log *.aux *.tex temp/*") # löschen der temporären dateien

	# todo: automatische abfrage ob wirklich beendet werden soll, wurde gespeichert??
}

# befund speichern handler
saveHandler = function(h, ...) {
	# speichern aller cel-file bezogenen variablen in einer liste, 
	# erzeugen eines ordners indem die abbildungen kopiert werden und das r-objekt mit den variablen abgelegt wird

	system(paste("mkdir -p save/", svalue(cel.label), sep="")) # verzeichnis mit namen des cel-file anlegen, überschreiben wenn schon vorhanden
	system(paste("cp temp/*.gif save/", svalue(cel.label), sep="")) # abbildungen aus temp ins neue verzeichnis kopieren
	system(paste("cp temp/*.pdf save/", svalue(cel.label), sep="")) # abbildungen aus temp ins neue verzeichnis kopieren
	
	# speichern der "inputs"
	save = list(
	       	cel = svalue(cel.label),
		plot = svalue(plot),
		p.name = svalue(p.name),
		p.vorname = svalue(p.vorname),
		p.geb = svalue(p.geb),
		p.strasse = svalue(p.strasse),
		p.ort = svalue(p.ort),
		p.plz = svalue(p.plz),
		p.diag = svalue(p.diag),
		p.igtype = svalue(p.igtype),
		p.lk = svalue(p.lk),
		p.sex = svalue(p.sex),
		probe.date = svalue(probe.date),
		probe.volume = svalue(probe.volume),
		probe.protokoll = svalue(probe.protokoll),
		probe.rna = svalue(probe.rna),
		probe.array = svalue(probe.array),
		probe.ampl = svalue(probe.ampl),
		probe.norm = svalue(probe.norm),
		beurteilung = svalue(beurteilung),
		befund.qualitycontrol = svalue(befund.qualitycontrol),
		befund.identitycontrold = svalue(befund.identitycontrol),
		befund.risk = svalue(befund.risk),
		befund.genes = svalue(befund.genes),
		befund.itherapy = svalue(befund.itherapy),
		befund.grtherapy = svalue(befund.grtherapy)
	       )
	
	# speichern der variablen als r-objekt, hier mit der endung *.befund
	tosave = c("save", "anzahl.bmpc", "anzahl.mmc", "bergsagel", "decaux", "ec", "gpi", 
		   "lightchain", "sex", "shaughnessy", "shrisk", "type", "yaqc", "nr.genes",
		   "aurka", "aurka.bmpc.signal", "aurka.mmc.signal", "aurka.signal", "p.aurka.bmpc", "p.aurka.mmc",
		   "ctag1", "ctag1.bmpc.signal", "ctag1.mmc.signal", "ctag1.signal", "p.ctag1.bmpc", "p.ctag1.mmc",
		   "cyclind1", "cyclind1.bmpc.signal", "cyclind1.mmc.signal", "cyclind1.signal", "p.cyclind1.bmpc", "p.cyclind1.mmc",
		   "cyclind2", "cyclind2.bmpc.signal", "cyclind2.mmc.signal", "cyclind2.signal","p.cyclind2.bmpc", "p.cyclind2.mmc",
		   "cyclind3", "cyclind3.bmpc.signal", "cyclind3.mmc.signal", "cyclind3.signal", "p.cyclind3.bmpc", "p.cyclind3.mmc",
		   "fgfr3", "fgfr3.bmpc.signal", "fgfr3.mmc.signal", "fgfr3.signal", "p.fgfr3.bmpc", "p.fgfr3.mmc",
		   "hm124", "hm124.bmpc.signal", "hm124.mmc.signal", "hm124.signal", "p.hm124.bmpc", "p.hm124.mmc",
		   "magea1", "magea1.bmpc.signal", "magea1.mmc.signal", "magea1.signal", "p.magea1.bmpc", "p.magea1.mmc",
		   "magea3", "magea3.bmpc.signal", "magea3.mmc.signal", "magea3.signal", "p.magea3.bmpc", "p.magea3.mmc",
		   "mmset", "mmset.bmpc.signal", "mmset.mmc.signal", "mmset.signal", "p.mmset.bmpc", "p.mmset.mmc",
		   "muc1", "muc1.bmpc.signal", "muc1.mmc.signal", "muc1.signal", "p.muc1.bmpc", "p.muc1.mmc",
		   "ssx2", "ssx2.bmpc.signal", "ssx2.mmc.signal", "ssx2.signal", "p.ssx2.bmpc", "p.ssx2.mmc")


	save(file=paste("save/", svalue(cel.label), "/", svalue(cel.label), ".befund", sep=""), list=tosave)

	svalue(sb) = paste("Befund", svalue(cel.label), "erfolgreich gespeichert", sep=" ")
}

# Befund laden handler
loadHandler = function(h, ...) {
	# laden des ausgewählten r-objekts,(befund) 
	# kopieren der gespeicherten pdf-abbildungen in den temp ordner

	              gfile(text="Bitte ein Rdata-Objekt auswählen...", 
			    type="open", 
			    action = function(h, ...) {print(h)
						       load(h, envir=.GlobalEnv)
						       },
			    filter=list("Befund-Objekte"=list(patterns=c("*.befund"))),
			    cont=file,			
			    handler = function(h, ...) {do.call(h$action, list(h$file))}
			    )	

	# setzen der "werte" in den eingabefeldern
	svalue(cel.label) = save$cel
	svalue(plot) = save$plot
	svalue(p.name) = save$p.name
	svalue(p.vorname) = save$p.vorname
	svalue(p.geb) = save$p.geb
	svalue(p.strasse) = save$p.strasse
	svalue(p.ort) = save$p.ort
	svalue(p.plz) = save$p.plz
	svalue(p.diag) = save$p.diag
	svalue(p.igtype) = save$p.igtype
	svalue(p.lk) = save$p.lk
	svalue(p.sex) = save$p.sex
	svalue(probe.date) = save$probe.date
	svalue(probe.volume) = save$probe.volume
	svalue(probe.protokoll) = save$probe.protokoll
	svalue(probe.rna) = save$probe.rna
	svalue(probe.array) = save$probe.array
	svalue(probe.ampl) = save$probe.ampl
	svalue(probe.norm) = save$probe.norm
	svalue(beurteilung) = save$beurteilung
	svalue(befund.qualitycontrol) = save$befund.qualitycontrol
	svalue(befund.identitycontrol) = save$befund.identitycontrol
	svalue(befund.risk) = save$befund.risk
	svalue(befund.genes) = save$befund.genes
	svalue(befund.itherapy) = save$befund.therapy
	svalue(befund.grtherapy) = save$befund.grtherapy
	svalue(sb) = save$sb
	
	# abbildungen kopieren
	system(paste("cp -f save/", svalue(cel.label), "/*.gif temp/", sep="")) 
	system(paste("cp -f save/", svalue(cel.label), "/*.pdf temp/", sep="")) 

	# handler erneut aufrufen
	identHandler()			# ausgabe ic, nb rechts
	riskHandler()			# ausgabe risk, nb rechts
	geneHandler()			# ausgabe gene, nb rechts
	qctableHandler()		# ausgabe qc, rechts
	enabled(qc) = "TRUE"		# qualitätskontrolle anzeige einschalten
	enabled(tables) = "TRUE"	# ergebnisse gene usw. einschalten..
	enabled(tbl.befund) = "TRUE"	# befund aktivieren
	enabled(tbl.beurteilung) = "TRUE" # beurteilung aktiveiren
	enabled(file.pdfcreate)="TRUE"  # button zum pdf erzeugen aktivieren

	svalue(sb) = paste("Befund", svalue(cel.label), "erfolgreich geladen", sep=" ")
}

# open File Handler
chooseFile = function(h, ...) {
	              gfile(text="Bitte ein CEL-File auswählen...", 
			    type="open", 
			    action = function(h, ...) {print(h)
						       cel.file = print(h)
						       assign("cel.file", cel.file, envir=.GlobalEnv)
						       svalue(cel.label) =  unlist((strsplit(cel.file, "/")))[length(unlist((strsplit(cel.file, "/"))))]
						       svalue(sb) = "Die Analyse kann nun gestartet werden"
						       enabled(file.analyse)="TRUE" # button zum start der analyse aktivieren
						       },
			    filter=list("CEL-Files"=list(patterns=c("*.CEL"))),
			    cont=file,			
			    handler = function(h, ...) {do.call(h$action, list(h$file))}
	)
}

# starte analyse handler
runAnalysis = function(h, ...) {
	svalue(sb) = "Analyse läuft ... bitte etwas Geduld!"         # <<-- das will noch nicht sorecht, vermutlich ausglidern und vor ranAnalysis() ausführen
	source("scripts/befund.R")	# durchfürhren der analyse
	#run.befund(cel.file)
	identHandler()			# ausgabe ic, nb rechts
	riskHandler()			# ausgabe risk, nb rechts
	geneHandler()			# ausgabe gene, nb rechts
	qctableHandler()		# ausgabe qc, rechts
	enabled(qc) = "TRUE"		# qualitätskontrolle anzeige einschalten
	enabled(tables) = "TRUE"	# ergebnisse gene usw. einschalten..
	enabled(tbl.befund) = "TRUE"	# befund aktivieren
	enabled(tbl.beurteilung) = "TRUE" # beurteilung aktiveiren
	enabled(file.pdfcreate)="TRUE"  # button zum pdf erzeugen aktivieren
	saveHandler()			# speichern
	svalue(sb) = "Analysis beendet & gespeichert, bitte nun befunden!"
	}

# diplay handler
dispHandlerMAQC = function(h, ...) {
	svalue(plot) = "temp/qualityplot.gif"
}
dispHandlerQCSTATS = function(h, ...) {
	svalue(plot) = "temp/qcsummary.gif"
}

# identity control
identHandler = function(h, ...) {
	ictable[1][[1]] = as.character(sex)
	ictable[1][[2]] = as.character(type)
	ictable[1][[3]] = as.character(lightchain)

	# ist vermutlich noch nicht optimal, was passiert wenn eingabe geändert wird, automatisches update??
	if (sex==svalue(p.sex) & type==svalue(p.igtype) & lightchain==svalue(p.lk)) {svalue(befund.identitycontrol) = "Hinweis: Identitätskonrolle bestanden"}
	if (sex!=svalue(p.sex) | type!=svalue(p.igtype) | lightchain!=svalue(p.lk)) {svalue(befund.identitycontrol) = "Hinweis: Identitätskonrolle nicht bestanden, bitte Eingaben überprüfen!"}
}

# risikoklassifizierungen
riskHandler = function(h, ...) {
	risktable[1][[1]] = as.character("Decaux")
	risktable[1][[2]] = as.character(decaux$decaux.risk)
	risktable[1][[3]] = as.character("[high;low]")

	risktable[2][[1]] = as.character("Shaughnessy 70 Genes Abs. dist.")
	risktable[2][[2]] = as.character(shaughnessy$predicted.abs)
	risktable[2][[3]] = as.character("[high;medium;low]")
	
	risktable[3][[1]] = as.character("Shaughnessy 70 Genes Sqrt. dist.")
	risktable[3][[2]] = as.character(shaughnessy$predicted.sqrt)
	risktable[3][[3]] = as.character("[high;medium;low]")

	risktable[4][[1]] = as.character("Shaughnessy 17 Genes Sqrt. dist.")
	risktable[4][[2]] = as.character(shaughnessy$predicted17)
	risktable[4][[3]] = as.character("[high;low]")

	risktable[5][[1]] = as.character("Bergsagel TC Klassifikation")
	risktable[5][[2]] = as.character(bergsagel)
	risktable[5][[3]] = as.character("[4p16;maf;6p21;11q13;d1;d1d2;d2;none]")

	risktable[6][[1]] = as.character("EC Klassifikation")
	risktable[6][[2]] = as.character(ec)
	risktable[6][[3]] = as.character("[11;12;21;22]")

	risktable[7][[1]] = as.character("GPI")
	risktable[7][[2]] = as.character(gpi)
	risktable[7][[3]] = as.character("[high;medium;low]")
}

geneHandler = function(h, ...) {
	genetable[1][[1]] = as.character("Cyclin D1")
	genetable[1][[2]] = as.character("208712_at")
	genetable[1][[3]] = as.character(cyclind1.signal)
	genetable[1][[4]] = as.character(cyclind1)
	genetable[1][[5]] = as.character(cyclind1.bmpc.signal)
	genetable[1][[6]] = as.character(round(p.cyclind1.bmpc/7*100, 1))
	genetable[1][[7]] = as.character(cyclind1.mmc.signal)
	genetable[1][[8]] = as.character(round(p.cyclind1.mmc/233*100, 1))

	genetable[2][[1]] = as.character("Cyclin D2")
	genetable[2][[2]] = as.character("200953_s_at")
	genetable[2][[3]] = as.character(cyclind2.signal)
	genetable[2][[4]] = as.character(cyclind2)
	genetable[2][[5]] = as.character(cyclind2.bmpc.signal)
	genetable[2][[6]] = as.character(round(p.cyclind2.bmpc/7*100, 1))
	genetable[2][[7]] = as.character(cyclind2.mmc.signal)
	genetable[2][[8]] = as.character(round(p.cyclind2.mmc/233*100, 1))

	genetable[3][[1]] = as.character("Cyclin D3")
	genetable[3][[2]] = as.character("201700_at")
	genetable[3][[3]] = as.character(cyclind3.signal)
	genetable[3][[4]] = as.character(cyclind3)
	genetable[3][[5]] = as.character(cyclind3.bmpc.signal)
	genetable[3][[6]] = as.character(round(p.cyclind3.bmpc/7*100, 1))
	genetable[3][[7]] = as.character(cyclind3.mmc.signal)
	genetable[3][[8]] = as.character(round(p.cyclind3.mmc/233*100, 1))

	genetable[4][[1]] = as.character("FGFR3")
	genetable[4][[2]] = as.character("204379_s_at")
	genetable[4][[3]] = as.character(fgfr3.signal)
	genetable[4][[4]] = as.character(fgfr3)
	genetable[4][[5]] = as.character(fgfr3.bmpc.signal)
	genetable[4][[6]] = as.character(round(p.fgfr3.bmpc/7*100, 1))
	genetable[4][[7]] = as.character(fgfr3.mmc.signal)
	genetable[4][[8]] = as.character(round(p.fgfr3.mmc/233*100, 1))

	genetable[5][[1]] = as.character("MAGEA1")
	genetable[5][[2]] = as.character("207325_x_at")
	genetable[5][[3]] = as.character(magea1.signal)
	genetable[5][[4]] = as.character(magea1)
	genetable[5][[5]] = as.character(magea1.bmpc.signal)
	genetable[5][[6]] = as.character(round(p.magea1.bmpc/7*100, 1))
	genetable[5][[7]] = as.character(magea1.mmc.signal)
	genetable[5][[8]] = as.character(round(p.magea1.mmc/233*100, 1))

	genetable[6][[1]] = as.character("MAGEA3")
	genetable[6][[2]] = as.character("209942_x_at")
	genetable[6][[3]] = as.character(magea3.signal)
	genetable[6][[4]] = as.character(magea3)
	genetable[6][[5]] = as.character(magea3.bmpc.signal)
	genetable[6][[6]] = as.character(round(p.magea3.bmpc/7*100, 1))
	genetable[6][[7]] = as.character(magea3.mmc.signal)
	genetable[6][[8]] = as.character(round(p.magea3.mmc/233*100, 1))

	genetable[7][[1]] = as.character("CTAG1")
	genetable[7][[2]] = as.character("210546_x_at")
	genetable[7][[3]] = as.character(ctag1.signal)
	genetable[7][[4]] = as.character(ctag1)
	genetable[7][[5]] = as.character(ctag1.bmpc.signal)
	genetable[7][[6]] = as.character(round(p.ctag1.bmpc/7*100, 1))
	genetable[7][[7]] = as.character(ctag1.mmc.signal)
	genetable[7][[8]] = as.character(round(p.ctag1.mmc/233*100, 1))

	genetable[8][[1]] = as.character("SSX2")
	genetable[8][[2]] = as.character("210497_x_at")
	genetable[8][[3]] = as.character(ssx2.signal)
	genetable[8][[4]] = as.character(ssx2)
	genetable[8][[5]] = as.character(ssx2.bmpc.signal)
	genetable[8][[6]] = as.character(round(p.ssx2.bmpc/7*100, 1))
	genetable[8][[7]] = as.character(ssx2.mmc.signal)
	genetable[8][[8]] = as.character(round(p.ssx2.mmc/233*100, 1))

	genetable[9][[1]] = as.character("HM1.24/BST2")
	genetable[9][[2]] = as.character("201641_at")
	genetable[9][[3]] = as.character(hm124.signal)
	genetable[9][[4]] = as.character(hm124)
	genetable[9][[5]] = as.character(hm124.bmpc.signal)
	genetable[9][[6]] = as.character(round(p.hm124.bmpc/7*100, 1))
	genetable[9][[7]] = as.character(hm124.mmc.signal)
	genetable[9][[8]] = as.character(round(p.hm124.mmc/233*100, 1))

	genetable[10][[1]] = as.character("MUC1")
	genetable[10][[2]] = as.character("213693_s_at")
	genetable[10][[3]] = as.character(muc1.signal)
	genetable[10][[4]] = as.character(muc1)
	genetable[10][[5]] = as.character(muc1.bmpc.signal)
	genetable[10][[6]] = as.character(round(p.muc1.bmpc/7*100, 1))
	genetable[10][[7]] = as.character(muc1.mmc.signal)
	genetable[10][[8]] = as.character(round(p.muc1.mmc/233*100, 1))

	genetable[11][[1]] = as.character("AURKA")
	genetable[11][[2]] = as.character("208079_s_at")
	genetable[11][[3]] = as.character(aurka.signal)
	genetable[11][[4]] = as.character(aurka)
	genetable[11][[5]] = as.character(aurka.bmpc.signal)
	genetable[11][[6]] = as.character(round(p.aurka.bmpc/7*100, 1))
	genetable[11][[7]] = as.character(aurka.mmc.signal)
	genetable[11][[8]] = as.character(round(p.aurka.mmc/233*100, 1))
}

# qualitätskontrolle
qctableHandler = function(h, ...) {
	qc.table[1][[1]] = as.character("scale.factors")
	qc.table[1][[2]] = as.character(yaqc@scale.factors)

	qc.table[2][[1]] = as.character("average.background")
	qc.table[2][[2]] = as.character(yaqc@average.background)

	qc.table[3][[1]] = as.character("average.noise")
	qc.table[3][[2]] = as.character(yaqc@average.noise)

	qc.table[4][[1]] = as.character("percent.present")
	qc.table[4][[2]] = as.character(yaqc@percent.present)

	for (i in seq(1:19)) {
		qc.table[i+4][[1]] = as.character(rownames(yaqc@morespikes)[i])
		qc.table[i+4][[2]] = as.character(yaqc@morespikes[i])
	}

	for (i in seq(1:6)) {
		qc.table[i+23][[1]] = as.character(rownames(yaqc@gcos.probes)[i])
		qc.table[i+23][[2]] = as.character(yaqc@gcos.probes[i])
	}

	for (i in seq(1:6)) {
		qc.table[i+29][[1]] = as.character(rownames(yaqc@bio.calls)[i])
		qc.table[i+29][[2]] = as.character(yaqc@bio.calls[i])
	}
}

# pdf erzeugen
pdfHandler = function(h, ...) {
	svalue(sb) = "PDF wird erzeugt ..."

	#  hier noch kontrolle einfügen ob alle nötigen eingabefelder ausgefüllt wurden!!!
	#  ansonsten warnhinweis ausgeben und abrechen

	#if (svalue(p.vorname)=="" | svalue(p.name)=="" | svalue(p.geb)=="" | svalue(p.strasse)=="" | 
	#    svalue(p.ort)=="" | svalue(p.plz)=="" | svalue(p.diag)=="" | svalue(p.igtype)=="" | 
	#    svalue(p.lk)=="" | svalue(p.sex)=="" | svalue(probe.date)=="" | svalue(probe.volume)=="" | 
	#    svalue(probe.protokoll)=="" | svalue(probe.rna)=="" | svalue(probe.array)=="" | svalue(probe.ampl)=="" | 
	#    svalue(probe.norm)=="")

	Sweave("scripts/befund.Rnw")
	system("R CMD pdflatex befund.tex")
	system(paste("pdftk befund.pdf output", svalue(cel.label), " compress"))
	system(paste("mv", svalue(cel.label), paste("reports/",svalue(cel.label), sep="")))
	enabled(file.pdfshow)="TRUE"  # button zum anzeigen des pdf aktivieren
	svalue(sb) = "PDF wurde erzeugt und kann nun angezeigt werden!"
}

# pdf mit acroread öffnen
viewpdfHandler = function(h, ...) {
	system(paste("acroread", paste("reports/",svalue(cel.label), sep="")))
}

# --------------------------------------------------------------------
# hautpfenster erzeugen
win = gwindow("Geneexpression Report", width=1024, height=768)
g = ggroup(horizontal=F, cont=win, expand=TRUE)

# toolbar
tbl = list(
	load = list(handler = loadHandler, icon="open"),
	save = list(handler = saveHandler, icon="save"), 
	quit = list(handler = quitHandler, icon="quit")
)
tb = gtoolbar(tbl, cont=g)

# paned group
pg = gpanedgroup(cont=g, expand=TRUE)

# notebook links
nb.left = gnotebook(cont=pg)
file = ggroup(horizontal=FALSE, cont=nb.left, label="GEP-Analyse")
patient = ggroup(horizontal=FALSE, cont=nb.left, label="Patient")
probe = ggroup(horizontal=FALSE, cont=nb.left, label="Probe")
befund = ggroup(horizontal=FALSE, cont=nb.left, label="Befund")
beurteilung = ggroup(horizontal=FALSE, cont=nb.left, label="Beurteilung")

# file 
tbl.file = glayout(cont=file)
tbl.file[1,1] = "CEL-File"
tbl.file[1,2] = (file.open = gbutton(text="Öffnen", border=TRUE, handler=chooseFile, cont=tbl.file))
tbl.file[2,1] = "Selected File:"
tbl.file[2,2] = (cel.label = glabel("", cont=tbl.file, editable=TRUE))
tbl.file[3,1] = ""
tbl.file[4,1] = ""
tbl.file[5,1] = "GEP Analyse"
tbl.file[5,2] = (file.analyse = gbutton(text="Starte Analyse", border=TRUE, handler=runAnalysis, cont=tbl.file))
tbl.file[6,1] = ""
tbl.file[7,1] = "" 
tbl.file[8,1] = "PDF"
tbl.file[8,2] = (file.pdfcreate = gbutton(text="PDF erstellen", border=TRUE, handler=pdfHandler, cont=tbl.file))
tbl.file[8,3] = (file.pdfshow = gbutton(text="PDF anzeigen", border=TRUE, handler=viewpdfHandler, cont=tbl.file))
enabled(file.analyse)="FALSE"
enabled(file.pdfcreate)="FALSE"
enabled(file.pdfshow)="FALSE"

# patient
tbl.patient = glayout(cont=patient)
tbl.patient[1,1] = "Name"
tbl.patient[1,2, expand=FALSE] = (p.name = gedit("", cont=tbl.patient))
tbl.patient[2,1] = "Vorname"
tbl.patient[2,2, expand=FALSE] = (p.vorname = gedit("", cont=tbl.patient))
tbl.patient[3,1] = "Geb.Datum"
tbl.patient[3,2, expand=FALSE] = (p.geb = gedit("", cont=tbl.patient))
tbl.patient[4,1] = "Straße"
tbl.patient[4,2, expand=FALSE] = (p.strasse = gedit("", cont=tbl.patient))
tbl.patient[5,1] = "Wohnort"
tbl.patient[5,2, expand=FALSE] = (p.ort = gedit("", cont=tbl.patient))
tbl.patient[6,1] = "Postleitzahl"
tbl.patient[6,2, expand=FALSE] = (p.plz = gedit("", cont=tbl.patient))
tbl.patient[7,1] = ""
tbl.patient[8,1] = ""
tbl.patient[9,1] = "Diagnose"
tbl.patient[9,2, expand=FALSE] = (p.diag = gdroplist(items=c("", "Multiples Myelom", "MGUS"), cont=tbl.patient))
tbl.patient[10,1] = "Ig-Typ"
tbl.patient[10,2, expand=FALSE] = (p.igtype = gdroplist(items=c("", "IgG", "IgA", "IgD", "BJ" ,"asekretorisch"), cont=tbl.patient))
tbl.patient[11,1] = "Leichtkette"
tbl.patient[11,2, expand=FALSE] = (p.lk = gdroplist(items=c("", "l", "k"), cont=tbl.patient))
tbl.patient[12,1] = "Geschlecht"
tbl.patient[12,2, expand=FALSE] = (p.sex = gdroplist(items=c("", "m", "f"), cont=tbl.patient))

# probe
tbl.probe = glayout(cont=probe)
tbl.probe[1,1] = "Datum der Probenentnahme"
tbl.probe[1,2, expand=FALSE] = (probe.date = gedit("", cont=tbl.probe))
tbl.probe[2,1] = "Probenvolumen"
tbl.probe[2,2, expand=FALSE] = (probe.volume = gedit("", cont=tbl.probe))
tbl.probe[2,3] = "ml"
tbl.probe[3,1] = "CD-138 Anreicherung"
tbl.probe[3,2, expand=FALSE] = (probe.protokoll = gedit("", cont=tbl.probe))
tbl.probe[3,3] = "%"
tbl.probe[4,1] = "Menge der verwendeten RNA"
tbl.probe[4,2] = (probe.rna = gedit("", cont=tbl.probe))
tbl.probe[4,3] = "ng"
tbl.probe[5,1] = "Array-Typ"
tbl.probe[5,2, expand=FALSE] = (probe.array = gdroplist(items=c("", "Affymetrix U133 plus 2.0", "Affymetrix U133A"), cont=tbl.probe))
tbl.probe[6,1] = "RNA Amplifikations-Protokoll"
tbl.probe[6,2, expand=FALSE] = (probe.ampl = gedit("", cont=tbl.probe))
tbl.probe[7,1] = "Normalisierung"
tbl.probe[7,2, expand=FALSE] = (probe.norm = gdroplist(items=c("", "RMA", "GC-RMA", "MAS5", "VSN"), cont=tbl.probe))

# befund
tbl.befund = glayout(cont=befund)
tbl.befund[1,1] = "Qualitätskontrolle"
tbl.befund[2,1] = (befund.qualitycontrol = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[3,1] = "Identitätskontrolle"
tbl.befund[4,1] = (befund.identitycontrol = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[5,1] = "Risikoklassifizierungen"
tbl.befund[6,1] = (befund.risk = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[7,1] = "Überexprimierte Gene"
tbl.befund[8,1] = (befund.genes = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[9,1] = "Zielantigene für Immuntherapie"
tbl.befund[10,1] = (befund.itherapy = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[11,1] = "Zielantigene für Gruppenspezifische Therapie"
tbl.befund[12,1] = (befund.grtherapy = gtext(width=400, height=80, cont=tbl.befund))
enabled(tbl.befund) = "FALSE"

# beuerteilung
tbl.beurteilung = glayout(cont=beurteilung)
tbl.beurteilung[1,1] = (beurteilung = gtext(width=400, height=250, cont=tbl.beurteilung))
enabled(tbl.beurteilung) = "FALSE"

# notebook rechts
nb.right = gnotebook(cont=pg)

qc = ggroup(horizontal=FALSE, cont=nb.right, label="QC-Plots")
gbutton(text="Zeige QC MAQC", border=TRUE, handler=dispHandlerMAQC, cont=qc)
gbutton(text="Zeige QC Stats", border=TRUE, handler=dispHandlerQCSTATS , cont=qc)
plot = gimage("data/default_empty.gif", cont=qc)
qc.table = gtable(data.frame(QC=rep("",35), Value="", stringsAsFactors=FALSE), cont=qc, expand=TRUE)
enabled(qc) = "FALSE"

tables = ggroup(horizontal=FALSE, cont=nb.right, label="Results")
ictable = gtable(data.frame(Geschlecht="", Typ="", Leichtkette="", stringsAsFactors=FALSE), cont=tables)
risktable = gtable(data.frame(Methode=rep("",7), Risk="", Bereich="", stringsAsFactors=FALSE), cont=tables, expand=TRUE)
genetable= gtable(data.frame(Gen=rep("",11), Probeset="", Pat.Sig.="", Pat.Call="", BMPC.Sig.="", BMPC.Call="", MM.Sig.="", MM.Call="", stringsAsFactors=FALSE), cont=tables, expand=TRUE)
enabled(tables) = "FALSE"

# statusbar
sb = gstatusbar("", cont=g)



# ------------------------------------------------------
# batch mode testing mit paralleler prozessierung

# cel.files ist vektor der länge n
# runAnalysis für jedes celfile[i]

# wichtiges todo: temporäre dateien eindeutig benennen!

batchHandler = function(h, ...) {
	library(snowfall)
	sfInit(parallel=TRUE, cpus=2)	
	sfLapply(cel.files, run.befund)
	sfStop()
}









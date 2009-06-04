require(gWidgets)
options("guiToolkit"="RGtk2")

# get system-information for systemspecific adaptions
# .. what about "not supported" systems?? stop from running the software... more comin soon...
system = Sys.info()[1]

# toolbar handler
quitHandler = function(h, ...) {
	dispose(win)
	# system("rm *.pdf *.log *.aux *.tex temp/*") # delete temporary files

	# delete files on the wd	
	if(file.exists("befund.aux")) {file.remove("befund.aux")}
	if(file.exists("befund.log")) {file.remove("befund.log")}
	if(file.exists("befund.tex")) {file.remove("befund.tex")}
	if(file.exists("befund.pdf")) {file.remove("befund.pdf")}

	# delet all files within temp
	file.remove(paste("temp/", dir("temp/"), sep=""))

	# todo: ask if really quit, ask to save
}

# report save handler
saveHandler = function(h, ...) {
	# save all cel-file associated variables in a list 
	# create a directory, copy imagages and r-object with variables there

	# create dir with name of celfile within the save directory
	if(!file.exists(paste("save/", svalue(cel.label), sep=""))) dir.create(paste("save/", svalue(cel.label), sep="")) 
	# copy .gif files from temp to the save folder, overwrite all existing files
	file.copy(paste("temp/", dir("temp/"), sep="")[grep("gif", paste("temp/", dir("temp/"), sep=""))], paste("save/", svalue(cel.label), sep=""), overwrite=T) 
	# copy .pdf files from temp to the save folder, overwrite all existing files
	file.copy(paste("temp/", dir("temp/"), sep="")[grep("pdf", paste("temp/", dir("temp/"), sep=""))], paste("save/", svalue(cel.label), sep=""), overwrite=T) 
	
	# save the "inputs"
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
	
	# save variables as a r-object with the ending *.befund
	tosave = c("save", "anzahl.bmpc", "anzahl.mmc", "bergsagel", "decaux", "ec", "gpi", "qc.obj",
		   "lightchain", "sex", "shaughnessy", "shrisk", "type", "nr.genes",
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


	save(file=paste("save/", svalue(cel.label), "/", svalue(cel.label), ".report", sep=""), list=tosave)

	svalue(sb) = paste("Report for CEL-File", svalue(cel.label), "was saved!", sep=" ")
}

# report load handler
loadHandler = function(h, ...) {
	# load the selected r-object,(befund) 
	# copy the save images to the temp folder

	              gfile(text="Please choose a report-object to load...", 
			    type="open", 
			    action = function(h, ...) {print(h)
						       load(h, envir=.GlobalEnv)
						       },
			    filter=list("report-objects"=list(patterns=c("*.report"))),
			    cont=file,			
			    handler = function(h, ...) {do.call(h$action, list(h$file))}
			    )	

	# set the values in the input boxes
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
	
	# copy images
	# copy .gif files from save to the temp folder, overwrite all existing files
	file.copy(paste("save/", svalue(cel.label), "/", dir(paste("save/", svalue(cel.label), sep="")), sep="")[grep("gif", paste("save/", dir(paste("save/", svalue(cel.label), sep="")), sep=""))], "temp/", overwrite=T) 
	# copy .pdf files from save to the temp folder, overwrite all existing files
	file.copy(paste("save/", svalue(cel.label), "/", dir(paste("save/", svalue(cel.label), sep="")), sep="")[grep("pdf", paste("save/", dir(paste("save/", svalue(cel.label), sep="")), sep=""))], "temp/", overwrite=T, recursiv=T) 

	# call handlers
	identHandler()			# printout ic, nb right
	riskHandler()			# printout risk, nb right
	geneHandler()			# printout genes, nb right
	qctableHandler()		# printout qc, right
	enabled(qc) = "TRUE"		# turn on qualitycontrol tab
	enabled(tables) = "TRUE"	# turn on tables tab
	enabled(tbl.befund) = "TRUE"	# turn on report tab
	enabled(tbl.beurteilung) = "TRUE" # turn on ind. report tabs
	enabled(file.pdfcreate)="TRUE"  # turn on create pdf button

	svalue(sb) = paste("Report for CEL-File", svalue(cel.label), "has been loaded!", sep=" ")
}

# open File Handler
chooseFile = function(h, ...) {
	              gfile(text="Please choose a CEL-File to load...", 
			    type="open", 
			    action = function(h, ...) {print(h)
						       cel.file = print(h)
						       assign("cel.file", cel.file, envir=.GlobalEnv)
						       svalue(cel.label) =  unlist((strsplit(cel.file, "/")))[length(unlist((strsplit(cel.file, "/"))))]
						       svalue(sb) = "The analysis can now be started!"
						       enabled(file.analyse)="TRUE" # enables run analysis button
						       },
			    filter=list("CEL-Files"=list(patterns=c("*.CEL"))),
			    cont=file,			
			    handler = function(h, ...) {do.call(h$action, list(h$file))}
	)
}

# start analyse handler
runAnalysis = function(h, ...) {
	svalue(sb) = "Analysis is running ... please be patient!"         # <<-- das will noch nicht sorecht, vermutlich ausglidern und vor ranAnalysis() ausführen
	source("scripts/befund.R")	# run the analysis
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
	svalue(sb) = "Analysis done & saved!"
	}

# diplay handler
dispHandlerMAQC = function(h, ...) {
	svalue(plot) = "temp/qualityplot.gif"
}
dispHandlerQCSTATS = function(h, ...) {
	svalue(plot) = "temp/qcsummary.gif"
}
dispHandlerNUSERLE = function(h, ...) {
	svalue(plot) = "temp/nuse_rle.gif"
}
dispHandlerARTIFACTS = function(h, ...) {
	svalue(plot) = "temp/artifacts.gif"
}
dispHandlerSPIKE = function(h, ...) {
	svalue(plot) = "temp/spikein_performance.gif"
}
dispHandlerDEGREDATION = function(h, ...) {
	svalue(plot) = "temp/degredation.gif"
}

# identity control
identHandler = function(h, ...) {
	ictable[1][[1]] = as.character(sex)
	ictable[1][[2]] = as.character(type)
	ictable[1][[3]] = as.character(lightchain)
	 
	# still some work to be done here!!!
	# ist vermutlich noch nicht optimal, was passiert wenn eingabe geändert wird, automatisches update??
	#if (sex==svalue(p.sex) & type==svalue(p.igtype) & lightchain==svalue(p.lk)) {svalue(befund.identitycontrol) = "Hinweis: Identitätskonrolle bestanden"}
	#if (sex!=svalue(p.sex) | type!=svalue(p.igtype) | lightchain!=svalue(p.lk)) {svalue(befund.identitycontrol) = "Hinweis: Identitätskonrolle nicht bestanden, bitte Eingaben überprüfen!"}
}

# risk stratification
riskHandler = function(h, ...) {
	risktable[1][[1]] = as.character("IFM 15-gene model")
	risktable[1][[2]] = as.character(decaux$decaux.risk)
	risktable[1][[3]] = as.character("[high;low]")

	#risktable[2][[1]] = as.character("Shaughnessy 70 Genes Abs. dist.")
	#risktable[2][[2]] = as.character(shaughnessy$predicted.abs)
	#risktable[2][[3]] = as.character("[high;medium;low]")
	
	risktable[2][[1]] = as.character("Shaughnessy 70-gene classifier")
	risktable[2][[2]] = as.character(shaughnessy$predicted.sqrt)
	risktable[2][[3]] = as.character("[high;medium;low]")

	risktable[3][[1]] = as.character("Shaughnessy 17-gene classifier")
	risktable[3][[2]] = as.character(shaughnessy$predicted17)
	risktable[3][[3]] = as.character("[high;low]")

	risktable[4][[1]] = as.character("Bergsagel TC classification")
	risktable[4][[2]] = as.character(bergsagel)[1]                                # just for the moment [1] bergsagel script hast to be checked!!!!!!
	risktable[4][[3]] = as.character("[4p16;maf;6p21;11q13;d1;d1d2;d2;none]")

	risktable[5][[1]] = as.character("Hose EC classification")
	risktable[5][[2]] = as.character(ec)
	risktable[5][[3]] = as.character("[11;12;21;22]")

	risktable[6][[1]] = as.character("Zahn molecular classification")
	risktable[6][[2]] = ""
	risktable[6][[3]] = ""

	risktable[7][[1]] = as.character("GPI")
	risktable[7][[2]] = as.character(gpi)
	risktable[7][[3]] = as.character("[high;medium;low]")
}

geneHandler = function(h, ...) {
	
	overexpression = function(sig.pat, call.pat, sig.bmpc, sd.bmpc, call.bmpc) {
		sig.value = ""
		if((sig.pat > (sig.bmpc+3*sd.bmpc)) & call.pat=="P" & call.bmpc!=0) {sig.value = "*** (up)"}
		if((sig.pat > (sig.bmpc+3*sd.bmpc)) & call.pat=="P" & call.bmpc==0) {sig.value = "*** (aberrant)"}
		if((sig.pat < (sig.bmpc-3*sd.bmpc)) & sig.bmpc>=6) {sig.value = "*** (down)"}	# <-- find a better cutoff for bmpc signal! this is not optimal yet!!
		#else {sig.value = ""}  <-- why doesnt that work ???
		return(sig.value)
	}

	genetable[1][[1]] = as.character("Cyclin D1")
	genetable[1][[2]] = as.character("208712_at")
	genetable[1][[3]] = as.character(cyclind1.signal)
	genetable[1][[4]] = as.character(cyclind1)
	genetable[1][[5]] = as.character(cyclind1.bmpc.signal)
	genetable[1][[6]] = as.character(round(p.cyclind1.bmpc/7*100, 1))
	genetable[1][[7]] = as.character(cyclind1.mmc.signal)
	genetable[1][[8]] = as.character(round(p.cyclind1.mmc/233*100, 1))
	genetable[1][[9]] = overexpression(cyclind1.signal, cyclind1, cyclind1.bmpc.signal, cyclind1.bmpc.sd, p.cyclind1.bmpc)

	genetable[2][[1]] = as.character("Cyclin D2")
	genetable[2][[2]] = as.character("200953_s_at")
	genetable[2][[3]] = as.character(cyclind2.signal)
	genetable[2][[4]] = as.character(cyclind2)
	genetable[2][[5]] = as.character(cyclind2.bmpc.signal)
	genetable[2][[6]] = as.character(round(p.cyclind2.bmpc/7*100, 1))
	genetable[2][[7]] = as.character(cyclind2.mmc.signal)
	genetable[2][[8]] = as.character(round(p.cyclind2.mmc/233*100, 1))
	genetable[2][[9]] = overexpression(cyclind2.signal, cyclind2, cyclind2.bmpc.signal, cyclind2.bmpc.sd, p.cyclind2.bmpc)

	genetable[3][[1]] = as.character("Cyclin D3")
	genetable[3][[2]] = as.character("201700_at")
	genetable[3][[3]] = as.character(cyclind3.signal)
	genetable[3][[4]] = as.character(cyclind3)
	genetable[3][[5]] = as.character(cyclind3.bmpc.signal)
	genetable[3][[6]] = as.character(round(p.cyclind3.bmpc/7*100, 1))
	genetable[3][[7]] = as.character(cyclind3.mmc.signal)
	genetable[3][[8]] = as.character(round(p.cyclind3.mmc/233*100, 1))
	genetable[3][[9]] = overexpression(cyclind3.signal, cyclind3, cyclind3.bmpc.signal, cyclind3.bmpc.sd, p.cyclind3.bmpc)

	genetable[4][[1]] = as.character("FGFR3")
	genetable[4][[2]] = as.character("204379_s_at")
	genetable[4][[3]] = as.character(fgfr3.signal)
	genetable[4][[4]] = as.character(fgfr3)
	genetable[4][[5]] = as.character(fgfr3.bmpc.signal)
	genetable[4][[6]] = as.character(round(p.fgfr3.bmpc/7*100, 1))
	genetable[4][[7]] = as.character(fgfr3.mmc.signal)
	genetable[4][[8]] = as.character(round(p.fgfr3.mmc/233*100, 1))
	genetable[4][[9]] = overexpression(fgfr3.signal, fgfr3, fgfr3.bmpc.signal, fgfr3.bmpc.sd, p.fgfr3.bmpc)

	genetable[5][[1]] = as.character("MMSET")
	genetable[5][[2]] = as.character("209053_s_at")
	genetable[5][[3]] = as.character(mmset.signal)
	genetable[5][[4]] = as.character(mmset)
	genetable[5][[5]] = as.character(mmset.bmpc.signal)
	genetable[5][[6]] = as.character(round(p.mmset.bmpc/7*100, 1))
	genetable[5][[7]] = as.character(mmset.mmc.signal)
	genetable[5][[8]] = as.character(round(p.mmset.mmc/233*100, 1))
	genetable[5][[9]] = overexpression(mmset.signal, mmset, mmset.bmpc.signal, mmset.bmpc.sd, p.mmset.bmpc)

	genetable[6][[1]] = as.character("MAGEA1")
	genetable[6][[2]] = as.character("207325_x_at")
	genetable[6][[3]] = as.character(magea1.signal)
	genetable[6][[4]] = as.character(magea1)
	genetable[6][[5]] = as.character(magea1.bmpc.signal)
	genetable[6][[6]] = as.character(round(p.magea1.bmpc/7*100, 1))
	genetable[6][[7]] = as.character(magea1.mmc.signal)
	genetable[6][[8]] = as.character(round(p.magea1.mmc/233*100, 1))
	genetable[6][[9]] = overexpression(magea1.signal, magea1, magea1.bmpc.signal, magea1.bmpc.sd, p.magea1.bmpc)

	genetable[7][[1]] = as.character("MAGEA3")
	genetable[7][[2]] = as.character("209942_x_at")
	genetable[7][[3]] = as.character(magea3.signal)
	genetable[7][[4]] = as.character(magea3)
	genetable[7][[5]] = as.character(magea3.bmpc.signal)
	genetable[7][[6]] = as.character(round(p.magea3.bmpc/7*100, 1))
	genetable[7][[7]] = as.character(magea3.mmc.signal)
	genetable[7][[8]] = as.character(round(p.magea3.mmc/233*100, 1))
	genetable[7][[9]] = overexpression(magea3.signal, magea3, magea3.bmpc.signal, magea3.bmpc.sd, p.magea3.bmpc)

	genetable[8][[1]] = as.character("CTAG1")
	genetable[8][[2]] = as.character("210546_x_at")
	genetable[8][[3]] = as.character(ctag1.signal)
	genetable[8][[4]] = as.character(ctag1)
	genetable[8][[5]] = as.character(ctag1.bmpc.signal)
	genetable[8][[6]] = as.character(round(p.ctag1.bmpc/7*100, 1))
	genetable[8][[7]] = as.character(ctag1.mmc.signal)
	genetable[8][[8]] = as.character(round(p.ctag1.mmc/233*100, 1))
	genetable[8][[9]] = overexpression(ctag1.signal, ctag1, ctag1.bmpc.signal, ctag1.bmpc.sd, p.ctag1.bmpc)

	genetable[9][[1]] = as.character("SSX2")
	genetable[9][[2]] = as.character("210497_x_at")
	genetable[9][[3]] = as.character(ssx2.signal)
	genetable[9][[4]] = as.character(ssx2)
	genetable[9][[5]] = as.character(ssx2.bmpc.signal)
	genetable[9][[6]] = as.character(round(p.ssx2.bmpc/7*100, 1))
	genetable[9][[7]] = as.character(ssx2.mmc.signal)
	genetable[9][[8]] = as.character(round(p.ssx2.mmc/233*100, 1))
	genetable[9][[9]] = overexpression(ssx2.signal, ssx2, ssx2.bmpc.signal, ssx2.bmpc.sd, p.ssx2.bmpc)

	genetable[10][[1]] = as.character("HM1.24/BST2")
	genetable[10][[2]] = as.character("201641_at")
	genetable[10][[3]] = as.character(hm124.signal)
	genetable[10][[4]] = as.character(hm124)
	genetable[10][[5]] = as.character(hm124.bmpc.signal)
	genetable[10][[6]] = as.character(round(p.hm124.bmpc/7*100, 1))
	genetable[10][[7]] = as.character(hm124.mmc.signal)
	genetable[10][[8]] = as.character(round(p.hm124.mmc/233*100, 1))
	genetable[10][[9]] = overexpression(hm124.signal, hm124, hm124.bmpc.signal, hm124.bmpc.sd, p.hm124.bmpc)

	genetable[11][[1]] = as.character("MUC1")
	genetable[11][[2]] = as.character("213693_s_at")
	genetable[11][[3]] = as.character(muc1.signal)
	genetable[11][[4]] = as.character(muc1)
	genetable[11][[5]] = as.character(muc1.bmpc.signal)
	genetable[11][[6]] = as.character(round(p.muc1.bmpc/7*100, 1))
	genetable[11][[7]] = as.character(muc1.mmc.signal)
	genetable[11][[8]] = as.character(round(p.muc1.mmc/233*100, 1))
	genetable[11][[9]] = overexpression(muc1.signal, muc1, muc1.bmpc.signal, muc1.bmpc.sd, p.muc1.bmpc)

	genetable[12][[1]] = as.character("AURKA")
	genetable[12][[2]] = as.character("208079_s_at")
	genetable[12][[3]] = as.character(aurka.signal)
	genetable[12][[4]] = as.character(aurka)
	genetable[12][[5]] = as.character(aurka.bmpc.signal)
	genetable[12][[6]] = as.character(round(p.aurka.bmpc/7*100, 1))
	genetable[12][[7]] = as.character(aurka.mmc.signal)
	genetable[12][[8]] = as.character(round(p.aurka.mmc/233*100, 1))
	genetable[12][[9]] = overexpression(aurka.signal, aurka, aurka.bmpc.signal, aurka.bmpc.sd, p.aurka.bmpc)
}

# qualitätskontrolle
qctableHandler = function(h, ...) {
	qc.table[1][[1]] = as.character("Average Background")
	qc.table[1][[2]] = as.character(log2(qc.obj@average.background))[7]

	qc.table[2][[1]] = as.character("Percent Present")
	qc.table[2][[2]] = as.character(qc.obj@percent.present)[7]

	for (i in seq(1:4)) {
		qc.table[i+2][[1]] = as.character(names(qc.obj@spikes[7,]))[i]
		qc.table[i+2][[2]] = as.character(qc.obj@spikes[7,])[i]
	}

	for (i in seq(1:6)) {
		qc.table[i+6][[1]] = as.character(names(qc.obj@qc.probes[7,]))[i]
		qc.table[i+6][[2]] = as.character(qc.obj@qc.probes[7,])[i]
	}

 	qc.table[13][[1]] = as.character("BioB Call")
	qc.table[13][[2]] = as.character(qc.obj@bioBCalls)[7]
}

# create the pdf
pdfHandler = function(h, ...) {
	svalue(sb) = "PDF will be created ..."

	# here to come more.... 

	#  hier noch kontrolle einfügen ob alle nötigen eingabefelder ausgefüllt wurden!!!
	#  ansonsten warnhinweis ausgeben und abrechen

	#if (svalue(p.vorname)=="" | svalue(p.name)=="" | svalue(p.geb)=="" | svalue(p.strasse)=="" | 
	#    svalue(p.ort)=="" | svalue(p.plz)=="" | svalue(p.diag)=="" | svalue(p.igtype)=="" | 
	#    svalue(p.lk)=="" | svalue(p.sex)=="" | svalue(probe.date)=="" | svalue(probe.volume)=="" | 
	#    svalue(probe.protokoll)=="" | svalue(probe.rna)=="" | svalue(probe.array)=="" | svalue(probe.ampl)=="" | 
	#    svalue(probe.norm)=="")

	Sweave("scripts/befund.Rnw")
	if(system=="Linux") {
		system("R CMD pdflatex befund.tex") # create pdf from tex file
		system(paste("pdftk befund.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk
		system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
		enabled(file.pdfshow)="TRUE"  # activate view pdf button
		svalue(sb) = "PDF was created and can now be shown!"
	}
	
	if(system=="Windows") {
		svalue(sb) = "Creating the PDF within windows is not yet supportet!"
	}	
}

# open pdf with acroread
viewpdfHandler = function(h, ...) {
	if(system=="Linux") {
		system(paste("acroread", paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep="")))
	}

	if(system=="Windows") {
		svalue(sb) = "Opening the PDF within windows is not yet supportet!"
	}
}

# --------------------------------------------------------------------
# create the main window
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

# notebook left
nb.left = gnotebook(cont=pg)
file = ggroup(horizontal=FALSE, cont=nb.left, label="GEP-Analysis")
patient = ggroup(horizontal=FALSE, cont=nb.left, label="Patient-Information")
probe = ggroup(horizontal=FALSE, cont=nb.left, label="Sample-Information")
befund = ggroup(horizontal=FALSE, cont=nb.left, label="Individual Comments")
beurteilung = ggroup(horizontal=FALSE, cont=nb.left, label="Report")

# file 
tbl.file = glayout(cont=file)
tbl.file[1,1] = "CEL-File"
tbl.file[1,2] = (file.open = gbutton(text="Open", border=TRUE, handler=chooseFile, cont=tbl.file))
tbl.file[2,1] = "Selected File:"
tbl.file[2,2] = (cel.label = glabel("", cont=tbl.file, editable=TRUE))
tbl.file[3,1] = ""
tbl.file[4,1] = ""
tbl.file[5,1] = "GEP Analysis"
tbl.file[5,2] = (file.analyse = gbutton(text="Run Analysis", border=TRUE, handler=runAnalysis, cont=tbl.file))
tbl.file[6,1] = ""
tbl.file[7,1] = "" 
tbl.file[8,1] = "PDF"
tbl.file[8,2] = (file.pdfcreate = gbutton(text="Create PDF", border=TRUE, handler=pdfHandler, cont=tbl.file))
tbl.file[8,3] = (file.pdfshow = gbutton(text="Show PDF", border=TRUE, handler=viewpdfHandler, cont=tbl.file))
enabled(file.analyse)="FALSE"
enabled(file.pdfcreate)="FALSE"
enabled(file.pdfshow)="FALSE"

# patient
tbl.patient = glayout(cont=patient)
tbl.patient[1,1] = "Name"
tbl.patient[1,2, expand=FALSE] = (p.name = gedit("", cont=tbl.patient))
tbl.patient[2,1] = "First Name"
tbl.patient[2,2, expand=FALSE] = (p.vorname = gedit("", cont=tbl.patient))
tbl.patient[3,1] = "Date of Birth"
tbl.patient[3,2, expand=FALSE] = (p.geb = gedit("", cont=tbl.patient))
tbl.patient[4,1] = "Street"
tbl.patient[4,2, expand=FALSE] = (p.strasse = gedit("", cont=tbl.patient))
tbl.patient[5,1] = "City"
tbl.patient[5,2, expand=FALSE] = (p.ort = gedit("", cont=tbl.patient))
tbl.patient[6,1] = "Zipcode"
tbl.patient[6,2, expand=FALSE] = (p.plz = gedit("", cont=tbl.patient))
tbl.patient[7,1] = ""
tbl.patient[8,1] = ""
tbl.patient[9,1] = "Diagnosis"
tbl.patient[9,2, expand=FALSE] = (p.diag = gdroplist(items=c("", "Multiple Myeloma", "MGUS"), cont=tbl.patient))
tbl.patient[10,1] = "Ig-Type"
tbl.patient[10,2, expand=FALSE] = (p.igtype = gdroplist(items=c("", "IgG", "IgA", "IgD", "BJ" ,"asecretory"), cont=tbl.patient))
tbl.patient[11,1] = "Lightchain"
tbl.patient[11,2, expand=FALSE] = (p.lk = gdroplist(items=c("", "l", "k"), cont=tbl.patient))
tbl.patient[12,1] = "Sex"
tbl.patient[12,2, expand=FALSE] = (p.sex = gdroplist(items=c("", "m", "f"), cont=tbl.patient))

# probe
tbl.probe = glayout(cont=probe)
tbl.probe[1,1] = "Date Sample was taken"
tbl.probe[1,2, expand=FALSE] = (probe.date = gedit("", cont=tbl.probe))
tbl.probe[2,1] = "Samplevolume"
tbl.probe[2,2, expand=FALSE] = (probe.volume = gedit("", cont=tbl.probe))
tbl.probe[2,3] = "ml"
tbl.probe[3,1] = "CD-138 Purification"
tbl.probe[3,2, expand=FALSE] = (probe.protokoll = gedit("", cont=tbl.probe))
tbl.probe[3,3] = "%"
tbl.probe[4,1] = "Amount of RNA used"
tbl.probe[4,2] = (probe.rna = gedit("", cont=tbl.probe))
tbl.probe[4,3] = "ng"
tbl.probe[5,1] = "Array-Type"
tbl.probe[5,2, expand=FALSE] = (probe.array = gdroplist(items=c("", "Affymetrix U133 plus 2.0"), cont=tbl.probe))
tbl.probe[6,1] = "RNA Purification Protokoll"
tbl.probe[6,2, expand=FALSE] = (probe.ampl = gedit("", cont=tbl.probe))
tbl.probe[7,1] = "Normalization Method"
tbl.probe[7,2, expand=FALSE] = (probe.norm = gdroplist(items=c("GC-RMA"), cont=tbl.probe))

# befund
tbl.befund = glayout(cont=befund)
tbl.befund[1,1] = "Qualitycontrol"
tbl.befund[2,1] = (befund.qualitycontrol = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[3,1] = "Identitycontrol"
tbl.befund[4,1] = (befund.identitycontrol = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[5,1] = "Risk Stratification"
tbl.befund[6,1] = (befund.risk = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[7,1] = "Overexpressed Genes"
tbl.befund[8,1] = (befund.genes = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[9,1] = "Targetgenes  for Immunotherapy"
tbl.befund[10,1] = (befund.itherapy = gtext(width=400, height=80, cont=tbl.befund))
tbl.befund[11,1] = "Targetgenes for Risk adapted Treatment"
tbl.befund[12,1] = (befund.grtherapy = gtext(width=400, height=80, cont=tbl.befund))
enabled(tbl.befund) = "FALSE"

# beuerteilung
tbl.beurteilung = glayout(cont=beurteilung)
tbl.beurteilung[1,1] = (beurteilung = gtext(width=400, height=250, cont=tbl.beurteilung))
enabled(tbl.beurteilung) = "FALSE"

# notebook right
nb.right = gnotebook(cont=pg)

qc = ggroup(horizontal=FALSE, cont=nb.right, label="QC-Plots")
tbl.qc = glayout(cont=qc)
tbl.qc[1,1] = gbutton(text="  Reproducability  ", border=TRUE, handler=dispHandlerMAQC, cont=tbl.qc)
tbl.qc[1,2] = gbutton(text="        Stats         ", border=TRUE, handler=dispHandlerQCSTATS, cont=tbl.qc)
tbl.qc[1,3] = gbutton(text=" Spike-in Performance ", border=TRUE, handler=dispHandlerSPIKE, cont=tbl.qc)
tbl.qc[2,1] = gbutton(text="       NUSE/RLE       ", border=TRUE, handler=dispHandlerNUSERLE, cont=tbl.qc)
tbl.qc[2,2] = gbutton(text="       Artifacts      ", border=TRUE, handler=dispHandlerARTIFACTS, cont=tbl.qc)
tbl.qc[2,3] = gbutton(text="   RNA Degredation    ", border=TRUE, handler=dispHandlerDEGREDATION, cont=tbl.qc)
plot = gimage("data/default_empty.gif", cont=qc)
qc.table = gtable(data.frame(QC=rep("",13), Value="", stringsAsFactors=FALSE), cont=qc, expand=TRUE)
enabled(qc) = "FALSE"

tables = ggroup(horizontal=FALSE, cont=nb.right, label="Results")
ictable = gtable(data.frame(Sex="", IG_Type="", Lightchain="", stringsAsFactors=FALSE), cont=tables)
risktable = gtable(data.frame(Method=rep("",7), Risk="", Range="", stringsAsFactors=FALSE), cont=tables, expand=TRUE)
genetable= gtable(data.frame(Gene=rep("",12), 
			     Probeset="", 
			     Pat.Sig.="", 
			     Pat.Call="", 
		             BMPC.Sig.="", 
		             BMPC.Call="", 
		             MM.Sig.="", 
	  	             MM.Call="", 
		             Sig.Overexpr.="", 
		             stringsAsFactors=FALSE), 
			     cont=tables, 
			     expand=TRUE)
enabled(tables) = "FALSE"

# statusbar
sb = gstatusbar("", cont=g)











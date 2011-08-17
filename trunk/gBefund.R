# ------------------------------------------------------------------------------------------------
#
# Options
#
# ------------------------------------------------------------------------------------------------

#lang 				default language for pdf report is english, you can also change to "german"
#					note: this changes only the language of the pdf, gui is always in english
#
#db.support			change to true for enabling psql database support, 
#					make sure the sql server is running and you have created the gepr and madb database!!!
#					note: works only within Linux
				  
#set.probe.ampl 	default "double amplification"; "single amplification"
#					still in development, so this is set to double until single is done..
#set.probe.array	"Affymetrix U133 plus 2.0", no other arrays supportet yet
#set.probe.norm 	"GC-RMA", no other normalization supported yet
#multicore 			default "no", "yes" only works on Linux systems

lang = "english"
db.support = FALSE
set.probe.ampl = "double amplification"
set.probe.array = "Affymetrix U133 plus 2.0"
set.probe.norm = "GC-RMA"
multicore = "no" # "yes"

system = as.vector(Sys.info()[1]) # what system is installed?

# ------------------------------------------------------------------------------------------------
#
# Load Libraries
#
# ------------------------------------------------------------------------------------------------
require(gWidgets)
options("guiToolkit"="RGtk2")

require(docval)
require(panp)
require(gdata)
require(affydata)
#require(MAQCsubsetAFX)
require(affyQCReport)
require(affyPLM)
require(simpleaffy)

# load packages for db-support and multicore support, note: works only within linux
if (system == "Linux" | system == "Darwin") {
	require(pgUtils)
	require(maDB)
	require(multicore)
} 	  

# ------------------------------------------------------------------------------------------------
#
# Analysis / Open / Save / Toolbar
#
# ------------------------------------------------------------------------------------------------

# toolbar handler
quitHandler = function(h, ...) {
	dispose(win)
	# system("rm *.pdf *.log *.aux *.tex temp/*") # delete temporary files

	# delete files on the wd	
	if(file.exists("befund.aux")) {file.remove("befund.aux")}
	if(file.exists("befund.log")) {file.remove("befund.log")}
	if(file.exists("befund.tex")) {file.remove("befund.tex")}
	if(file.exists("befund.pdf")) {file.remove("befund.pdf")}
	if(file.exists("befund_en.aux")) {file.remove("befund_en.aux")}
	if(file.exists("befund_en.log")) {file.remove("befund_en.log")}
	if(file.exists("befund_en.tex")) {file.remove("befund_en.tex")}
	if(file.exists("befund_en.pdf")) {file.remove("befund_en.pdf")}

	# delete all files within temp
	file.remove(paste("temp/", dir("temp/"), sep=""))

	# todo: ask if really quit, ask to save
}

# report save handler
saveHandler = function(h, ...) {
	# save all cel-file associated variables in a list 
	# create a directory, copy imagages and r-object with variables there

	if (system == "Linux" | system == "Darwin") {
		# create dir with name of celfile within the save directory
		# if(!file.exists(paste("save/", svalue(cel.label), sep=""))) dir.create(paste("save/", svalue(cel.label), sep="")) 
		if(!file.exists(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""))) dir.create(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")) # get rid of brackets within filenames
		# copy .gif files from temp to the save folder, overwrite all existing files
		# file.copy(paste("temp/", dir("temp/"), sep="")[grep("gif", paste("temp/", dir("temp/"), sep=""))], paste("save/", svalue(cel.label), sep=""), overwrite=T) 
		file.copy(paste("temp/", dir("temp/"), sep="")[grep("gif", paste("temp/", dir("temp/"), sep=""))], paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""), overwrite=T)
		# copy .png files from temp to the save folder, overwrite all existing files
		# file.copy(paste("temp/", dir("temp/"), sep="")[grep("png", paste("temp/", dir("temp/"), sep=""))], paste("save/", svalue(cel.label), sep=""), overwrite=T) 
		file.copy(paste("temp/", dir("temp/"), sep="")[grep("png", paste("temp/", dir("temp/"), sep=""))], paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""), overwrite=T) 
		# copy .pdf files from temp to the save folder, overwrite all existing files
		# file.copy(paste("temp/", dir("temp/"), sep="")[grep("pdf", paste("temp/", dir("temp/"), sep=""))], paste("save/", svalue(cel.label), sep=""), overwrite=T) 
		# file.copy(paste("temp/", dir("temp/"), sep="")[grep("pdf", paste("temp/", dir("temp/"), sep=""))], paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""), overwrite=T)
	}
	
	if (system == "Windows") {
		if(!file.exists(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""))) dir.create(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")) # get rid of brackets within filenames
		file.copy(paste("temp/", dir("temp/"), sep="")[grep("gif", paste("temp/", dir("temp/"), sep=""))], paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""), overwrite=T)
		file.copy(paste("temp/", dir("temp/"), sep="")[grep("png", paste("temp/", dir("temp/"), sep=""))], paste("save/", gsub("[()]" , "", svalue(cel.label)), sep=""), overwrite=T) 
	}
	
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
		p.stage = svalue(p.stage),
		p.datediag = svalue(p.datediag),
		p.country = svalue(p.country),
		p.iss = svalue(p.iss),
		phys.name = svalue(phys.name),
		phys.vorname = svalue(phys.vorname),
		phys.strasse = svalue(phys.strasse),
		phys.ort = svalue(phys.ort),
		phys.plz = svalue(phys.plz),	
		phys.country = svalue(phys.country),
		phys.title= svalue(phys.title),
		probe.date = svalue(probe.date),
		probe.volume = svalue(probe.volume),
		probe.protokoll = svalue(probe.protokoll),
		probe.purity.facs = svalue(probe.purity.facs),
		probe.purity.fish = svalue(probe.purity.fish),
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
		befund.grtherapy = svalue(befund.grtherapy),
		befund.classification = svalue(befund.classification),
		befund.cyto = svalue(befund.cyto)
	       )
	
	# save variables as a r-object with the ending *.report
	tosave = c("save", "quality.res", "cyto.res", "risk.res", "prediction.res", "genes.res", "process.res") 

	save(file=paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", gsub("[()]" , "", svalue(cel.label)), ".report", sep=""), list=tosave)

	svalue(sb) = paste("Report for CEL-File", svalue(cel.label), "was saved!", sep=" ")
	
	if(db.support == TRUE & (system == "Linux" | system == "Darwin")) {
		savedbHanlder()
		madbHandler()
	} # save values to database
}

# report load handler
loadHandler = function(h, ...) {
	# load the selected r-object,(befund) 
	# copy the save images to the temp folder

	gfile(text="Please choose a report-object to load...", 
		  type="open", 
		  action = function(h, ...) {
			print(h)
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
	svalue(p.stage) = save$p.stage
	svalue(p.datediag) = save$p.datediag
	svalue(p.country) = save$p.country
	#svalue(p.iss) = "" # workaround to get meta.score.function called if iss is the same as iss before...
	svalue(p.iss) = save$p.iss
	svalue(phys.name) = save$phys.name
	svalue(phys.vorname) = save$phys.vorname
	svalue(phys.strasse) = save$phys.strasse
	svalue(phys.ort) = save$phys.ort
	svalue(phys.plz) = save$phys.plz
	svalue(phys.title) = save$phys.title
	svalue(phys.country) = save$phys.country
	svalue(probe.date) = save$probe.date
	svalue(probe.volume) = save$probe.volume
	svalue(probe.protokoll) = save$probe.protokoll
	svalue(probe.rna) = save$probe.rna
	svalue(probe.array) = save$probe.array
	svalue(probe.ampl) = save$probe.ampl
	svalue(probe.norm) = save$probe.norm
	svalue(probe.purity.facs) = save$probe.purity.facs
	svalue(probe.purity.fish) = save$probe.purity.fish
	svalue(beurteilung) = save$beurteilung
	svalue(befund.qualitycontrol) = save$befund.qualitycontrol
	svalue(befund.identitycontrol) = save$befund.identitycontrol
	svalue(befund.risk) = save$befund.risk
	svalue(befund.genes) = save$befund.genes
	svalue(befund.itherapy) = save$befund.itherapy
	svalue(befund.grtherapy) = save$befund.grtherapy
	svalue(befund.classification) = save$befund.classification
	svalue(befund.cyto) = save$befund.cyto
	# svalue(sb) = save$sb <-- has to be checked some time...

	load("data/genes.Rdata", envir=.GlobalEnv) # load the reference genes for bmpc and mmc
	
	# copy images
	if (system == "Linux" | system == "Darwin") {
		# copy .gif files from save to the temp folder, overwrite all existing files
		file.copy(paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep="")[grep("gif", paste("save/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep=""))], "temp/", overwrite=T, recursiv=T) 
		# copy .pdf files from save to the temp folder, overwrite all existing files
		# file.copy(paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep="")[grep("pdf", paste("save/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep=""))], "temp/", overwrite=T, recursiv=T) 
		# copy .png files from save to the temp folder, overwrite all existing files
		file.copy(paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep="")[grep("png", paste("save/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep=""))], "temp/", overwrite=T, recursiv=T) 
	}
	
	if (system == "Windows") {
		file.copy(paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep="")[grep("gif", paste("save/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep=""))], "temp", overwrite=T, recursiv=T) 
		file.copy(paste("save/", gsub("[()]" , "", svalue(cel.label)), "/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep="")[grep("png", paste("save/", dir(paste("save/", gsub("[()]" , "", svalue(cel.label)), sep="")), sep=""))], "temp", overwrite=T, recursiv=T) 
	}

	# call handlers
	identHandler()			# printout ic, nb right
	riskHandler()			# printout risk, nb right
	geneHandler()			# printout genes, nb right
	qctableHandler()		# printout qc, right
	enabled(qc) = "TRUE"		# turn on qualitycontrol tab
	enabled(tables) = "TRUE"	# turn on tables tab
	enabled(befund) = "TRUE"	# turn on report tab
	enabled(tbl.beurteilung) = "TRUE" # turn on ind. report tabs
	enabled(file.integrity) = "TRUE"	# turn on button for integrity check
	enabled(file.ignore) = "TRUE"	# turn on the ignore integrity check button
	enabled(file.pdfcreate)="FALSE" # make sure create pdf button is turned off
	enabled(file.pdfshow)="FALSE"  	# make sure pdf viewing ist turned off
	enabled(file.analyse)="FALSE" 	# make sure runing the analysis button is turned off

	svalue(sb) = paste("Report for CEL-File", svalue(cel.label), "has been loaded!", sep=" ")
}

# open File Handler
chooseFile = function(h, ...) {
	gfile(text="Please choose a CEL-File to load...", 
	     type="open", 
	     action = function(h, ...) {
			print(h)
			cel.file = print(h)
			assign("cel.file", cel.file, envir=.GlobalEnv)
  	        clearGUIHandler() # clear all "old" entries within the gui
			if(system == "Linux" | system == "Darwin") {svalue(cel.label) =  unlist((strsplit(cel.file, "/")))[length(unlist((strsplit(cel.file, "/"))))]}
			if(system == "Windows" ) {svalue(cel.label) =  unlist((strsplit(cel.file, "\\", fixed=T)))[length(unlist((strsplit(cel.file, "\\", fixed=T))))]}
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
		
		source("scripts/befund.R")	# load the anaylsis functions
		external = ReadAffy(filenames=cel.file) # affy batch object of the sample cel file
		assign("external", external, envir=.GlobalEnv)
		
		process.res = gepr.process.external(external)
		assign("process.res", process.res, envir=.GlobalEnv)
		
		quality.res = gepr.qualitycontrol(external)
		assign("quality.res", quality.res, envir=.GlobalEnv)
		
		prediction.res = gepr.prediction()
		assign("prediction.res", prediction.res, envir=.GlobalEnv)
		
		genes.res = gepr.genes()
		assign("genes.res", genes.res, envir=.GlobalEnv)
		
		risk.res = gepr.risk()
		assign("risk.res", risk.res, envir=.GlobalEnv)
		
		cyto.res = gepr.cyto()		
		assign("cyto.res", cyto.res, envir=.GlobalEnv)
				
		identHandler()			# ausgabe ic, nb rechts
		riskHandler()			# ausgabe risk, nb rechts
		geneHandler()			# ausgabe gene, nb rechts
		qctableHandler()		# ausgabe qc, rechts
		enabled(qc) = "TRUE"		# qualitätskontrolle anzeige einschalten
		enabled(tables) = "TRUE"	# ergebnisse gene usw. einschalten..
		enabled(befund) = "TRUE"	# befund aktivieren
		enabled(tbl.beurteilung) = "TRUE" # beurteilung aktiveiren
		enabled(file.integrity)="TRUE"  # activate integrity check button
		enabled(file.ignore)="TRUE"	# activate ignore integrity check button
		saveHandler()			# speichern
		svalue(sb) = "Analysis done & saved!"
}

# clear info within the gui prior to loading a new cel-file or *.befund
clearGUIHandler = function(h, ...) {
	dispHandlerEMPTY() # set the qc window to the default empty plot

	# reset the values in the input boxes
	svalue(cel.label) = ""
	svalue(p.name) = ""
	svalue(p.vorname) = ""
	svalue(p.geb) = ""
	svalue(p.strasse) = ""
	svalue(p.ort) = ""
	svalue(p.plz) = ""
	svalue(p.diag) = ""
	svalue(p.igtype) = ""
	svalue(p.lk) = ""
	svalue(p.sex) = ""
	svalue(p.stage) = ""
	svalue(p.datediag) = ""
	svalue(p.country) = ""
	svalue(p.iss) = ""
	svalue(phys.name) = ""
	svalue(phys.vorname) = ""
	svalue(phys.strasse) = ""
	svalue(phys.ort) = ""
	svalue(phys.plz) = ""
	svalue(phys.title) = ""
	svalue(phys.country) = ""
	svalue(probe.date) = ""
	svalue(probe.volume) = ""
	svalue(probe.protokoll) = ""
	svalue(probe.rna) = ""
	# svalue(probe.array) = ""
	# svalue(probe.ampl) = ""
	# svalue(probe.norm) = ""
	svalue(probe.purity.facs) = ""
	svalue(probe.purity.fish) = ""
	svalue(beurteilung) = ""
	svalue(befund.qualitycontrol) = ""
	svalue(befund.identitycontrol) = ""
	svalue(befund.risk) = ""
	svalue(befund.genes) = ""
	svalue(befund.itherapy) = ""
	svalue(befund.grtherapy) = ""
	svalue(befund.classification) = ""
	svalue(sb) = ""	
}

# ------------------------------------------------------------------------------------------------
#
# Identity control
#
# ------------------------------------------------------------------------------------------------

# identity control
identHandler = function(h, ...) {
	ictable[1][[1]] = as.character(prediction.res[[1]])
	ictable[1][[2]] = as.character(prediction.res[[2]])
	ictable[1][[3]] = as.character(prediction.res[[3]])
}

# ------------------------------------------------------------------------------------------------
#
# Risk
#
# ------------------------------------------------------------------------------------------------
# risk stratification
riskHandler = function(h, ...) {
	#headers
	risktable[6][[1]] = as.character("Classification")
	risktable[1][[1]] = as.character("Risk stratification")
	risktable[10][[1]] = as.character("Cytogenetics")

	# molecular (classifications)
	risktable[8][[1]] = as.character("\tTC classification")
	risktable[8][[2]] = as.character(risk.res[[4]][1])                                # just for the moment [1] bergsagel script hast to be checked!!!!!!
	risktable[8][[3]] = as.character("[4p16;maf;6p21;11q13;d1;d1d2;d2;none]")

	risktable[9][[1]] = as.character("\tEC classification")
	risktable[9][[2]] = as.character(risk.res[[3]])
	risktable[9][[3]] = as.character("[11;12;21;22]")
	
	risktable[7][[1]] = as.character("\tMolecular classification")
	risktable[7][[2]] = as.character(risk.res[[2]])
	risktable[7][[3]] = as.character("[HP,CD1,CD2,PR,LB,MS,MF]")
	
	# risk stratifications
	# risktable[6][[1]] = as.character("\tUAMS 17-gene risk score")
	# risktable[6][[2]] = as.character(risk.res[[5]]$predicted17)
	# risktable[6][[3]] = as.character("[high;low]")
	
	risktable[3][[1]] = as.character("\tUAMS 70-gene risk score")
	risktable[3][[2]] = as.character(risk.res[[5]]$predicted.sub.sqrt) # squared distance as in publication, alternatively change to absolute distance --> predicted.sub.abs
	risktable[3][[3]] = as.character("[low;high]")
	risktable[3][[4]] = round(as.numeric(risk.res[[5]]$score),2)
	risktable[3][[5]] = as.character("[-0.833202182, 0.075642196, 1.395960868]")
	
	#risktable[2][[1]] = as.character("Shaughnessy 70 Genes Abs. dist.")
	#risktable[2][[2]] = as.character(shaughnessy$predicted.abs)
	#risktable[2][[3]] = as.character("[high;medium;low]")
	
	risktable[4][[1]] = as.character("\tGPI")
	risktable[4][[2]] = as.character(risk.res[[7]]$pi.risk)
	risktable[4][[3]] = as.character("[low;medium;high]")
	risktable[4][[4]] = round(as.numeric(risk.res[[7]]$pi.score),2)
	risktable[4][[5]] = as.character("[152.0118;316.4200]")
	
	risktable[2][[1]] = as.character("\tIFM 15-gene risk score")
	risktable[2][[2]] = as.character(risk.res[[6]]$decaux.risk)
	risktable[2][[3]] = as.character("[low;high]")
	risktable[2][[4]] = round(as.numeric(risk.res[[6]]$decaux.score),2)
	risktable[2][[5]] = as.character("[13.71279]")
	
	# meta score
	risktable[5][[1]] = as.character("\tHM-meta-score")
	#risktable[5][[2]] = ""
	#risktable[5][[3]] = ""
	risktable[5][[3]] = as.character("[low;medium;high]")
	risktable[5][[5]] = as.character("[0;0.5-3;>3]")

	#risktable[10][[1]] = as.character("\tZs score")
	#risktable[10][[2]] = as.character(risk.res[[1]])
	#risktable[10][[3]] = as.character("[high;medium;low]")

	risktable[11][[1]] = as.character("\tTranslocation t(4;14)")
	risktable[11][[2]] = as.character(cyto.res[[1]])
	risktable[11][[3]] = as.character("[yes;no]")
	

}

# ------------------------------------------------------------------------------------------------
#
# Genes
#
# ------------------------------------------------------------------------------------------------

geneHandler = function(h, ...) {
	overexpression = function(sig.pat, call.pat, sig.bmpc, sd.bmpc, call.bmpc) {
		sig.value = ""
		if((sig.pat > (sig.bmpc+3*sd.bmpc)) & call.pat=="P" & call.bmpc!=0) {sig.value = "up"} 
		if((sig.pat > (sig.bmpc+3*sd.bmpc)) & call.pat=="P" & call.bmpc==0) {sig.value = "aberrant"} 
		if((sig.pat < (sig.bmpc-3*sd.bmpc)) & sig.bmpc>=6) {sig.value = "down"}	# <-- find a better cutoff for bmpc signal! this is not optimal yet!!
		#else {sig.value = ""}
		return(sig.value)
	}
	
	#headers
	genetable[1][[1]] = as.character("Aberrantly-/overexpressed genes")
	genetable[7][[1]] = as.character("Immuntherapeutical genes")
	genetable[14][[1]] = as.character("Treatable targets")
	
	genetable[2][[1]] = as.character("\tCyclin D1")
	genetable[2][[2]] = as.character("208712_at")
	genetable[2][[3]] = as.character(genes.res[[4]])
	genetable[2][[4]] = as.character(genes.res[[3]])
	genetable[2][[5]] = as.character(cyclind1.bmpc.signal)
	genetable[2][[6]] = as.character(round(p.cyclind1.bmpc/10*100, 1))
	genetable[2][[7]] = as.character(cyclind1.mmc.signal)
	genetable[2][[8]] = as.character(round(p.cyclind1.mmc/262*100, 1))
	genetable[2][[9]] = overexpression(genes.res[[4]], genes.res[[3]], cyclind1.bmpc.signal, cyclind1.bmpc.sd, p.cyclind1.bmpc)

	genetable[3][[1]] = as.character("\tCyclin D2")
	genetable[3][[2]] = as.character("200953_s_at")
	genetable[3][[3]] = as.character(genes.res[[6]])
	genetable[3][[4]] = as.character(genes.res[[5]])
	genetable[3][[5]] = as.character(cyclind2.bmpc.signal)
	genetable[3][[6]] = as.character(round(p.cyclind2.bmpc/10*100, 1))
	genetable[3][[7]] = as.character(cyclind2.mmc.signal)
	genetable[3][[8]] = as.character(round(p.cyclind2.mmc/262*100, 1))
	genetable[3][[9]] = overexpression(genes.res[[6]], genes.res[[5]], cyclind2.bmpc.signal, cyclind2.bmpc.sd, p.cyclind2.bmpc)

	genetable[4][[1]] = as.character("\tCyclin D3")
	genetable[4][[2]] = as.character("201700_at")
	genetable[4][[3]] = as.character(genes.res[[8]])
	genetable[4][[4]] = as.character(genes.res[[7]])
	genetable[4][[5]] = as.character(cyclind3.bmpc.signal)
	genetable[4][[6]] = as.character(round(p.cyclind3.bmpc/10*100, 1))
	genetable[4][[7]] = as.character(cyclind3.mmc.signal)
	genetable[4][[8]] = as.character(round(p.cyclind3.mmc/262*100, 1))
	genetable[4][[9]] = overexpression(genes.res[[8]], genes.res[[7]], cyclind3.bmpc.signal, cyclind3.bmpc.sd, p.cyclind3.bmpc)

	genetable[5][[1]] = as.character("\tMMSET")
	genetable[5][[2]] = as.character("209053_s_at")
	genetable[5][[3]] = as.character(genes.res[[12]])
	genetable[5][[4]] = as.character(genes.res[[11]])
	genetable[5][[5]] = as.character(mmset.bmpc.signal)
	genetable[5][[6]] = as.character(round(p.mmset.bmpc/10*100, 1))
	genetable[5][[7]] = as.character(mmset.mmc.signal)
	genetable[5][[8]] = as.character(round(p.mmset.mmc/262*100, 1))
	genetable[5][[9]] = overexpression(genes.res[[12]], genes.res[[11]], mmset.bmpc.signal, mmset.bmpc.sd, p.mmset.bmpc)
	
	genetable[6][[1]] = as.character("\tTP53")
	genetable[6][[2]] = as.character("201746_at")
	genetable[6][[3]] = as.character(genes.res[[16]])
	genetable[6][[4]] = as.character(genes.res[[15]])
	genetable[6][[5]] = as.character(tp53.bmpc.signal)
	genetable[6][[6]] = as.character(round(p.tp53.bmpc/10*100, 1))
	genetable[6][[7]] = as.character(tp53.mmc.signal)
	genetable[6][[8]] = as.character(round(p.tp53.mmc/262*100, 1))
	genetable[6][[9]] = overexpression(genes.res[[16]], genes.res[[15]], tp53.bmpc.signal, tp53.bmpc.sd, p.tp53.bmpc)

	genetable[8][[1]] = as.character("\tCTAG1")
	genetable[8][[2]] = as.character("210546_x_at")
	genetable[8][[3]] = as.character(genes.res[[22]])
	genetable[8][[4]] = as.character(genes.res[[21]])
	genetable[8][[5]] = as.character(ctag1.bmpc.signal)
	genetable[8][[6]] = as.character(round(p.ctag1.bmpc/10*100, 1))
	genetable[8][[7]] = as.character(ctag1.mmc.signal)
	genetable[8][[8]] = as.character(round(p.ctag1.mmc/262*100, 1))
	genetable[8][[9]] = overexpression(genes.res[[22]], genes.res[[21]], ctag1.bmpc.signal, ctag1.bmpc.sd, p.ctag1.bmpc)
	
	genetable[9][[1]] = as.character("\tHM1.24/BST2")
	genetable[9][[2]] = as.character("201641_at")
	genetable[9][[3]] = as.character(genes.res[[26]])
	genetable[9][[4]] = as.character(genes.res[[25]])
	genetable[9][[5]] = as.character(hm124.bmpc.signal)
	genetable[9][[6]] = as.character(round(p.hm124.bmpc/10*100, 1))
	genetable[9][[7]] = as.character(hm124.mmc.signal)
	genetable[9][[8]] = as.character(round(p.hm124.mmc/262*100, 1))
	genetable[9][[9]] = overexpression(genes.res[[26]], genes.res[[25]], hm124.bmpc.signal, hm124.bmpc.sd, p.hm124.bmpc)

	genetable[10][[1]] = as.character("\tMAGEA1")
	genetable[10][[2]] = as.character("207325_x_at")
	genetable[10][[3]] = as.character(genes.res[[18]])
	genetable[10][[4]] = as.character(genes.res[[17]])
	genetable[10][[5]] = as.character(magea1.bmpc.signal)
	genetable[10][[6]] = as.character(round(p.magea1.bmpc/10*100, 1))
	genetable[10][[7]] = as.character(magea1.mmc.signal)
	genetable[10][[8]] = as.character(round(p.magea1.mmc/262*100, 1))
	genetable[10][[9]] = overexpression(genes.res[[18]], genes.res[[17]], magea1.bmpc.signal, magea1.bmpc.sd, p.magea1.bmpc)

	genetable[11][[1]] = as.character("\tMAGEA3")
	genetable[11][[2]] = as.character("209942_x_at")
	genetable[11][[3]] = as.character(genes.res[[20]])
	genetable[11][[4]] = as.character(genes.res[[19]])
	genetable[11][[5]] = as.character(magea3.bmpc.signal)
	genetable[11][[6]] = as.character(round(p.magea3.bmpc/10*100, 1))
	genetable[11][[7]] = as.character(magea3.mmc.signal)
	genetable[11][[8]] = as.character(round(p.magea3.mmc/262*100, 1))
	genetable[11][[9]] = overexpression(genes.res[[20]], genes.res[[19]], magea3.bmpc.signal, magea3.bmpc.sd, p.magea3.bmpc)

	genetable[12][[1]] = as.character("\tMUC1")
	genetable[12][[2]] = as.character("213693_s_at")
	genetable[12][[3]] = as.character(genes.res[[28]])
	genetable[12][[4]] = as.character(genes.res[[27]])
	genetable[12][[5]] = as.character(muc1.bmpc.signal)
	genetable[12][[6]] = as.character(round(p.muc1.bmpc/10*100, 1))
	genetable[12][[7]] = as.character(muc1.mmc.signal)
	genetable[12][[8]] = as.character(round(p.muc1.mmc/262*100, 1))
	genetable[12][[9]] = overexpression(genes.res[[28]], genes.res[[27]], muc1.bmpc.signal, muc1.bmpc.sd, p.muc1.bmpc)

	genetable[13][[1]] = as.character("\tSSX2")
	genetable[13][[2]] = as.character("210497_x_at")
	genetable[13][[3]] = as.character(genes.res[[24]])
	genetable[13][[4]] = as.character(genes.res[[23]])
	genetable[13][[5]] = as.character(ssx2.bmpc.signal)
	genetable[13][[6]] = as.character(round(p.ssx2.bmpc/10*100, 1))
	genetable[13][[7]] = as.character(ssx2.mmc.signal)
	genetable[13][[8]] = as.character(round(p.ssx2.mmc/262*100, 1))
	genetable[13][[9]] = overexpression(genes.res[[24]], genes.res[[23]], ssx2.bmpc.signal, ssx2.bmpc.sd, p.ssx2.bmpc)

	genetable[15][[1]] = as.character("\tAURKA")
	genetable[15][[2]] = as.character("208079_s_at")
	genetable[15][[3]] = as.character(genes.res[[2]])
	genetable[15][[4]] = as.character(genes.res[[1]])
	genetable[15][[5]] = as.character(aurka.bmpc.signal)
	genetable[15][[6]] = as.character(round(p.aurka.bmpc/10*100, 1))
	genetable[15][[7]] = as.character(aurka.mmc.signal)
	genetable[15][[8]] = as.character(round(p.aurka.mmc/262*100, 1))
	genetable[15][[9]] = overexpression(genes.res[[2]], genes.res[[1]], aurka.bmpc.signal, aurka.bmpc.sd, p.aurka.bmpc)
	
	genetable[16][[1]] = as.character("\tFGFR3")
	genetable[16][[2]] = as.character("204379_s_at")
	genetable[16][[3]] = as.character(genes.res[[10]])
	genetable[16][[4]] = as.character(genes.res[[9]])
	genetable[16][[5]] = as.character(fgfr3.bmpc.signal)
	genetable[16][[6]] = as.character(round(p.fgfr3.bmpc/10*100, 1))
	genetable[16][[7]] = as.character(fgfr3.mmc.signal)
	genetable[16][[8]] = as.character(round(p.fgfr3.mmc/262*100, 1))
	genetable[16][[9]] = overexpression(genes.res[[10]], genes.res[[9]], fgfr3.bmpc.signal, fgfr3.bmpc.sd, p.fgfr3.bmpc)

	genetable[17][[1]] = as.character("\tIGF1R")
	genetable[17][[2]] = as.character("225330_at")
	genetable[17][[3]] = as.character(genes.res[[14]])
	genetable[17][[4]] = as.character(genes.res[[13]])
	genetable[17][[5]] = as.character(igf1r.bmpc.signal)
	genetable[17][[6]] = as.character(round(p.igf1r.bmpc/10*100, 1))
	genetable[17][[7]] = as.character(igf1r.mmc.signal)
	genetable[17][[8]] = as.character(round(p.igf1r.mmc/262*100, 1))
	genetable[17][[9]] = overexpression(genes.res[[14]], genes.res[[13]], igf1r.bmpc.signal, igf1r.bmpc.sd, p.igf1r.bmpc)
}

# ------------------------------------------------------------------------------------------------
#
# QC
#
# ------------------------------------------------------------------------------------------------

# qualitycontrol
qctableHandler = function(h, ...) {
	qc.table[1][[1]] = as.character("Average Background")
	qc.table[1][[2]] = as.character(log2(quality.res@average.background))[7]

	qc.table[2][[1]] = as.character("Percent Present")
	qc.table[2][[2]] = as.character(quality.res@percent.present)[7]

	for (i in seq(1:4)) {
		qc.table[i+2][[1]] = as.character(names(quality.res@spikes[7,]))[i]
		qc.table[i+2][[2]] = as.character(quality.res@spikes[7,])[i]
	}

	for (i in seq(1:6)) {
		qc.table[i+6][[1]] = as.character(names(quality.res@qc.probes[7,]))[i]
		qc.table[i+6][[2]] = as.character(quality.res@qc.probes[7,])[i]
	}

 	qc.table[13][[1]] = as.character("BioB Call")
	qc.table[13][[2]] = as.character(quality.res@bioBCalls)[7]
	qc.table[14][[1]] = as.character("BioC Call")
	qc.table[14][[2]] = as.character(quality.res@bioCCalls)[7]
	qc.table[15][[1]] = as.character("BioD Call")
	qc.table[15][[2]] = as.character(quality.res@bioDCalls)[7]
	qc.table[16][[1]] = as.character("Cre Call")
	qc.table[16][[2]] = as.character(quality.res@creCalls)[7]
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
dispHandlerEMPTY = function(h, ...) {
	svalue(plot) = "data/default_empty.gif"
}

# ------------------------------------------------------------------------------------------------
#
# Integrity check
#
# ------------------------------------------------------------------------------------------------

integrityHandler = function(h, ...)  {
	dispose(warning.message)

	# check if the input-fields are not empty and if its the right input type
	# patient information
	error.count = 0
	if (svalue(p.name)=="") {
		insert(warning.message, "Please enter the name of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(p.vorname)=="") {
		insert(warning.message, "Please enter the first name of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.geb)=="") {
		insert(warning.message, "Please enter the birthday of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.strasse)=="") {
		insert(warning.message, "Please enter a street name", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.ort)=="") {
		insert(warning.message, "Please enter a city", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(p.country)=="") {
		insert(warning.message, "Please enter a country", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

		
	if (svalue(p.plz)=="") {
		insert(warning.message, "Please enter a zipcode", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.diag)=="") {
		insert(warning.message, "Please enter the diagnosis of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.datediag)=="") {
		insert(warning.message, "Please enter the date of first diagnosis", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.igtype)=="") {
		insert(warning.message, "Please enter the Ig-type of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(p.lk)=="") {
		insert(warning.message, "Please enter the lightchain type of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.sex)=="") {
		insert(warning.message, "Please enter the sex of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(p.iss)=="") {
		insert(warning.message, "Please enter the ISS stage of the patient", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.name)=="") {
		insert(warning.message, "Please enter the name of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.vorname)=="") {
		insert(warning.message, "Please enter the first name of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.strasse)=="") {
		insert(warning.message, "Please enter the street of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.ort)=="") {
		insert(warning.message, "Please enter the city of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.plz)=="") {
		insert(warning.message, "Please enter the zipcode of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(phys.title)=="") {
		insert(warning.message, "Please enter the title of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
		if (svalue(phys.country)=="") {
		insert(warning.message, "Please enter the country of the physician", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	# sample information
	if (svalue(probe.date)=="") {
		insert(warning.message, "Please enter the date of the BM-aspiration", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.volume)=="") {
		insert(warning.message, "Please enter the collected amount of samplevolume", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.protokoll)=="") {
		insert(warning.message, "Please enter cd-138 purification", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.purity.facs)=="") {
		insert(warning.message, "Please enter cd-138 purity (flow cytometry)", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}
	
	if (svalue(probe.purity.fish)=="") {
		insert(warning.message, "Please enter cd-138 purity (iFISH)", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.rna)=="") {
		insert(warning.message, "Please enter the amount rna that was used", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.array)=="") {
		insert(warning.message, "Please select the array type", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.ampl)=="") {
		insert(warning.message, "Please enter the purification protocoll used", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(probe.norm)=="") {
		insert(warning.message, "Please select the normalization mehtod used", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}	

	# are individual comments entered?
	if (svalue(befund.qualitycontrol)=="") {
		insert(warning.message, "Please comment on qualitycontrol", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}	

	if (svalue(befund.identitycontrol)=="") {
		insert(warning.message, "Please comment on identitycontrol", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(befund.risk)=="") {
		insert(warning.message, "Please comment on risk stratification", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(befund.genes)=="") {
		insert(warning.message, "Please comment on overexpressed genes", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(befund.itherapy)=="") {
		insert(warning.message, "Please comment on targetgenes for immunotherapy", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(befund.grtherapy)=="") {
		insert(warning.message, "Please comment on targetgenes for risk adapted treatment", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	# final report
	if (svalue(beurteilung)=="") {
		insert(warning.message, "Please enter your comments for the report", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	# identiycontrol
	if (svalue(p.igtype)!=as.character(prediction.res[[2]])) {
		insert(warning.message, "Ig-Type does not match with the predicted Ig-Type!", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.lk)!=as.character(prediction.res[[3]])) {
		insert(warning.message, "Lightchain does not match with the predicted Lightchain!", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}

	if (svalue(p.sex)!=as.character(prediction.res[[1]])) {
		insert(warning.message, "Sex does not match with the predicted Sex!", font.attr=c(foreground.colors="red"))
		error.count = error.count + 1
	}


	# if there are no more errors, go on and enable pdf-creation
	if (error.count > 0) {
		svalue(sb) = paste(error.count, "Error(s). See Warnings for Details!", sep=" ")	
	} else {
		svalue(sb) = "Integrity check passed! You may create the PDF now!"
		enabled(file.pdfcreate)="TRUE"  # aktivate pdf creation button
	}
}

ignoreHandler = function(h, ...) {
	enabled(file.pdfcreate)="TRUE"
}

# ------------------------------------------------------------------------------------------------
#
# PDF
#
# ------------------------------------------------------------------------------------------------

# create the pdf
pdfHandler = function(h, ...) {
	svalue(sb) = "PDF will be created ..."

	# Sweave("scripts/befund.Rnw") # german
	# Sweave("scripts/befund_en.Rnw")   # english
	if(system == "Linux") {
		if (lang=="english") {
			Sweave("scripts/befund_en.Rnw")
			system("R CMD pdflatex befund_en.tex") # create pdf from tex file, english
			system(paste("pdftk befund_en.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, english
			system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"
		}
		
		if (lang=="german") {
			Sweave("scripts/befund.Rnw")
			system("R CMD pdflatex befund.tex") # create pdf from tex file, german
			system(paste("pdftk befund.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, german
			system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"	
		}
	}
	
	if(system == "Darwin") {
		if (lang=="english") {
			Sweave("scripts/befund_en.Rnw")
			system("R CMD pdflatex befund_en.tex") # create pdf from tex file, english
			system(paste("pdftk befund_en.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, english
			system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), ".pdf",sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"
		}
		
		if (lang=="german") {
			Sweave("scripts/befund.Rnw")
			system("R CMD pdflatex befund.tex") # create pdf from tex file, german
			system(paste("pdftk befund.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, german
			system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"	
		}
	}
	
	if(system=="Windows") {
		if (lang=="english") {
			Sweave("scripts/befund_en.Rnw")
			shell("R CMD pdflatex befund_en.tex") # create pdf from tex file, english
			shell(paste("pdftk befund_en.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, english
			shell(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"
		}
		
		if (lang=="german") {
			Sweave("scripts/befund.Rnw")
			system("R CMD pdflatex befund.tex") # create pdf from tex file, german
			system(paste("pdftk befund.pdf output", gsub("[()]" , "", svalue(cel.label)), " compress")) # optimize file size using pdftk, german
			system(paste("mv", gsub("[()]" , "", svalue(cel.label)), paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep=""))) # move the pdf to the reports directory
			enabled(file.pdfshow)="TRUE"  # activate view pdf button
			svalue(sb) = "PDF was created and can now be viewed!"	
		}	
	}	
}

# open pdf with pdf viewer
viewpdfHandler = function(h, ...) {
	if(system=="Linux") {
		system(paste("evince", paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep="")))
	}
	
	if(system == "Darwin") {
		system(paste("open -a Preview", paste("reports/",gsub("[()]" , "", svalue(cel.label)), ".pdf",sep="")))
	}	

	if(system=="Windows") {
		system(paste("acroread", paste("reports/",gsub("[()]" , "", svalue(cel.label)), sep="")))
	}
}

# ------------------------------------------------------------------------------------------------
#
# DATABASE SUPPORT
#
# ------------------------------------------------------------------------------------------------
# this part ist still early development
# functions are not implemented in the gui yet!

# save info to dadabase
savedbHanlder = function(h, ...) {
	# open connection to the database
	con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="gepr")

	# set dateystyle to european format dd/mm/yyyy
	dbSendQuery(con, "SET datestyle to 'european'")
	
	# check if patient is already entered in the db, if yes just update the inforamtion
	result = dbSendQuery(con, "SELECT cel FROM celfile")
	if(svalue(cel.label) %in% dbGetResult(result)$cel) {
		updatedbHandler()
	} else {dbSendQuery(con, 
			    paste("INSERT INTO  celfile (cel, name, first_name, birth, street, city, zipcode, diagnosis, ig_type, lightchain, sex, sample_date, sample_volume, cd_138_purification, array_type, rna_purification_protocoll, normalization_method, qualitycontrol, identitycontrol, risk_stratification, overexpressed_genes, targetgenes_immunotherapy, targetgenes_risk_treatment, report) VALUES (", 
				  "'", svalue(cel.label), "', ", 
				  "'", svalue(p.name), "', ", 
				  "'", svalue(p.vorname), "', ", 
				  "'", svalue(p.geb), "', ", 
				  "'", svalue(p.strasse), "', ", 
				  "'", svalue(p.ort), "', ", 
				  "'", svalue(p.plz), "', ", 
				  "'", svalue(p.diag), "', ", 
				  "'", svalue(p.igtype), "', ", 
				  "'", svalue(p.lk), "', ", 
				  "'", svalue(p.sex), "', ", 
				  "'", svalue(probe.date), "', ", 
				  "'", svalue(probe.volume), "', ", 
				  "'", svalue(probe.protokoll), "', ", 
				  "'", svalue(probe.array), "', ", 
				  "'", svalue(probe.rna), "', ", 
				  "'", svalue(probe.norm), "', ", 
				  "'", svalue(befund.qualitycontrol), "', ", 
				  "'", svalue(befund.identitycontrol), "', ", 
				  "'", svalue(befund.risk), "', ", 
				  "'", svalue(befund.genes), "', ", 
				  "'", svalue(befund.itherapy), "', ", 
				  "'", svalue(befund.grtherapy), "', ", 
				  "'", svalue(beurteilung), "')",
			  	sep=""))
	}
	
	# disconnect the database
	dbDisconnect(con)
}

# update information within the database
updatedbHandler = function(h, ...) {
	# open connection to the database
	#require(pgUtils)
	con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="gepr")

	dbSendQuery(con, paste("UPDATE celfile SET name = '", svalue(p.name), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET first_name = '", svalue(p.vorname), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET birth = '", svalue(p.geb), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET street = '", svalue(p.strasse), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET city = '", svalue(p.ort), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET zipcode = '", svalue(p.plz), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET diagnosis = '", svalue(p.diag), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET ig_type = '", svalue(p.igtype), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET lightchain = '", svalue(p.lk), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET sex = '", svalue(p.sex), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET sample_date = '", svalue(probe.date), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET sample_volume = '", svalue(probe.volume), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET cd_138_purification = '", svalue(probe.protokoll), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET array_type = '", svalue(probe.array), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET rna_purification_protocoll = '", svalue(probe.rna), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET normalization_method = '", svalue(probe.norm), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET qualitycontrol = '", svalue(befund.qualitycontrol), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET identitycontrol = '", svalue(befund.identitycontrol), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET overexpressed_genes = '", svalue(befund.genes), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET targetgenes_immunotherapy = '", svalue(befund.itherapy), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET risk_stratification = '", svalue(befund.risk), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET targetgenes_risk_treatment = '", svalue(befund.grtherapy), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))
	dbSendQuery(con, paste("UPDATE celfile SET report = '", svalue(beurteilung), "'", " WHERE cel = '", svalue(cel.label), "'", sep=""))

	# not finished yet!!!
	
	# disconnect the database
	dbDisconnect(con)
}

# madb handler --> writes the geneexpression profile to the madb database
# this is experementell and not really working yet!!!!
madbHandler = function(h, ...) {
	
	source("scripts/madb_mod.R") # overwrites original publishToDB function and enables to write expr set to the same experiment eacht time, still neeeds extensive testing..
	# open connection to the database
	con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="madb")
	# setting loglevel to error
	log.level = "ERROR"
	
	result = dbSendQuery(con, "SELECT arrayname FROM arrays")
	
	if(svalue(cel.label) %in% dbGetResult(result)$arrayname) {
		dbDisconnect(con)
		print("Sample already in Database")
	} else {
	
		# this is a "dirty" trick.. getting the featureData from qc.data.norm.. definetly not optimal solution!
		madb.chip = new ("ExpressionSet", phenoData = phenoData(exprs.external.gcrma) , featureData = featureData(qc.data.norm), experimentData = experimentData(exprs.external.gcrma), annotation = annotation(exprs.external.gcrma),  assayData= assayData(exprs.external.gcrma))

		# converting to madb object
		madb.chip = newMadbSet(madb.chip)

		# sample description
		madb.sample = new("Sample", name=rownames(phenoData(madb.chip)@data), individual=svalue(p.diag), sex=svalue(p.sex)) 
		TheSamples = list(madb.sample)

		SigChannels = getSignalChannels(madb.chip)
		for (i in 1:length(SigChannels)) {
			SigChannels[[i]]@sample.index = i
		}

		publishToDB(madb.chip, con, exp.name="GEP-R", signal.channels=SigChannels, samples=TheSamples, preprocessing="gcRMA", v=FALSE)

		# disconnect the database
		dbDisconnect(con)
	}
}

# -------------------------------------------------------------------------
# function to caclulate the HM-Meta-Score
# -------------------------------------------------------------------------
meta.score = function(ISS, t414, GPI, SHRISK, DECAUX, AURKA, IGF1R) {
	a = ifelse(ISS == 1, 0, ifelse(ISS==2, 0.5, 1))
	
	b = ifelse(t414 == "no", 0, 1)
	
	c = ifelse(SHRISK == "high risk" & DECAUX == "high risk", 1, ifelse(SHRISK == "high risk" & DECAUX == "low risk", 0.5, ifelse(SHRISK == "low risk" & DECAUX == "high risk", 0.5, 0)))
	
	d = ifelse(GPI == "high risk", 1, ifelse(GPI == "medium risk", 0.5, 0))
	
	e = ifelse(AURKA == "A", 0, 1)
	
	f = ifelse(IGF1R == "A", 0, 1)
	
	meta = a+b+c+d
	if (e==1 | f==1) meta = meta+1

	meta.res = ifelse(meta==0, "low risk", ifelse(meta>0 & meta<=3, "medium risk", "high risk"))
	return(list(meta.res=meta.res, meta.value=meta))
}

# meta score handler
p.meta.score = function(h, ...) {
	if(exists("cyto.res") & exists("risk.res") & exists("genes.res")) {
		# meta score
		meta.sample = meta.score(as.numeric(svalue(h$obj)), cyto.res[[1]], risk.res[[7]]$pi.risk, risk.res[[5]]$predicted.sub.sqrt, risk.res[[6]]$decaux.risk,
								genes.res[[1]], genes.res[[13]])
	} else meta.sample = ""
	
	risktable[5][[2]] = as.character(meta.sample$meta.res)
	risktable[5][[4]] = as.numeric(meta.sample$meta.value)
	
	assign("p.meta.score", meta.sample, env=.GlobalEnv)
}

# ------------------------------------------------------------------------------------------------
#
# GUI
#
# ------------------------------------------------------------------------------------------------
# About-Dialog
aboutHandler = function(h, ...) {
	Dialog("GEP-R GUI Version 0.8 \n(C) Tobias Meißner, 2010 \n\nhttp://code.google.com/p/gep-r/")
}

Dialog = function(message, handler=NULL) { 						# this functino analog to the example in the gWidgets vignette
	window = gwindow("About")
	group = ggroup(container = window)
	gimage("info", dirname="stock", size="dialog", container=group)
	
	# message + buttons
	inner.group = ggroup(horizontal=FALSE, container = group)
	glabel(message, container=inner.group, expand=TRUE)
	
	# organzie the buttons
	button.group = ggroup(container=inner.group)
	addSpring(button.group) # push button to right
	gbutton("OK", handler=function(h, ...) dispose(window), container=button.group)
	return()
}

helpHandler = function(h, ...) {
	#window = gwindow("Help")
	#url = "http://cran.r-project.org/web/packages/gWidgets/index.html"
	#ghtml(url, container=window)
	if(system=="Linux" | system == "Darwin") {
		system("firefox http://code.google.com/p/gep-r/w/list")
	}
	if(system=="Windows") {
		shell("firefox http://code.google.com/p/gep-r/w/list")
	}
}



win = gwindow("Geneexpression Report", width=1024, height=768)
g = ggroup(horizontal=F, cont=win, expand=TRUE)

# menue 
aOpen = gaction(label="Open Report", icon="open", handler=loadHandler)
aSave = gaction(label="Save Report", icon="save", handler=saveHandler)

aQuit = gaction(label="Quit", icon="quit", handler=quitHandler)
aHelp = gaction(label="Help", handler=helpHandler)
aAbout = gaction(label="About", icon="info", handler=aboutHandler)

aGerman = gaction(label="German", handler=function(h, ...) {
	assign("lang", "german", envir=.GlobalEnv)
	svalue(sb) = "The PDF report will be created in German!"
})
aEnglish = gaction(label="English", handler=function(h, ...) {
	assign("lang", "english", envir=.GlobalEnv)
	svalue(sb) = "The PDF report will be created in English!"
})

ml  = list(File = list(open=aOpen, save=aSave, sep=list(separator=TRUE), quit=aQuit),
	   PDF_Language = list(German=aGerman, English=aEnglish),
	   Help = list(help=aHelp),
	   About = list(about=aAbout)
	   )
gmenu(ml, cont=g)


# paned group
pg = gpanedgroup(cont=g, expand=TRUE)

# notebook left
nb.left = gnotebook(cont=pg)
file = ggroup(horizontal=FALSE, cont=nb.left, label="GEP-Analysis")
patient = ggroup(horizontal=FALSE, use.scrollwindow=TRUE, cont=nb.left, label="General-Information")
probe = ggroup(horizontal=FALSE, cont=nb.left, use.scrollwindow=TRUE, label="Sample-Information")
befund = ggroup(horizontal=FALSE, cont=nb.left, use.scrollwindow=TRUE, label="Individual Comments")
beurteilung = ggroup(horizontal=FALSE, cont=nb.left, label="Comment")

# file
file.row.1 = glayout(horizontal="TRUE", cont=file)
file.row.1[1,1, anchor = c(-1,0)] = "CEL-File "
file.row.1[1,2, anchor = c(-1,1)] = (file.open = gbutton(text="Open", border=TRUE, handler=chooseFile, cont=file.row.1))

file.row.2 = glayout(horizontal="TRUE", cont=file)
file.row.2[1,1, anchor = c(-1,0)] = "Selected File: "
file.row.2[1,2] = (cel.label = glabel("", cont=file.row.2, editable=TRUE))
file.row.2[2,1] = ""
file.row.2[3,1] = ""

file.row.3 = glayout(horizontal="TRUE", cont=file)
file.row.3[1,1, anchor = c(-1,0)] = "GEP Analysis "
file.row.3[1,2] = (file.analyse = gbutton(text="Run Analysis", border=TRUE, handler=runAnalysis, cont=file.row.3))
file.row.3[2,1] = ""
file.row.3[3,1] = ""

file.row.4 = glayout(horizontal="TRUE", cont=file)
file.row.4[1,1, anchor = c(-1,0)] = "Integrity "
file.row.4[1,2] = (file.integrity = gbutton(text="Check", border=TRUE, handler=integrityHandler, cont=file.row.4))
file.row.4[1,3] = (file.ignore = gbutton(text="Ignore", border=TRUE, handler=ignoreHandler, cont=file.row.4))
file.row.4[2,1] = ""
file.row.4[2,1] = ""


file.row.5 = glayout(horizontal="TRUE", cont=file)
file.row.5[1,1, anchor = c(-1,0)] = "PDF "
file.row.5[1,2] = (file.pdfcreate = gbutton(text="Create PDF", border=TRUE, handler=pdfHandler, cont=file.row.5))
file.row.5[1,3] = (file.pdfshow = gbutton(text="Show PDF", border=TRUE, handler=viewpdfHandler, cont=file.row.5))

enabled(file.analyse)="FALSE"
enabled(file.integrity)="FALSE"
enabled(file.pdfcreate)="FALSE"
enabled(file.pdfshow)="FALSE"
enabled(file.ignore)="FALSE"

# patient
tbl.patient = glayout(cont=patient)
tbl.patient[1,1] = "Patient Information"
tbl.patient[2,1, anchor = c(-1,0)] = "Name / First name"
tbl.patient[2,2, expand=FALSE] = (p.name = gedit("", width=18, cont=tbl.patient))
tbl.patient[2,3, expand=FALSE] = (p.vorname = gedit("", width=18, cont=tbl.patient))
tbl.patient[3,1, anchor = c(-1,0)] = "Date of birth"
tbl.patient[3,2, expand=FALSE] = (p.geb = gedit("", width=18, cont=tbl.patient))
tbl.patient[4,1, anchor = c(-1,0)] = "Street"
tbl.patient[4,2:3, expand=FALSE] = (p.strasse = gedit("", width=18, cont=tbl.patient))
tbl.patient[5,1, anchor = c(-1,0)] = "City / Zip"
tbl.patient[5,2, expand=FALSE] = (p.ort = gedit("", width=18, cont=tbl.patient))
tbl.patient[5,3, expand=FALSE] = (p.plz = gedit("", width=18, cont=tbl.patient))
tbl.patient[6,1, anchor = c(-1,0)] = "Country"
tbl.patient[6,2, expand=FALSE] = (p.country = gedit("", width=18, cont=tbl.patient))
tbl.patient[7,1, anchor = c(-1,0)] = "Diagnosis & Stage"
tbl.patient[7,2, expand=FALSE] = (p.diag = gdroplist(items=c("", "Multiple Myeloma", "MGUS"), cont=tbl.patient))
tbl.patient[7,3, expand=FALSE] = (p.stage = gdroplist(items=c("", "IA", "IB", "IIA", "IIB", "IIIA", "IIIB"), cont=tbl.patient))
tbl.patient[8,1, anchor = c(-1,0)] = "Date of diagnosis"
tbl.patient[8,2, expand=FALSE] = (p.datediag = gedit("", width=18, cont=tbl.patient))
tbl.patient[9,1, anchor = c(-1,0)] = "IgH/IgL-type"
tbl.patient[9,2, expand=FALSE] = (p.igtype = gdroplist(items=c("", "G", "A", "D", "BJ" ,"asecretory"), cont=tbl.patient))
tbl.patient[9,3, expand=FALSE] = (p.lk = gdroplist(items=c("", "lambda", "kappa"), cont=tbl.patient))
tbl.patient[10,1, anchor = c(-1,0)] = "ISS"
tbl.patient[10,2, expand=FALSE] = (p.iss = gdroplist(items=c("", "1", "2", "3"), cont=tbl.patient, handler=p.meta.score))
tbl.patient[11,1, anchor = c(-1,0)] = "Sex"
tbl.patient[11,2, expand=FALSE] = (p.sex = gdroplist(items=c("", "male", "female"), cont=tbl.patient))
tbl.patient[12,1:3, anchor = c(-1,0)] = "Corresponding Physician"
tbl.patient[13,1, anchor = c(-1,0)] = "Title"
tbl.patient[13,2, expand=FALSE] = (phys.title = gedit("", width=18, cont=tbl.patient))
tbl.patient[14,1, anchor = c(-1,0)] = "Name / Firstname"
tbl.patient[14,2, expand=FALSE] = (phys.name = gedit("", width=18, cont=tbl.patient))
tbl.patient[14,3, expand=FALSE] = (phys.vorname = gedit("", width=18, cont=tbl.patient))
tbl.patient[15,1, anchor = c(-1,0)] = "Street"
tbl.patient[15,2:3, expand=FALSE] = (phys.strasse = gedit("", width=18, cont=tbl.patient))
tbl.patient[16,1, anchor = c(-1,0)] = "City / Zip"
tbl.patient[16,2, expand=FALSE] = (phys.ort = gedit("", width=18, cont=tbl.patient))
tbl.patient[16,3, expand=FALSE] = (phys.plz = gedit("", width=18, cont=tbl.patient))
tbl.patient[17,1, anchor = c(-1,0)] = "Country"
tbl.patient[17,2, expand=FALSE] = (phys.country = gedit("", width=18, cont=tbl.patient))


# probe
tbl.probe = glayout(cont=probe)
tbl.probe[1,1] = "Date BM-aspiration"
tbl.probe[1,2, expand=FALSE] = (probe.date = gedit("", cont=tbl.probe))
tbl.probe[2,1] = "Samplevolume"
tbl.probe[2,2, expand=FALSE] = (probe.volume = gedit("", cont=tbl.probe))
tbl.probe[2,3] = "ml"
tbl.probe[3,1] = "CD-138 purification"
tbl.probe[3,2, expand=FALSE] = (probe.protokoll = gedit("", cont=tbl.probe))
tbl.probe[4,1] = "CD-138 purity (flow cytometry)"
tbl.probe[4,2, expand=FALSE] = (probe.purity.facs = gedit("", cont=tbl.probe))
tbl.probe[4,3] = "%"
tbl.probe[5,1] = "CD-138 purity (iFISH)"
tbl.probe[5,2, expand=FALSE] = (probe.purity.fish = gedit("", cont=tbl.probe))
tbl.probe[5,3] = "%"
tbl.probe[6,1] = "Amount of RNA used"
tbl.probe[6,2] = (probe.rna = gedit("", cont=tbl.probe))
tbl.probe[6,3] = "ng"
tbl.probe[7,1] = "Array-Type"
tbl.probe[7,2, expand=FALSE] = (probe.array = gdroplist(items=c("", "Affymetrix U133 plus 2.0"), cont=tbl.probe))
tbl.probe[8,1] = "RNA Purification Protokoll"
# tbl.probe[8,2, expand=FALSE] = (probe.ampl = gdroplist(items=c("", "double amplification"), cont=tbl.probe))
tbl.probe[8,2, expand=FALSE] = (probe.ampl = gdroplist(items=c("", "single amplification", "double amplification"), cont=tbl.probe))
tbl.probe[9,1] = "Preprocessing Method"
tbl.probe[9,2, expand=FALSE] = (probe.norm = gdroplist(items=c("", "GC-RMA"), cont=tbl.probe))


###
svalue(probe.ampl) = set.probe.ampl # until single ampl. is fully integrated
svalue(probe.array) = set.probe.array
svalue(probe.norm) = set.probe.norm
###

# individual comments
# create the widgets
befund.qualitycontrol = gtext(width=400, height=60)
#befund.qualitycontrol.message = gtext(width=150, height=60)

befund.identitycontrol = gtext(width=400, height=60)
#befund.identitycontrol.message = gtext(width=150, height=60)

befund.genes = gtext(width=400, height=60)
#befund.genes.message = gtext(width=150, height=60)

befund.classification = gtext(width=400, height=60)
#befund.classification.message = gtext(width=150, height=60)

befund.risk = gtext(width=400, height=60)
#befund.risk.message = gtext(width=150, height=60)

befund.itherapy = gtext(width=400, height=60)
#befund.itherapy.message = gtext(width=150, height=60)

befund.grtherapy = gtext(width=400, height=60)
#befund.grtherapy.message = gtext(width=150, height=60)

befund.cyto = gtext(width=400, height=60)

# 1. row
befund.row.1 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Quality control", container=befund.row.1)
add(tmp, befund.qualitycontrol)
#tmp = gframe("", container=befund.row.1)
#add(tmp, befund.qualitycontrol.message)

# 2. row
befund.row.2 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Identity control", container=befund.row.2)
add(tmp, befund.identitycontrol)
#tmp = gframe("", container=befund.row.2)
#add(tmp, befund.identitycontrol.message)

# 3. row
befund.row.3 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Genes frequently over- or aberrantly expressed in multiple myeloma", container=befund.row.3)
add(tmp, befund.genes)
#tmp = gframe("", container=befund.row.3)
#add(tmp, befund.genes.message)

# 4. row
befund.row.4 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Classification of Myeloma", container=befund.row.4)
add(tmp, befund.classification)
#tmp = gframe("", container=befund.row.4)
#add(tmp, befund.classification.message)

# 5. row
befund.row.5 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Risk stratification", container=befund.row.5)
add(tmp, befund.risk)
#tmp = gframe("", container=befund.row.5)
#add(tmp, befund.risk.message)

# 6. row
befund.row.6 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Targets for immunotherapy", container=befund.row.6)
add(tmp, befund.itherapy)
#tmp = gframe("", container=befund.row.6)
#add(tmp, befund.itherapy.message)

# 7. row
befund.row.7 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Targets for individiualized treatment", container=befund.row.7)
add(tmp, befund.grtherapy)
#tmp = gframe("", container=befund.row.7)
#add(tmp, befund.grtherapy.message)

# 8. row
befund.row.8 = ggroup(horizontal="TRUE", cont=befund)
tmp = gframe("Chromosomal aberations", container=befund.row.8)
add(tmp, befund.cyto)

# disable comments on startup, disable message boxes
enabled(befund) = "FALSE"
#enabled(befund.qualitycontrol.message)="FALSE"
#enabled(befund.identitycontrol.message)="FALSE"
#enabled(befund.risk.message)="FALSE"
#enabled(befund.genes.message)="FALSE"
#enabled(befund.itherapy.message)="FALSE"
#enabled(befund.grtherapy.message)="FALSE"
#enabled(befund.classification.message)="FALSE"

# beuerteilung
tbl.beurteilung = glayout(cont=beurteilung)
tbl.beurteilung[1,1, expand=TRUE] = (beurteilung = gtext(width=450, height=350, cont=tbl.beurteilung))
enabled(tbl.beurteilung) = "FALSE"

# notebook right
nb.right = gnotebook(cont=pg)

tables = ggroup(horizontal=FALSE, cont=nb.right, label="Results")
ictable = gtable(data.frame(Sex="", IgH_type="", IgL_type="", stringsAsFactors=FALSE), cont=tables)
risktable = gtable(data.frame(Method=rep("",11), Risk="", Range="", Value="", Cutoff_or_Group_mean="", stringsAsFactors=FALSE), cont=tables, expand=TRUE)
genetable= gtable(data.frame(Gene=rep("",17), 
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

qc = ggroup(horizontal=FALSE, cont=nb.right, label="Quality control")
tbl.qc = glayout(cont=qc)
tbl.qc[1,1] = gbutton(text="  Reproducability  ", border=TRUE, handler=dispHandlerMAQC, cont=tbl.qc)
tbl.qc[1,2] = gbutton(text="        QC-Stats      ", border=TRUE, handler=dispHandlerQCSTATS, cont=tbl.qc)
tbl.qc[1,3] = gbutton(text=" Spike-in Performance ", border=TRUE, handler=dispHandlerSPIKE, cont=tbl.qc)
tbl.qc[2,1] = gbutton(text="       NUSE/RLE       ", border=TRUE, handler=dispHandlerNUSERLE, cont=tbl.qc)
tbl.qc[2,2] = gbutton(text="       Pseudo Images   ", border=TRUE, handler=dispHandlerARTIFACTS, cont=tbl.qc)
tbl.qc[2,3] = gbutton(text="   RNA Degradation    ", border=TRUE, handler=dispHandlerDEGREDATION, cont=tbl.qc)
plot = gimage("data/default_empty.gif", cont=qc)
qc.table = gtable(data.frame(QC=rep("",16), Value="", stringsAsFactors=FALSE), cont=qc, expand=TRUE)
enabled(qc) = "FALSE"

warnings = ggroup(horizontal=FALSE, cont=nb.right, label="Warnings")
warnings.message = (warning.message = gtext(width=300, height=500, cont=warnings))
enabled(warnings) = "FALSE" # just to output warning messages, text is not supossed to be editable

# get focus on firt tabs
svalue(nb.left) = 1
svalue(nb.right) = 1

# statusbar
sb = gstatusbar("", cont=g)



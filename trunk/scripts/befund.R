# -------------------------------------------------------------------------
# this script does the whole "computational" part of the gepr generation
# processing of the sample cel file
# qualitycontrol
# individual genes
# risk assessment
# -------------------------------------------------------------------------
load("data/affinity.info.hgu133plus2.Rdata") #affinity info of HGU133 Plus 2.0 Chip

# modified wrap.val.add function for parsing the affyinity info to bg.adjust.gcrma
my.wrap.val.add = function(abo,scale,affinity.info) {
	if(!exists("summarize.add")) source("./summarize_val.r")
   
    abo.bg  =  docval:::bg.adjust.gcrma.2(abo, affinity.info=affinity.info)
    abo.nrm =  docval:::normalize.qnt.add(abo.bg,scale$mqnts)
    eset    =  docval:::summarize.add(abo.nrm,scale$probe.effects)
   
    return(eset)
}

gepr.process.external = function(external) {
	# load gcrma reference data
	if (svalue(probe.ampl) == "single amplification") {
		load("xyz.Rdata") # there is no single amplification data available...
		params = sh.params
	}
	if (svalue(probe.ampl) == "double amplification") {
		load("data/params.befund.Rdata") # report reference gcrma parmameter 
	}

	# external patient preprocessing, windows and no multicore
	if(system=="Windows" | multicore=="no") {
	exprs.external.gcrma = wrap.val.add(external, params, method="gcrma")
	#exprs.external.gcrma = my.wrap.val.add(external, params, "affinity.info.hgu133plus2")
	exprs.external.mas5 = mas5(external)
	}

	# external patient preprocessing with multicore, mas5 and gcrma parallel
	if(system == "Linux" & multicore == "yes") {
	p = parallel(wrap.val.add(external, params, method="gcrma"))
	q = parallel(mas5(external))

	coll.erg = collect(list(p, q))

	exprs.external.gcrma = coll.erg[[1]]
	exprs.external.mas5 = coll.erg[[2]]

	rm(coll.erg)
	}

	nr.genes = dim(exprs(exprs.external.gcrma))[1] # number probesets on the chip

	# panp
	source("scripts/panp_docval_mod.R")
	panp.external = my.pa.calls(exprs.external.gcrma, verbose=TRUE)

	return(list(gcrma=exprs.external.gcrma, mas5=exprs.external.mas5, panp=panp.external, genes=nr.genes))
}

# -------------------------------------------------------------------------
# Qualitycontrol
# -------------------------------------------------------------------------
gepr.qualitycontrol = function(external) {
	if (svalue(probe.ampl) == "single amplification") {
		#load("data/xyzRdata")  # 
		# source("scripts/qc_gcrma.sh.R") # does not exist yet
		source("scripts/qc_gcrma.R") # load modifikation of qc() and RepPlot() to work with gcrma and for more performance
	}
	if (svalue(probe.ampl) == "double amplification") {
		load("data/qc.ref.Rdata") # load myeloma reference chips for qc
		source("scripts/qc_gcrma.R") # load modifikation of qc() and RepPlot() to work with gcrma and for more performance
	}
	# qc summary statistics
	qc.data = merge.AffyBatch(qc.ref, external) # combine sample + referece to affybatch object
	
	qc.data.norm = gcrma(qc.data, ,affinity.info=affinity.info.hgu133plus2) # gcrma 	
	qc.data.norm.panp = my.pa.calls(qc.data.norm, verbose=TRUE) # panp
	qc.obj = my.qc(qc.data, qc.data.norm, qc.data.norm.panp$Pcalls) # create qc object
	
	# qc plot
	png("temp/qcsummary.png")
	my.plot.qc.stats(qc.obj, usemid=T, main="", present.thresh=10, bg.thresh=20)
	dev.off()

	#  quality control metrics -- reproducibility plot
	png("temp/qualityplot.png")
	my.repplot(qc.data.norm)
	dev.off()

	# NUSE / RLE
	# detect which arrays have lower quality data
	Pset <- fitPLM(qc.data, background.method="GCRMA")
	png("temp/nuse_rle.png")
	par(mfrow=c(2,1), cex=0.5)
	RLE(Pset,col="lightblue",main="RLE", ylim=c(-3.5, 3.5))
	NUSE(Pset,col="yellow", main="NUSE", ylim=c(0.8, 1.5))
	dev.off()

	# check the "new" chip for possible artifacts, by plotting pseudo-images
	png("temp/artifacts.png", width=4800, height=4800, pointsize=108)
	par(mfrow=c(2,3))
	image(Pset, which=7, main="Weights") # pseudo-image of the weights
	image(Pset, which=7,type="resids", main="Residuals")
	image(Pset, which=7,type="pos.resids", main="Positive Rresiduals")
	image(Pset, which=7,type="neg.resids", main="Negative Residuals")
	image(Pset, which=7,type="sign.resids", main="Signed Residuals")
	dev.off()

	# performance of spike-in genes --> measure hybridization performance
	concentration = log(c(1.5, 5, 25, 100))
	x.val = array(concentration, c(4, length(qc.data)))
	x.val = t(x.val)
	y.val = simpleaffy:::spikeInProbes(qc.obj)

	png("temp/spikein_performance.png")
	plot(x.val, y.val, col=1:7, main="Spike-in performance", xlab="log ( concentration in pM)", ylab="log2 (expression)", ylim=c(4,16))
	legend(legend=sampleNames(qc.data), x=2.5, y=7, lty=1, col=1:7, cex=0.7)
	for (i in 1:length(qc.data)) {
		y.val = simpleaffy:::spikeInProbes(qc.obj)[i,]
		lm.spike = lm(y.val ~ concentration)
		slope = coef(lm.spike)[2]
		intercept = coef(lm.spike)[1]
		abline(intercept, slope, col=i)
	}
	dev.off()

	# rna degredation
	degredation = AffyRNAdeg(qc.data)
	png("temp/degredation.png")
	plotAffyRNAdeg(degredation, col=1:7, lty=1)
	legend(legend=sampleNames(qc.data), x=4.5, y=15, lty=1, col=1:7, cex=0.6)
	dev.off()

	# convert  *.pdf to *.gif for the imagehandler within the gui
	# convert  *.pdf to *.png for the creation of the pdf report
	if(system=="Linux") {
		system("convert temp/qualityplot.png temp/qualityplot.gif")
		system("convert temp/qcsummary.png temp/qcsummary.gif")
		system("convert temp/nuse_rle.png temp/nuse_rle.gif")
		system("convert -resize 480x480 temp/artifacts.png temp/artifacts.gif")
		system("convert temp/spikein_performance.png temp/spikein_performance.gif")
		system("convert temp/degredation.png temp/degredation.gif")
		system("convert temp/degredation.png temp/degredation.gif")

	#	system("convert temp/qualityplot.pdf temp/qualityplot.png")
	#	system("convert temp/qcsummary.pdf temp/qcsummary.png")
	#	system("convert temp/nuse_rle.pdf temp/nuse_rle.png")
	#	system("convert temp/artifacts.pdf temp/artifacts.png")
	#	system("convert temp/spikein_performance.pdf temp/spikein_performance.png")
	#	system("convert temp/degredation.pdf temp/degredation.png")
	}

	if(system=="Windows") {
		shell("convert temp/qualityplot.png temp/qualityplot.gif")
		shell("convert temp/qcsummary.png temp/qcsummary.gif")
		shell("convert temp/nuse_rle.png temp/nuse_rle.gif")
		shell("convert temp/artifacts.png temp/artifacts.gif")
		shell("convert temp/spikein_performance.png temp/spikein_performance.gif")
		shell("convert temp/degredation.png temp/degredation.gif")
		shell("convert temp/degredation.png temp/degredation.gif")
	}

	return(qc.obj)
}

# -------------------------------------------------------------------------
# predictions / identity control
# -------------------------------------------------------------------------
gepr.prediction = function() {
	# sex
	load("data/pam.sex.Rdata")
	sex = sig.sex(exprs(process.res$gcrma))
	sex  = ifelse(sex=="m", "male", "female")

	# type
	load("data/pam.type.Rdata")
	type = sig.type(exprs(process.res$gcrma))
	type  = ifelse(type=="IgA", "A", ifelse(type=="IgD", "D", "G"))

	# lightchain
	load("data/pam.lightchain.Rdata")
	lightchain = sig.lightchain(exprs(process.res$gcrma))
	lightchain = ifelse(lightchain=="k", "kappa", "lambda")
	
	return(list(sex, type, lightchain))
}

# -------------------------------------------------------------------------
# genes patient
# -------------------------------------------------------------------------
gepr.genes = function() {
	load("data/genes.Rdata", envir=.GlobalEnv) # load the reference genes for bmpc and mmc

	# targetgenes for group-specific therapie
	# aurorakinase A
	aurka = as.vector((process.res$panp$Pcalls["208079_s_at", ]))
	aurka.signal = as.numeric(round(exprs(process.res$gcrma)["208079_s_at", ],1))

	# within mm samples often overexpressed genes
	# Cyclin D1, D2, D3
	cyclind1 = as.vector((process.res$panp$Pcalls["208712_at", ]))
	cyclind1.signal = as.numeric(round(exprs(process.res$gcrma)["208712_at", ],1))
	cyclind2 = as.vector((process.res$panp$Pcalls["200953_s_at", ]))
	cyclind2.signal = as.numeric(round(exprs(process.res$gcrma)["200953_s_at", ],1))
	cyclind3 = as.vector((process.res$panp$Pcalls["201700_at", ]))
	cyclind3.signal = as.numeric(round(exprs(process.res$gcrma)["201700_at", ],1))
	# FGFR3
	fgfr3 = as.vector((process.res$panp$Pcalls["204379_s_at", ]))
	fgfr3.signal = as.numeric(round(exprs(process.res$gcrma)["204379_s_at", ],1))
	# whsc1/mmset
	mmset = as.vector((process.res$panp$Pcalls["209053_s_at", ]))
	mmset.signal = as.numeric(round(exprs(process.res$gcrma)["209053_s_at", ],1))
	# igf1r
	igf1r = as.vector((process.res$panp$Pcalls["225330_at", ]))
	igf1r.signal = as.numeric(round(exprs(process.res$gcrma)["225330_at", ],1))
	# t53
	tp53 = as.vector((process.res$panp$Pcalls["201746_at", ]))
	tp53.signal = as.numeric(round(exprs(process.res$gcrma)["201746_at", ],1))

	# target genes for imuntherapyh
	# MAGEA1
	magea1 = as.vector((process.res$panp$Pcalls["207325_x_at", ])) 
	magea1.signal = as.numeric(round(exprs(process.res$gcrma)["207325_x_at", ],1))
	# MAGEA3
	magea3 = as.vector((process.res$panp$Pcalls["209942_x_at", ]))
	magea3.signal = as.numeric(round(exprs(process.res$gcrma)["209942_x_at", ],1))
	# CTAG1
	ctag1 = as.vector((process.res$panp$Pcalls["210546_x_at", ]))
	ctag1.signal = as.numeric(round(exprs(process.res$gcrma)["210546_x_at", ],1))
	# SSX2
	ssx2 = as.vector((process.res$panp$Pcalls["210497_x_at", ]))
	ssx2.signal = as.numeric(round(exprs(process.res$gcrma)["210497_x_at", ],1))
	# HM1.24/BST2
	hm124 = as.vector((process.res$panp$Pcalls["201641_at", ]))
	hm124.signal = as.numeric(round(exprs(process.res$gcrma)["201641_at", ],1))
	# MUC1
	muc1 = as.vector((process.res$panp$Pcalls["213693_s_at", ]))
	muc1.signal = as.numeric(round(exprs(process.res$gcrma)["213693_s_at", ],1))
	
	return(list(aurka, aurka.signal, cyclind1, cyclind1.signal, cyclind2, cyclind2.signal, cyclind3, cyclind3.signal,
		   fgfr3, fgfr3.signal, mmset, mmset.signal, igf1r, igf1r.signal, tp53, tp53.signal, magea1, magea1.signal,
		   magea3, magea3.signal, ctag1, ctag1.signal, ssx2, ssx2.signal, hm124, hm124.signal, muc1, muc1.signal))
}

# -------------------------------------------------------------------------
# risk stratification & molecular classifications
# -------------------------------------------------------------------------
gepr.risk = function() {
	# gpi
	source("scripts/gpi.befund.R")
	gpi = gpi(process.res$gcrma, process.res$panp)
	# decaux
	source("scripts/decaux.R")
	decaux = decaux(process.res$gcrma)
	
	# shaughnessy 17/70 Gene Risk score
	source("scripts/shrisk.R")
	shaughnessy = shrisk(process.res$mas5) # needs mas5 normalized data
						  # returns this:
						  # shaughnessy$predicted.abs 	70 Gene abs distance
						  # shaughnessy$predicted.sqrt	70 Gene squared distcance
						  # shaughnessy$predicted17	17 Gene predictor

	# bergsagel tc classification
	#source("scripts/bergsagel.R")
	#bergsagel = bergsagel(process.res$mas5)
	
	# update 24.01.2011
	#  TC Classification according to 
	#  Chng et al. 2007
	source("scripts/tc_new.R")
	bergsagel = tc.class(exprs(process.res$mas5))

	# ec
	load("data/pam.ec.Rdata")
	ec = sig.ec(exprs(process.res$gcrma))

	# zhan molecular classification
	load("data/pam.sh_red.Rdata")
	genes.sh.mol = unique(read.csv2("data/shmol.txt", sep="", header=F, as.is=T)$V1)
	shmol = sig.sh(as.matrix(exprs(process.res$gcrma)[genes.sh.mol,]))

	# s_score + Zs_grouping
	#load("data/image_hm_bg.Rdata")
	#exprs.external.zscore = wrap.val.add(external, params.zscore, method="gcrma")
	#don = data.frame(exprs(exprs.external.zscore))
	#colnames(don) = cel.file
	#s = 0
	#for (j in 1:length(bad)) {
	#	s = s+as.numeric(don[bad[j],1])
	#}
	#for (j in 1:length(good)) {
	#	s = s-as.numeric(don[good[j],1])
	#}   
	#Zs = ifelse(s>232.4080,"high risk",ifelse(s<=162.2643,"low risk","medium risk"))
	Zs = "not supported yet"
	
	return(list(Zs, shmol, ec, bergsagel, shaughnessy, decaux, gpi))
}

# -------------------------------------------------------------------------
# prediction of chromosomal abberations
# -------------------------------------------------------------------------
gepr.cyto = function() {
	# translocation t(4;14)
	load("data/pam.t414.Rdata")
	t414 = sig.t414(exprs(process.res$gcrma))
	if(t414==1) {t414="yes"} else {t414 = "no"}

	return(t414)
}






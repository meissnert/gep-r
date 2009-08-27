# load required librarys
require(docval)
require(panp)
require(gdata)

# ---------------------------------------------------------------------------
# load gcrma reference data
load("data/params.befund.Rdata") # report reference gcrma parmameter params

# external patient preprocessing
external = ReadAffy(filenames=cel.file)
exprs.external.gcrma = wrap.val.add(external, params, method="gcrma")

nr.genes = dim(exprs(exprs.external.gcrma))[1] # number probesets on the chip

# panp
source("scripts/panp_docval_mod.R")
panp.external = my.pa.calls(exprs.external.gcrma, verbose=TRUE)

# mas5
exprs.external.mas5 = mas5(external)

# -------------------------------------------------------------------------
# Qualitycontrol
# -------------------------------------------------------------------------
require(affydata)
require(MAQCsubsetAFX)
require(affyQCReport)
require(affyPLM)
load("data/qc.ref.Rdata") # load myeloma reference chips for qc
source("scripts/qc_gcrma.R") # load modifikation of qc() and RepPlot() to work with gcrma and for more performance

# qc summary statistics
qc.data = merge.AffyBatch(qc.ref, external) # combine sample + referece to affybatch object
qc.data.norm = gcrma(qc.data) # gcrma 	
qc.data.norm.panp = my.pa.calls(qc.data.norm, verbose=TRUE) # panp
qc.obj = my.qc(qc.data, qc.data.norm, qc.data.norm.panp$Pcalls) # create qc object

# qc plot
pdf("temp/qcsummary.pdf")
my.plot.qc.stats(qc.obj, usemid=T, main="", present.thresh=10, bg.thresh=20)
dev.off()

#  quality control metrics -- reproducibility plot
pdf("temp/qualityplot.pdf")
my.repplot(qc.data.norm)
dev.off()

# NUSE / RLE
# detect which arrays have lower quality data
Pset <- fitPLM(qc.data, background.method="GCRMA")

pdf("temp/nuse_rle.pdf")
par(mfrow=c(2,1), cex=0.5)
RLE(Pset,col="lightblue",main="RLE", ylim=c(-3.5, 3.5))
NUSE(Pset,col="yellow", main="NUSE", ylim=c(0.8, 1.5))
dev.off()

# check the "new" chip for possible artifacts, by plotting pseudo-images
pdf("temp/artifacts.pdf")
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
y.val = spikeInProbes(qc.obj)

pdf("temp/spikein_performance.pdf")
plot(x.val, y.val, col=1:7, main="Spike-in performance", xlab="log ( concentration in pM)", ylab="log2 (expression)", ylim=c(4,16))
legend(legend=sampleNames(qc.data), x=2.5, y=7, lty=1, col=1:7, cex=0.7)
for (i in 1:length(qc.data)) {
	y.val = spikeInProbes(qc.obj)[i,]
 	lm.spike = lm(y.val ~ concentration)
	slope = coef(lm.spike)[2]
 	intercept = coef(lm.spike)[1]
	abline(intercept, slope, col=i)
}
dev.off()

# rna degredation
degredation = AffyRNAdeg(qc.data)
pdf("temp/degredation.pdf")
plotAffyRNAdeg(degredation, col=1:7, lty=1)
legend(legend=sampleNames(qc.data), x=4.5, y=15, lty=1, col=1:7, cex=0.6)
dev.off()

# convert  *.pdf to *.gif for the imagehandler within the gui
if(system=="Linux") {
	system("convert temp/qualityplot.pdf temp/qualityplot.gif")
	system("convert temp/qcsummary.pdf temp/qcsummary.gif")
	system("convert temp/nuse_rle.pdf temp/nuse_rle.gif")
	system("convert temp/artifacts.pdf temp/artifacts.gif")
	system("convert temp/spikein_performance.pdf temp/spikein_performance.gif")
	system("convert temp/degredation.pdf temp/degredation.gif")
}

if(system=="Windows") {
	# conversion within windows ...
	# imagemagic windows version has to be installed, more to come here soon....!!!
}

# -------------------------------------------------------------------------
# predictions / identity control
# -------------------------------------------------------------------------
# sex
load("data/pam.sex.Rdata")
sex = sig.sex(exprs(exprs.external.gcrma))

# type
load("data/pam.type.Rdata")
type = sig.type(exprs(exprs.external.gcrma))

# lightchain
load("data/pam.lightchain.Rdata")
lightchain = sig.lightchain(exprs(exprs.external.gcrma))

# -------------------------------------------------------------------------
# risk stratification & molecular classifications
# -------------------------------------------------------------------------
# gpi
source("scripts/gpi.befund.R")
gpi = gpi(exprs.external.gcrma, panp.external)


# -------------------------------------------------------------------------
# genes patient (just some samples yet..)
# -------------------------------------------------------------------------
# targetgenes for group-specific therapie
# aurorakinase A
aurka = as.vector((panp.external$Pcalls["208079_s_at", ]))
aurka.signal = as.numeric(round(exprs(exprs.external.gcrma)["208079_s_at", ],1))

# within mm samples often overexpressed genes
# Cyclin D1, D2, D3
cyclind1 = as.vector((panp.external$Pcalls["208712_at", ]))
cyclind1.signal = as.numeric(round(exprs(exprs.external.gcrma)["208712_at", ],1))
cyclind2 = as.vector((panp.external$Pcalls["200953_s_at", ]))
cyclind2.signal = as.numeric(round(exprs(exprs.external.gcrma)["200953_s_at", ],1))
cyclind3 = as.vector((panp.external$Pcalls["201700_at", ]))
cyclind3.signal = as.numeric(round(exprs(exprs.external.gcrma)["201700_at", ],1))
# FGFR3
fgfr3 = as.vector((panp.external$Pcalls["204379_s_at", ]))
fgfr3.signal = as.numeric(round(exprs(exprs.external.gcrma)["204379_s_at", ],1))
# whsc1/mmset
mmset = as.vector((panp.external$Pcalls["209053_s_at", ]))
mmset.signal = as.numeric(round(exprs(exprs.external.gcrma)["209053_s_at", ],1))

# target genes for imuntherapyh
# MAGEA1
magea1 = as.vector((panp.external$Pcalls["207325_x_at", ])) 
magea1.signal = as.numeric(round(exprs(exprs.external.gcrma)["207325_x_at", ],1))
# MAGEA3
magea3 = as.vector((panp.external$Pcalls["209942_x_at", ]))
magea3.signal = as.numeric(round(exprs(exprs.external.gcrma)["209942_x_at", ],1))
# CTAG1
ctag1 = as.vector((panp.external$Pcalls["210546_x_at", ]))
ctag1.signal = as.numeric(round(exprs(exprs.external.gcrma)["210546_x_at", ],1))
# SSX2
ssx2 = as.vector((panp.external$Pcalls["210497_x_at", ]))
ssx2.signal = as.numeric(round(exprs(exprs.external.gcrma)["210497_x_at", ],1))
# HM1.24/BST2
hm124 = as.vector((panp.external$Pcalls["201641_at", ]))
hm124.signal = as.numeric(round(exprs(exprs.external.gcrma)["201641_at", ],1))
# MUC1
muc1 = as.vector((panp.external$Pcalls["213693_s_at", ]))
muc1.signal = as.numeric(round(exprs(exprs.external.gcrma)["213693_s_at", ],1))

# -------------------------------------------------------------------------
# risk stratification & molecular classifications
# -------------------------------------------------------------------------
# decaux
source("scripts/decaux.R")
decaux = decaux(exprs.external.gcrma)

# shaughnessy 17/70 Gene Risk score
source("scripts/shrisk.R")
shaughnessy = shrisk(exprs.external.mas5) # needs mas5 normalized data
					  # returns this:
					  # shaughnessy$predicted.abs 	70 Gene abs distance
					  # shaughnessy$predicted.sqrt	70 Gene squared distcance
					  # shaughnessy$predicted17	17 Gene predictor

# bergsagel tc classification
source("scripts/bergsagel.R")
bergsagel = bergsagel(exprs.external.mas5)

# ec
load("data/pam.ec.Rdata")
ec = sig.ec(exprs(exprs.external.gcrma))

# shaughnessy molecular classification
# still to do...






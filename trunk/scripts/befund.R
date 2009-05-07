# tobias meißner
# 29.04.2009

# --------------------------------------------------------------------------
#
# Erstellung eines genexpressionsbasierenden Befundes
#
# -------------------------------------------------------------------------- 

# folgende Scores sollen für ein neues CEL-Files bestimmt werden:
#	GPI
#	.....
#
# basierend auf der docval modifikation mit gcrma von TM und TH
# basierund auf der panp modifikation von TM

# --------------------------------------------------------------------------
# Algorithmus:
# - einlesen de(s/r) CEL-File(s) 
# - normalisieren mittels docval-package zum Refrenz-Datensatz
# - presence/absence call mittels panp
# - mas5 normalilierung
# - qualitätskontrolle mittels yaqcaffy
# - aufrufen der einzelnen funtionen zur berechnung der risikoparameter
# - erstellen eines befundes mittels sweave/odfweave

#--------------------------------------------------------------------------- 
# TODO
# shaughnessy molecular classification
# angiogenese (Score??) + MRT ?
# code-optimierung und usability
# gibt es aufrufe die sich mittels snowfall optimieren/parallelisieren lassen?
# qualitätskontrolle optimieren!! braucht mit den meisten rechenaufwand!!
# .plotdiag() in repplot funktion
# yaqc = yaqc(tmp) durch qc() ersetzen?

# der hm2 datensatz, so wie er jetzt ist ist nicht optimal! --> korrigieren für batch-effekt!. reduzieren um chips welche den qualitätskontrollkriterien nicht genügen!

# known BUGS

# funktion run.befund() führt alles berechnungen durch
# run.befund = function(cel.file) {

# --------------------------------------------------------------------------
# libraries einbinden
require(docval)
require(panp)
require(gdata)
#library(monash) # ermöglicht das erstellen von figures mit möglichst wenig umrandung

# ---------------------------------------------------------------------------
# gcrma referenz daten laden
load("data/params.hm2.Rdata") # hm2 gcrma parmaeter params.hm2

# externen patient/en preprocessing
external = ReadAffy(filenames=cel.file)
exprs.external.gcrma = wrap.val.add(external, params.hm2, method="gcrma")

nr.genes = dim(exprs(exprs.external.gcrma))[1] # anzahl der transkripte

# panp
source("scripts/panp_docval_mod.R")
panp.external = my.pa.calls(exprs.external.gcrma, verbose=TRUE)

# mas5
exprs.external.mas5 = mas5(external)

# -------------------------------------------------------------------------
# Qualitätskontrolle
# -------------------------------------------------------------------------
require(yaqcaffy) # kommt weg ...
require(affydata)
require(MAQCsubsetAFX)
require(affyQCReport)
data(refA) # ersetzen durch MM-Referenzen
source("scripts/qc_gcrma.R") # modifikationen von qc() und RepPlot() für gcrma und performance

# phenoData anpassen, sonst meckert qc(), irgendwie so...
tmp = external
tmp@phenoData@data = refA@phenoData@data[1,]

cel.file.temp = cel.file
last.temp = length(unlist((strsplit(cel.file.temp, "/"))))
cel.file.temp2 = unlist((strsplit(cel.file.temp, "/")))[last.temp]

rownames(tmp@phenoData@data) = cel.file.temp2

# qc summary statistics
qc.data = merge.AffyBatch(refA, tmp) # sample + referenz als affybatch zusammenfassen
qc.data.norm = gcrma(qc.data) # gcrma 	
qc.data.norm.panp = my.pa.calls(qc.data.norm, verbose=TRUE) # panp
qc.obj = my.qc(qc.data, qc.data.norm, qc.data.norm.panp$Pcalls) # qc objekt erstellen

# qc plot
pdf("temp/qcsummary.pdf")
my.plot.qc.stats(qc.obj, usemid=T, main="", present.thresh=50, bg.thresh=100)
dev.off()

#  quality control metrics -- reproducibility plot
pdf("temp/qualityplot.pdf")
my.repplot(qc.data.norm)
dev.off()

# kommt weg...
yaqc = yaqc(tmp) # durch qc() ersetzen!!!

# konvertierung der *.pdf zu *.gif für den imagehandler im gui
system("convert temp/qualityplot.pdf temp/qualityplot.gif")
system("convert temp/qcsummary.pdf temp/qcsummary.gif")

rm(tmp, cel.file.temp, last.temp)
# -------------------------------------------------------------------------
# prädiktionen / identitätskontrolle
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
# berechnen der risiko parameter
# -------------------------------------------------------------------------
# gpi
source("scripts/gpi.befund.R")
gpi = gpi(exprs.external.gcrma, panp.external)


# -------------------------------------------------------------------------
# Gene Patient
# -------------------------------------------------------------------------
# zielgene für gruppenspezifische therapie
# aurorakinase A
aurka = as.vector((panp.external$Pcalls["208079_s_at", ]))
aurka.signal = as.numeric(round(exprs(exprs.external.gcrma)["208079_s_at", ],1))

# beim multiplen myelom häufig überexprimierte gene
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

# zielgene für immuntherapie
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
# Gene MMC / BMPC
# -------------------------------------------------------------------------
load("data/genes.Rdata")

# -------------------------------------------------------------------------
# berechnen der risiko klassifikationen
# -------------------------------------------------------------------------
# decaux
source("scripts/decaux.R")
decaux = decaux(exprs.external.gcrma)

# shaughnessy 17/70 Gene Risk score
source("scripts/shrisk.R")
shaughnessy = shrisk(exprs.external.mas5) # benötigt mas5 normalisierte daten als übergabe
					  # liefert liste mit 3 Rückgaben:
					  # shaughnessy$predicted.abs 	70 Gene abs distance
					  # shaughnessy$predicted.sqrt	70 Gene squared distcance
					  # shaughnessy$predicted17	17 Gene Prediktor

# bergsagel tc classification
source("scripts/bergsagel.R")
bergsagel = bergsagel(exprs.external.mas5)

# ec
load("data/pam.ec.Rdata")
ec = sig.ec(exprs(exprs.external.gcrma))

# shaughnessy molecular classification
# to do...



#} # ende run.befund()




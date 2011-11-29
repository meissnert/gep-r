# tobias meißner
# 02.12.08

# GPI for gep-r
gpi = function(exprs, panp) {
	# Einlesen der Proliferationsindex Probesets
	genes = read.csv2("data/probe.list.perm.csv")[,2]
	genes <- substr(as.character(genes),3,100)
	load("data/prolif_genes.Rdata")  # 50 Probesets

	# GEP MM Patienten / PI genes
	gep = exprs(exprs)[genes.ab,]

	# beruecksichtige PANP call
	pa.call = panp$Pcalls[genes.ab,]

	pa.call[pa.call == "P"] = 1
	pa.call[pa.call %in% c("A","M")] = 0

	# not present -> expression is considered zero
	gep.panp = gep * as.numeric(pa.call)

	pi.score = sum(gep.panp)

	pi.risk = ifelse(pi.score<=152.0118, "low risk", ifelse(pi.score>152.0118 & pi.score<=316.4200, "medium risk", "high risk"))
	return(list(pi.risk=pi.risk, pi.score=pi.score))
}

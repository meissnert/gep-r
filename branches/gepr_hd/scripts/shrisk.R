# 	Prediction algorithm description
# 	Shaughnessy, et al, 2007, Blood

#	Using a single new U133Plus2.0 sample, compute a 70 GEP Risk Score and Class as
#	follows:
#1) Normalize: process and normalize U133Plus2.0 samples using MAS5.0 to a scale
#	factor of 500. If a different scale factor was used, you may rescale using
#	log2(AvgDiff) + log2(YourScale) – log2(500), then keep the log2 scale result and
#	skip step 2 below.
#2) Log2: compute the log base 2 transformation of the AvgDiff expression measures.
#3) 70 probe sets: extract the expression measures for the 70 probe sets listed in the
#	enclosing spreadsheet.
#4) Center probe sets using the training set expression averages: subtract the average
#	expression measures over the 351 (training) samples from the new sample’s
#	expression measure for a given probe set.
#5) Use the first 6 principal components (PC) of the log base 2 expression measures
#	over the 351 samples to compute 6 weighted averages (i.e. PC rotated expression
#	measures) by taking the vector products of the PC rotation coefficients and the
#	new sample’s log2, centered expression measures.
#6) Compute a risk score for the new sample as the vector product of the PH
#	coefficients and the 6 weighted averages computed in step 5).
#7) Classify the sample according to the group mean risk score to which the sample’s
#	risk score is closest in squared distance (or absolute distance, since the K-means
#	classifier is based upon the univariate risk score).

shrisk = function(expr.mas5) {
	# 1) + 2)
	exp.mas5 = exprs(expr.mas5)

	# 3)
	# 70 gene predictor
	gep70 = read.table("data/GEP70Genes.txt", header=TRUE, sep="\t", as.is=TRUE)

	rownames(gep70) = gep70[,2]
	sel = intersect(gep70$Array_No, rownames(exp.mas5))
	gep70_log2_exp_mas5 = as.matrix(log2(exp.mas5[sel,]))

	# 4)
	centered_gep70_log2_exp_mas5 = gep70_log2_exp_mas5 - gep70[sel,"TrainingMeans"]
	
	# 5)
	weighted_ave = t(gep70[sel,c("PC1","PC2","PC3","PC4","PC5","PC6")]) %*% centered_gep70_log2_exp_mas5

	# 6)
	PH_coef = c(-0.193970199, -0.102403915, 0.048411741, 0.043861307, 0.181940086, 0.171153439)

	risk_score = t(weighted_ave) %*% PH_coef

	Group_center = c(-0.833202182, 0.075642196, 1.395960868)

	# 7)
	# --> only squared dist used for report, abs dist is additionaly caclulated as suggested above (Shaughnessy, 2007)
	abs_dist = abs(matrix(risk_score,nrow=length(risk_score),ncol=3) - matrix(Group_center,nrow=length(risk_score),ncol=3,byrow=TRUE))
	predicted = apply(abs_dist, 1, which.min)

	squared_dist = (matrix(risk_score,nrow=length(risk_score),ncol=3) - matrix(Group_center,nrow=length(risk_score),ncol=3,byrow=TRUE))^2
	predicted2 = apply(squared_dist, 1, which.min)

	predicted  = ifelse(predicted==1, "low risk", ifelse(predicted==2, "medium risk", "high risk"))
	predicted2 = ifelse(predicted2==1, "low risk", ifelse(predicted2==2, "medium risk", "high risk"))

	# substitute medium risk to low risk
	predicted.sub = gsub("medium risk", "low risk", predicted)
	predicted2.sub = gsub("medium risk", "low risk", predicted2)

	# 17 gene predictor according to paper
	probesets.17 = c("200638_s_at","1557277_a_at","200850_s_at","201897_s_at","202729_s_at","203432_at","204016_at","205235_s_at","206364_at","206513_at","211576_s_at",
                  "213607_x_at","213628_at","218924_s_at","219918_s_at","220789_s_at","242488_at")
	coef.17 = c(0.283,-0.296,-0.208,0.314,-0.287,0.251,0.193,0.269,0.375,0.158,0.316,0.232,-0.251,-0.230,-0.402,0.191,0.148)
	
	risk_score17 <-  t(centered_gep70_log2_exp_mas5[probesets.17,]) %*% coef.17
	risk_score17c <- as.factor(ifelse(risk_score17 > 1.5,"high risk","low risk"))

	return(list(predicted.abs=predicted, predicted.sub.abs=predicted.sub, predicted.sqrt=predicted2, predicted.sub.sqrt=predicted2.sub, predicted17=risk_score17c, score=risk_score))
}

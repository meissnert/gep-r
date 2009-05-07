shrisk = function(expr.mas5) {
	exp.mas5 = exprs(expr.mas5)

	# 70 gene predictor
	gep70 = read.table("data/GEP70Genes.txt", header=TRUE, sep="\t", as.is=TRUE)

	rownames(gep70) = gep70[,2]
	sel = intersect(gep70$Array_No, rownames(exp.mas5))
	gep70_log2_exp_mas5 = as.matrix(log2(exp.mas5[sel,]))

	centered_gep70_log2_exp_mas5 = gep70_log2_exp_mas5 - gep70[sel,"TrainingMeans"]
	# apply(gep70_log2_exp_mas5,1,mean) - gep70[sel,"TrainingMeans"] << warum??  noch klären!
	
	weighted_ave = t(gep70[sel,c("PC1","PC2","PC3","PC4","PC5","PC6")]) %*% centered_gep70_log2_exp_mas5

	PH_coef = c(-0.193970199, -0.102403915, 0.048411741, 0.043861307, 0.181940086, 0.171153439)

	risk_score = t(weighted_ave) %*% PH_coef

	Group_center = c(-0.833202182, 0.075642196, 1.395960868)

	abs_dist = abs(matrix(risk_score,nrow=length(risk_score),ncol=3) - matrix(Group_center,nrow=length(risk_score),ncol=3,byrow=TRUE))
	predicted = apply(abs_dist, 1, which.min)

	squared_dist = (matrix(risk_score,nrow=length(risk_score),ncol=3) - matrix(Group_center,nrow=length(risk_score),ncol=3,byrow=TRUE))^2
	predicted2 = apply(squared_dist, 1, which.min)

	predicted  = ifelse(predicted==1, "low risk", ifelse(predicted==2, "medium risk", "high risk"))
	predicted2 = ifelse(predicted2==1, "low risk", ifelse(predicted2==2, "medium risk", "high risk"))

	# 17 gene predictor according to paper
	probesets.17 = c("200638_s_at","1557277_a_at","200850_s_at","201897_s_at","202729_s_at","203432_at","204016_at","205235_s_at","206364_at","206513_at","211576_s_at",
                  "213607_x_at","213628_at","218924_s_at","219918_s_at","220789_s_at","242488_at")
	coef.17 = c(0.283,-0.296,-0.208,0.314,-0.287,0.251,0.193,0.269,0.375,0.158,0.316,0.232,-0.251,-0.230,-0.402,0.191,0.148)
	
	risk_score17 <-  t(centered_gep70_log2_exp_mas5[probesets.17,]) %*% coef.17
	risk_score17c <- as.factor(ifelse(risk_score17 > 1.5,"high risk","low risk"))

	return(list(predicted.abs=predicted, predicted.sqrt=predicted2, predicted17=risk_score17c))
}

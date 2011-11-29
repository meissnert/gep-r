#  TC Classification according to 
#  Bergsagel et al (2005)

bergsagel = function(expr.mas5) {
	load("data/bergsagel_references.Rdata")
	
	#korrektur
	cmaf2.reference = cmaf2.rererence	
	
	# relative expression values / standardized by max per gene / do not use log2 transformed data (Bergsagel Spreadsheet)
	fgfr3 = exprs(expr.mas5)["204379_s_at",]                 # 31805_at
	fgfr3 = fgfr3/fgfr3.reference
	mmset = exprs(expr.mas5)["209054_s_at",]                 # 38988_at
	mmset = mmset/mmset.reference
	cmaf1 = exprs(expr.mas5)["209348_s_at",]                 # 41504_s_at
	cmaf1 = cmaf1/cmaf1.reference
	cmaf2 = exprs(expr.mas5)["209348_s_at",]                 # 41505_s_at
	cmaf2 = cmaf2/cmaf2.reference
	cmaf.total = cmaf1
	cmaf.total = cmaf.total/max(cmaf.total)
	itgb7 = exprs(expr.mas5)["205718_at",]                   # 2019_s_at
	itgb7 = itgb7/itgb7.reference
	cx3cr1 = exprs(expr.mas5)["205898_at",]                   # 40646_at
	cx3cr1 = cx3cr1/cx3cr1.reference
	d31 = exprs(expr.mas5)["201700_at",]                   # 1794_at
	d32 = exprs(expr.mas5)["201700_at",]                   # 1795_g_at
	d3.total = d31
	d3.total = d3.total/d3.total.reference
	d11 = exprs(expr.mas5)["208711_s_at",]                 # 2017_s_at
	d12 = exprs(expr.mas5)["208712_at",]                   # 2020_at
	d13 = exprs(expr.mas5)["208712_at",]                   # 38418_at
	d1.av = apply(cbind(d11,d12),1,mean)
	d1.av = d1.av/d1.av.reference
	d21  = exprs(expr.mas5)["200953_s_at",]                 # 36650_at
	d22 = exprs(expr.mas5)["200951_s_at",]                 # 1983_at
	d2.total = apply(cbind(d21,d22),1,sum)
	d2.total = d2.total/d2.total.reference

	# classification
	tc.4p16 = ifelse(fgfr3>0.3 | mmset>0.1,1,0)
	tc.maf  = ifelse(itgb7 > 0.18 & cx3cr1 > 0.02,1,0)
	tc.6p21 = ifelse(d3.total > 0.5 ,1,0)
	tc.11q13 = ifelse(d1.av > 0.25 ,1,0)
	tc.d1 = ifelse(d1.av > 0.024 & d2.total < 0.09 ,1,0)
	tc.d1d2 = ifelse(d1.av > 0.024 & d2.total > 0.09 ,1,0)
	tc.d2 = ifelse(d1.av < 0.024 & d2.total > 0.09 ,1,0)
	tc.none = ifelse(tc.4p16 == 0 &
	                 tc.maf == 0 &
	                 tc.6p21 == 0 &
	                 tc.11q13 == 0 &
	                 tc.d1 == 0 &
	                 tc.d1d2 == 0 &
	                 tc.d2 == 0, 1, 0)
	tc.mat = cbind(tc.4p16, tc.maf, tc.6p21, tc.11q13, tc.d1, tc.d1d2, tc.d2, tc.none)
	colnames(tc.mat) = c("4p16", "maf", "6p21", "11q13", "d1", "d1d2", "d2" ,"none")
	
	return(colnames(tc.mat)[which(tc.mat==1)])
}
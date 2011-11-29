#  TC Classification according to 
#  Chng et al. 2007

geometric.mean = function(x,na.rm=TRUE) { 
		exp(mean(log(x),na.rm=na.rm)) 
	}

tc.class = function(expr.mas5) {
	# Calculate the geometric mean of (201506_at, 201743_at, 203645_s_at, 204006_s_at, 206680_at, 209224_s_at, 209924_at, 213550_s_at, 
	# 215049_x_at, 216233_at, 32128_at). This is 10 to the power of the average log10. So you sum the log10 values, divide by 11, 
	# and raise to the power of ten. Call this value Macrophage_Index 
	probesets = c("201506_at", "201743_at", "203645_s_at", "204006_s_at", 
	              "206680_at", "209224_s_at", "209924_at", "213550_s_at",
	              "215049_x_at", "216233_at", "32128_at")
	              
    macrophageIndex = geometric.mean(expr.mas5[probesets,])
    
    if (expr.mas5["204379_s_at",] > 11215) erg = "4p16 FGFR3+"
    
    else if (expr.mas5["204379_s_at",] < 11215 & expr.mas5["223472_at",] > 759) erg = "4p16 FGFR3-"
    
    else if (expr.mas5["201700_at",] > 9660 & (expr.mas5["200953_s_at",] < 3198 & expr.mas5["208712_at",] < 465)) erg = "6p21"
    
    # if ((expr.mas5["205718_at"] > 17296 | expr.mas5["205898_at"] > 2231) & (expr.mas5["209348_s_at"] > 5633 | expr.mas5["218559_s_at"] > 11220)) erg = "Maf"
    
    else if (expr.mas5["205718_at",] > 17296 & expr.mas5["205898_at",] > 2231) {
	  if (expr.mas5["209348_s_at",] > 5633 & expr.mas5["218559_s_at",] < 11220) erg = "c-MAF"
	  else if (expr.mas5["209348_s_at",] < 5633 & expr.mas5["218559_s_at",] > 11220) erg = "MAFB"
	  else if (expr.mas5["209348_s_at",] > 5633 & expr.mas5["218559_s_at",] > 11220) erg = "C-MAF and MAFB" else erg = "?MAFA"
    }
    
    else if (expr.mas5["205718_at",] > 17296 | expr.mas5["209348_s_at",] > 5633 | expr.mas5["218559_s_at",] > 2231 &
        macrophageIndex < 700 & expr.mas5["200953_s_at",] > 9840) {
	  if (expr.mas5["209348_s_at",] > 5633 & expr.mas5["218559_s_at",] < 11220) erg = "c-MAF"
	  else if (expr.mas5["209348_s_at",] < 5633 & expr.mas5["218559_s_at",] > 11220) erg = "MAFB"
	  else if (expr.mas5["209348_s_at",] > 5633 & expr.mas5["218559_s_at",] > 11220) erg = "C-MAF and MAFB" else erg = "?MAFA"
    }
    
    else if (expr.mas5["208711_s_at",] > 6191) erg = "11q13"
    
    else if (expr.mas5["200953_s_at",] <= 3198 & expr.mas5["208711_s_at",] <= 6191 & expr.mas5["208712_at",] > 465) erg = "D1"
    
    else if (expr.mas5["200953_s_at",] > 3198 & expr.mas5["208711_s_at",] <= 6191 & expr.mas5["208712_at",] > 465) erg = "D1+D2"
    
    else if (expr.mas5["200953_s_at",] > 3198 & expr.mas5["208712_at",] <= 465) erg = "D2"
    
    else if (expr.mas5["200953_s_at",] <= 3198 & expr.mas5["208712_at",] <= 465) erg = "none"
 
  return(erg)
}

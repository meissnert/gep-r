decaux = function(expr) {
	load("data/cutoff.decaux.Rdata")

	umgc.risk.score =  exprs(expr)["208644_at",]  *0.27578783 + exprs(expr)["202470_s_at",]*0.26987655 + exprs(expr)["202951_at",]  *0.29530369 +
                           exprs(expr)["200783_s_at",]*0.31490195 - exprs(expr)["201425_at",]  *0.13137903 + exprs(expr)["231736_x_at",]*0.17772804 + 
                           exprs(expr)["217752_s_at",]*0.38697337 + exprs(expr)["202486_at",]  *0.30371178 + exprs(expr)["212098_at",]  *0.25043791 -
                           exprs(expr)["209683_at",]  *0.29483393 + exprs(expr)["228677_s_at",]*0.19243758 - exprs(expr)["200779_at",]  *0.2491429  - 
                           exprs(expr)["203657_s_at",]*0.17822457 + exprs(expr)["204072_s_at",]*0.21255699 + exprs(expr)["228737_at",]  *0.21956366
	decaux.risk = as.factor(ifelse(umgc.risk.score > cutoff.decaux,"high risk","low risk"))
	return(list(decaux.score = as.numeric(umgc.risk.score), decaux.risk = as.vector(decaux.risk)))
}

decaux.sa = function(expr) {
	load("data/cutoff.decaux.sa.Rdata")

	umgc.risk.score =  exprs(expr)["208644_at",]  *0.27578783 + exprs(expr)["202470_s_at",]*0.26987655 + exprs(expr)["202951_at",]  *0.29530369 +
                           exprs(expr)["200783_s_at",]*0.31490195 - exprs(expr)["201425_at",]  *0.13137903 + exprs(expr)["231736_x_at",]*0.17772804 + 
                           exprs(expr)["217752_s_at",]*0.38697337 + exprs(expr)["202486_at",]  *0.30371178 + exprs(expr)["212098_at",]  *0.25043791 -
                           exprs(expr)["209683_at",]  *0.29483393 + exprs(expr)["228677_s_at",]*0.19243758 - exprs(expr)["200779_at",]  *0.2491429  - 
                           exprs(expr)["203657_s_at",]*0.17822457 + exprs(expr)["204072_s_at",]*0.21255699 + exprs(expr)["228737_at",]  *0.21956366
	decaux.risk = as.factor(ifelse(umgc.risk.score > cutoff.decaux,"high risk","low risk"))
	return(list(decaux.score = as.numeric(umgc.risk.score), decaux.risk = as.vector(decaux.risk)))
}



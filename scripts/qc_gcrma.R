# function taken from simpleaffy(.bg.stats() function), not changed, needed for 
# my.qc()
my.bg.stats <- function(unnormalised, grid=c(4,4)) {
pms         <- unlist(pmindex(unnormalised))
mms         <- unlist(mmindex(unnormalised))
all         <- c(pms,mms)
intensities <- exprs(unnormalised)
rws <- nrow(unnormalised)
cls <- ncol(unnormalised)
zonebg <- c();
zonesd <- c();
for(no in 1:length(unnormalised)){
  this.array <- intensities[,no];
  result <- .C("bgmas",as.integer(as.vector(all)),as.integer(length(all)),
       as.double(as.vector(this.array)),as.integer(length(this.array)),
       as.integer(rws),
       as.integer(cls),
       as.integer(grid[1]),as.integer(grid[2]),
       zonebg=double(grid[1] * grid[2]),
       zonesd=double(grid[1] * grid[2]),corrected=double(length(this.array)),PACKAGE="simpleaffy");
  zonesd <- rbind(zonesd, result$zonesd);
  zonebg <- rbind(zonebg, result$zonebg);
  }
  colnames(zonesd) <- paste("zone",1:16,sep=".");
  colnames(zonebg) <- paste("zone",1:16,sep=".");
  rownames(zonesd) <- sampleNames(unnormalised);
  rownames(zonebg) <- sampleNames(unnormalised);
  return(list(zonebg=zonebg,zonesd=zonesd))
}

# function taken from the yaqcaffy package, not changed, used for my.repplot()
my.plotdiag <- function() {
  fc <- c(2,4,8)
  k <- 1
  for ( i in fc) {
    ## upper diagonal and text
    abline(log2(i),1,lwd=0.7,col="darkgray")
    ## lower diagonal and text
    abline(-log2(i),1,lwd=0.7,col="darkgray")
  }
}

# function taken from the simpleaffy package (gc() function) and modified:
# scalefactors and targed taken out, not used for gcrma
# present call changed to panp present call
setClass("QCStats",representation(scale.factors="numeric",target="numeric",percent.present="numeric",average.background="numeric",minimum.background="numeric",maximum.background="numeric",spikes="matrix",qc.probes="matrix",bioBCalls="character",bioCCalls="character",bioDCalls="character",creCalls="character",arraytype="character"));


my.qc = function (unnormalised, normalised, panp, tau = 0.015, logged = TRUE, cdfn = cdfName(unnormalised)) {
    verbose <- getOption("verbose")

    cdfn <- cleancdfname(cdfn)

    simpleaffy:::setQCEnvironment(cdfn)

    x <- exprs(normalised)
    #det <- detection.p.val(unnormalised, tau = tau, alpha1 = qc.get.alpha1(),
    #    alpha2 = qc.get.alpha2())
    dpv <- apply(panp, 2, function(x) {
        x[x == "A"] <- 0
        x[x == "P" | x == "M"] <- 1
        x <- as.numeric(x)
        return(100 * sum(x)/length(x))
    })
    #sfs <- experimentData(normalised)@preprocessing$sfs
    #target <- experimentData(normalised)@preprocessing$tgt
    if (!logged) {
        x <- log2(x)
    }
    bgsts <- my.bg.stats(unnormalised)$zonebg
    meanbg <- apply(bgsts, 1, mean)
    minbg <- apply(bgsts, 1, min)
    maxbg <- apply(bgsts, 1, max)
    stdvbg <- sqrt(apply(bgsts, 1, var))
    qc.probenames <- simpleaffy:::qc.get.probes()
    qc.probe.vals <- rbind(c(), (sapply(qc.probenames, function(y) {
        x[y, ]
    })))
    rownames(qc.probe.vals) <- colnames(x)
    colnames(qc.probe.vals) <- qc.probenames
    spike.probenames <- simpleaffy:::qc.get.spikes()
    spike.vals <- rbind(c(), (sapply(spike.probenames, function(y) {
        x[y, ]
    })))
    rownames(spike.vals) <- colnames(x)
    colnames(spike.vals) <- spike.probenames
    bb <- spike.probenames["bioB"]
    bc = spike.probenames["bioC"] # added bioC call
    bd = spike.probenames["bioD"] # added bioD call
    cre = spike.probenames["creX"] # added cre Call
    if (!is.na(bb)) {
        biobcalls <- panp[bb, ]
    }
    else {
        biobcalls <- NULL
    }
    if (!is.na(bc)) {
        bioccalls <- panp[bc, ]
    }
    else {
        bioccalls <- NULL
    }
    if (!is.na(bd)) {
        biodcalls <- panp[bd, ]
    }
    else {
        biodcalls <- NULL
    }
    if (!is.na(cre)) {
        crecalls <- panp[cre, ]
    }
    else {
        crecalls <- NULL
    }
    return(new("QCStats", #scale.factors = sfs, target = target,
        percent.present = dpv, average.background = meanbg, minimum.background = minbg,
        maximum.background = maxbg, 
	spikes = spike.vals, qc.probes = qc.probe.vals,
        bioBCalls = biobcalls, bioCCalls = bioccalls, bioDCalls = biodcalls, creCalls = crecalls, arraytype = cdfn))
}

# this is taken from the yaqcaffy package (reprodPlot() function) and modified
# normalization taken out, only ploting here, function takes the normalized obekt as an argument
my.repplot = function(data, cex=1, main = "Myeloma reference reproducibility") {
    e <- exprs(data)
    dim <- ncol(e)
    cor <- cor(e)
    labels <- sampleNames(data)
    old.par <- par(no.readonly = TRUE)
    on.exit(par(old.par))
    par(mfrow = c(dim, dim), mgp = c(0, 0.2, 0), mar = c(0, 0,
        0, 0), oma = c(1.8, 1.8, 1.8, 1.8))
    for (i in 1:(dim - 1)) {
        par(mfg = c(i, i))
        plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "",
            ylab = "")
        text(1, 1, labels[i], cex = 1.2 * cex)
        for (j in (i + 1):dim) {
            par(mfg = c(i, j))
            txt <- format(cor[i, j], digits = 3)
            plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "",
                ylab = "")
            text(1, 1, txt, cex = 1.3 * cex)
            par(mfg = c(j, i))
            if (i == 1 && j%%2 != 0)
                smoothScatter(e[, c(i, j)], xlab = "", ylab = "",
                  xaxt = "n")
            else if (j == dim && i%%2 == 0)
                smoothScatter(e[, c(i, j)], xlab = "", ylab = "",
                  yaxt = "n")
            else smoothScatter(e[, c(i, j)], xlab = "", ylab = "",
                xaxt = "n", yaxt = "n")
            my.plotdiag()  
        }
    }
    par(mfg = c(dim, dim))
    plot(1, 1, type = "n", xaxt = "n", yaxt = "n", xlab = "",
        ylab = "")
    text(1, 1, labels[dim], cex = cex)
    mtext(main, 3, outer = TRUE, cex = 1.4 * cex)
}

# function taken from the simpleaffy package (plot.qc.stats() function) and modified for gcrma preprocessing
# scale factors taken out
my.plot.qc.stats = function(x, fc.line.col="black", chip.label.col="black", gdh.thresh = 2.3, ba.thresh = 5.2, present.thresh=8, bg.thresh=20, label=NULL, main="QC Stats", usemid=F, spread=c(-8,8), cex=1,...) {
  old.par <- par()
  par(mai=c(0,0,0,0))
  # sfs    <- log2(sfs(x))

  # n      <- length(sfs)

  # meansf <- mean(sfs)

  dpv <- simpleaffy:::percent.present(x)
  dpv <- (round(100*dpv))/100;

  abg <- log2(simpleaffy:::avbg(x))
  abg <- (round(100*abg))/100;
	
  if(is.null(label)) { label <- names(simpleaffy:::maxbg(x)) }
  d1 <- 0.0;
  d2 <- 0.0;
  d3 <- 0.0;

  n = length(dpv)

  for(i in 1:n) {
    for(j in 1:n) { 
      #d1 <- max(abs(sfs[i] - sfs[j]),d1);
      d2 <- max(abs(dpv[i] - dpv[j]),d2);
      d3 <- max(abs(abg[i] - abg[j]),d3);
    }
  }

  # set up plotting area - a column for array names next to a column for the QC

  m <- matrix(c(4,2,1,3) ,nrow=2,ncol=2)
  layout(m,c(1,2),c(0.1,1))
  # the title
  if(is.null(main)) { main="" }
  plot(0,0,xlim=range(0,1),ylim=range(0,1),type="n",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(0.5,0.5,labels=main,adj=0,cex=cex*2)

  # write out the array names

  plot(0,0,xlim=range(0,1),ylim=range(-1,n),type="n",yaxs="i",xaxt="n",yaxt="n",bty="n")
  text(1,(1:n)-0.5,labels=label,adj=1,cex=cex)
  plot(0,0,xlim=spread,ylim=c(-1,n),type="n",xaxs="i",yaxs="i",xaxt="n",yaxt="n",bty="n")

  #x1 <- (sf.thresh/2.0 +  meansf)
  #y1 <- 0
  #x2 <- (-sf.thresh/2.0 +  meansf)
  #y2 <- n

  #polygon(c(x1,x2,x2,x1),c(y1,y1,y2,y2),col=sf.ok.region,border=sf.ok.region);
  lines(c(0,0),c(0,n),lty=1,col=fc.line.col)
  lines(c(-1,-1),c(0,n),lty=2,col="grey")
  lines(c(-2,-2),c(0,n),lty=2,col="grey")
  lines(c(-2.3,-2.3),c(0,n),lty=2,col=fc.line.col)
  lines(c(-3,-3),c(0,n),lty=2,col="grey")
  lines(c(-4,-4),c(0,n),lty=2,col="grey")
  lines(c(-5,-5),c(0,n),lty=2,col="grey")
  lines(c(-5.2,-5.2),c(0,n),lty=2,col=fc.line.col)
  lines(c(1,1),c(0,n),lty=2,col="grey")
  lines(c(2,2),c(0,n),lty=2,col="grey")
  lines(c(2.3,2.3),c(0,n),lty=2,col=fc.line.col)
  lines(c(3,3),c(0,n),lty=2,col="grey")
  lines(c(4,4),c(0,n),lty=2,col="grey")
  lines(c(5,5),c(0,n),lty=2,col="grey")
  lines(c(5.2,5.2),c(0,n),lty=2,col=fc.line.col)
  text(6,-1,"6",pos=3,col=fc.line.col,cex=cex)
  text(5,-1,"5",pos=3,col=fc.line.col,cex=cex)
  text(4,-1,"4",pos=3,col=fc.line.col,cex=cex)
  text(3,-1,"3",pos=3,col=fc.line.col,cex=cex)
  text(2,-1,"2",pos=3,col=fc.line.col,cex=cex)
  text(1,-1,"1",pos=3,col=fc.line.col,cex=cex)
  text(-6,-1,"-6",pos=3,col=fc.line.col,cex=cex)
  text(-5,-1,"-5",pos=3,col=fc.line.col,cex=cex)
  text(-4,-1,"-4",pos=3,col=fc.line.col,cex=cex)
  text(-3,-1,"-3",pos=3,col=fc.line.col,cex=cex)
  text(-2,-1,"-2",pos=3,col=fc.line.col,cex=cex)
  text(-1,-1,"-1",pos=3,col=fc.line.col,cex=cex)
  text(0,-1,"0",pos=3,col=fc.line.col,cex=cex)

  rats <- simpleaffy:::ratios(x);
  if(!usemid) {
    gdh <- rats[,3];
    ba  <- rats[,1];
  }
  else {
    gdh <- rats[,4];
    ba  <- rats[,2];
  }

  bb  <- x@bioBCalls
  bc = x@bioCCalls
  bd = x@bioDCalls
  cre = x@creCalls

  for(i in 1:n) {
    x1<-spread[1]
    x2<-spread[2]
    y1<-i-1;
    y2<-i-1;
    lines(c(x1,x2),c(y1,y2),lty=2,col="light grey")
    #if(d1 > sf.thresh) { col = "red" } else {col="blue"}
    # x1 <- sfs[i]
    # y1 <- i-0.25
    # lines(c(0,x1),c(y1,y1),col=col);

    # points(x1,y1,col=col,pch=20);
     x2 <- gdh[i]
     y2 <- i-0.5;
     if(gdh[i] > gdh.thresh) { col = "red" } else {col="blue"}	
     points(x2,y2,pch=1,col=col);

     x2 <- ba[i];
     y2 <- i-0.5;
     if(ba[i] > ba.thresh) { col = "red" } else {col="blue"}	
     points(x2,y2,pch=2,col=col);

     if(d2 > present.thresh) { col = "red" } else {col="blue"}
     x2 <- spread[1]
     y2 <- i-0.25
     dpvs<-paste(dpv[i],"%",sep="")
     text(x2,y2,label=dpvs,col=col,pos=4,cex=cex);
     if(d3 > bg.thresh) { col = "red" } else {col="blue"}
     x2 <- spread[1]
     y2 <- i-0.75
     text(x2,y2,label=abg[i],col=col,pos=4,cex=cex);
     if(bb[i]!="P") {
       text(0,i-1,label="bioB",col="red",cex=cex);
     }
     if(bc[i]!="P") {
       text(0,i-1,label="bioC",col="red",cex=cex);
     }
     if(bd[i]!="P") {
       text(0,i-1,label="bioD",col="red",cex=cex);
     }
     if(cre[i]!="P") {
       text(0,i-1,label="Cre",col="red",cex=cex);
     }

  }
  plot(0,0,xlim=range(0,1),ylim=range(0,1),type="n",yaxs="i",xaxt="n",yaxt="n",bty="n")
  if(!usemid) {
    points(0.25,0.25,pch=1)
    text(0.3,0.25,colnames(rats)[3],pos=4,cex=cex)
    points(0.25,0.5,pch=2)
    text(0.3,0.5,colnames(rats)[1],pos=4,cex=cex)
  }
  else {
    points(0.25,0.25,pch=1)
    text(0.3,0.25,colnames(rats)[4],pos=4,cex=cex)
    points(0.25,0.5,pch=2)
    text(0.3,0.5,colnames(rats)[2],pos=4,cex=cex)
  }
  
  ow <- options("warn")$warn
  options(warn=-1)
  par(old.par)
  options(warn=ow)
}

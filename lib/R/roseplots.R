
# Rose diagrams by individual
rose.indiv <- function(d, file='Plots.pdf'){
  degs <- seq(0,355,by=5)
  pdf(file=file)
  par(mfrow=c(2,2))
  dd <- apply(d[,-(1:2)], 1, function(x) degs[which(x>0)])
  for(i in 1:length(dd)){
    x = circular(dd[[i]], units='degrees')
    rose.diag(x, bins=18, main=paste('Subject',d[i,1]))
  }
  dev.off()
}

rose.overall <- function(dat, degs=seq(0,355,by=5),thresh=0, ignore=1:2,...){ d <- apply(dat,1, function(x) degs[which(x[-ignore]>thresh)])
  rose.diag(circular(do.call(c,d), units='degrees'),bin=18,...)
}


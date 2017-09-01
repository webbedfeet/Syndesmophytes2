
# Implement Follman and Proschan method (Biometrics 55:782-791, 1999)

follman <- function(dat, nperm=1000, thresh=0,degs = seq(0,355,by=5), ignore=1:2,seed=1251235){
  d <- apply(dat,1,function(x) degs[which(x[-ignore]>thresh)]) # first 2 cols of dat are id and IDS
  T0 = kuiper.test(circular(do.call(c,d), units='degrees'))$stat
  set.seed(seed)
  stat <- rep(0,nperm)
  for(i in 1:nperm){
    th = as.list(round(runif(length(d),0,360)))
    d2 <- lapply(mapply('+',d,th),'%%',360) # random rotations
    stat[i] <- kuiper.test(circular(do.call(c,d2),units='degrees'))$stat
  }
  return(list(stat=T0, dist=stat, pval=mean(stat>T0)))
}

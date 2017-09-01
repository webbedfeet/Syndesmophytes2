#' Using ggplot2 to creae a rose plot
ggrose <- function (dat, degs=seq(0,355,by=5), thresh = 0, title="") {
  d <- apply(dat, 1, function(x) degs[which(x[-(1:2)]>thresh)])
  dd <- do.call(c,d)
  dd <- as.data.frame(dd)
  dd$dd <- factor(dd$dd, levels = as.character(degs))
  p <- ggplot(dd, aes(x=dd))+geom_bar(width=1, fill='red', color='red')+
    coord_polar()+scale_x_discrete(drop=F,breaks=c(0,90,180,270)) + theme_bw()+
    theme(axis.ticks.y=element_blank(), axis.text.y=element_blank(),
          axis.text.x=element_text(size=12, face='bold'))+
    labs(x='', y='') +ggtitle(paste('IDS =',unique(dat$IDS.level)))
  return(p)
}

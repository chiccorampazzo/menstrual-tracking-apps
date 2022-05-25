# plot fit of predictions
plotFit <- function(dat, main='Model Fit', xlab=NULL, ylab=NULL, file=NULL, zoom=1, predCol='mean'){
  
  print(paste('Observed vs predicted plot:', file))
  
  if(!is.null(file)) jpeg(file)
  
  dat$pred <- dat[,predCol]
  
  r2 <- round(cor(dat$obs, dat$pred, use='pairwise.complete.obs')^2, 3)
  
  if(zoom < 1){
    dat <- dat[dat$obs < quantile(dat$obs, probs=zoom, na.rm=T), ]
  }
  
  xlim <- c(0,max(dat$obs, na.rm=T))
  ylim <- c(0,max(dat$upper, na.rm=T))
  
  # labels
  if(is.null(xlab)) xlab <- 'Observed'
  if(is.null(ylab)) ylab <- 'Predicted'
  main <- paste0(main,'\nr-squared = ',r2)
  
  par(mar=c(4,4,1,1) + 0.5)
  
  plot(NA,
       xlim=xlim,
       ylim=ylim,
       xlab=xlab,
       ylab=NA,
       main=NA)
  
  mtext(ylab, side=2, line=2.5)
  
  text(x=xlim[1], y=ylim[2]-0.05*diff(ylim), labels=main, cex=1.5, pos=4)
  
  for(i in 1:nrow(dat)){
    arrows(x0=dat$obs[i], y0=dat$upper[i], y1=dat$lower[i], length=0, col=rgb(0,0,0,0.2))
  }
  points(dat$obs, dat$pred, col=rgb(0,0,0,0.5))
  abline(0,1,col='red')
  
  if(!is.null(file)) dev.off()
}
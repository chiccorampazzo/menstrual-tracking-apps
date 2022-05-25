# create data for plotFit
plotDat <- function(d, obs, hat, alpha=0.05){
  
  n <- length(obs)
  
  plotdat <- data.frame(i = 1:n, 
                        obs = obs)
  
  plotdat[,c('mean','median','lower','upper')] <- NA
  
  for(i in 1:n){
    plotdat[i, 'mean'] <- mean(d[,hat[i]], na.rm=T)
    plotdat[i, 'median'] <- median(d[,hat[i]], na.rm=T)
    plotdat[i, 'lower'] <- quantile(d[,hat[i]], probs=alpha/2, na.rm=T)
    plotdat[i, 'upper'] <- quantile(d[,hat[i]], probs=1-(alpha/2), na.rm=T)
  }
  
  return(plotdat)
}
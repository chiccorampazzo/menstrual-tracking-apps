# create data for plotFit
plotDat <- function(d, data_list, alpha=0.05){
  
  n <- length(data_list$y)
  
  plotdat <- data.frame(i = 1:n, 
                        obs = data_list$y)
  
  plotdat[,c('mean','median','lower','upper')] <- NA
  
  for(i in 1:n){
    colname <- paste0('yhat.',i,'.')
    plotdat[i, 'mean'] <- mean(d[,colname])
    plotdat[i, 'median'] <- median(d[,colname])
    plotdat[i, 'lower'] <- quantile(d[,colname], probs=alpha/2)
    plotdat[i, 'upper'] <- quantile(d[,colname], probs=1-(alpha/2))
  }
  return(plotdat)
}
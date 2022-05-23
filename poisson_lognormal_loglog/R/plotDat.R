# create data for plotFit
plotDat <- function(d, md, alpha=0.05, obs='y', hat='yhat'){
  
  n <- length(md[[obs]])
  
  plotdat <- data.frame(i = 1:n, 
                        obs = md[[obs]])
  
  plotdat[,c('mean','median','lower','upper')] <- NA
  
  for(i in 1:n){
    colname <- paste0(hat,'[',i,']')
    plotdat[i, 'mean'] <- mean(d[,colname])
    plotdat[i, 'median'] <- median(d[,colname])
    plotdat[i, 'lower'] <- quantile(d[,colname], probs=alpha/2)
    plotdat[i, 'upper'] <- quantile(d[,colname], probs=1-(alpha/2))
  }
  return(plotdat)
}
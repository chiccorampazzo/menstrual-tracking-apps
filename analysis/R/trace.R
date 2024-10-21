trace <- function(fit, tracedir, vars=NA){
  
  dir.create(tracedir, recursive=T, showWarnings=F)
  
  if(is.na(vars[1])){
    vars <- coda::varnames(fit$mcmc)
  }
  
  jpeg(file.path(tracedir, paste('trace.%02d.jpg', sep='')), quality=100, height=1000, width=1000)
  for (i in seq(1, length(vars), 3)){
    plot(fit$mcmc[,vars][,vars[i:min(length(vars), i+2)]], cex = 1.25)
  }
  dev.off()
}
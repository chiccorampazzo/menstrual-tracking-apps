# convert mcmc list to data.frame 
mcmcToDataframe <- function(mcmc_list){
  d <- mcmc_list[[1]]
  for(i in 2:length(mcmc_list)){
    d <- rbind(d, mcmc_list[[i]])
  }
  d <- data.frame(d)
  return(d)
}
inits <- function(md, nchains=3){
  
  set.seed(md$seed)
  result <- list()
  
  for(i in 1:nchains){
    i.result <- list()
    
    i.result$beta0 <- runif(1, 5, 7)
    i.result$beta1 <- runif(1, -1, 1)
    i.result$beta2 <- runif(1, -1, 1)
    
    i.result$sigma <- runif(1, 0, 1)
    i.result$alpha <- runif(md$n_covs+md$n_interacts, -1, 1)
    
    i.result$installs <- md$installs
    i.result$installs[,'appstore'] <- md$installs[,'playstore']
    i.result$installs[,'playstore'] <- NA
    
    i.result$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    i.result$.RNG.seed = i * md$seed
    result[[i]] <- i.result
  }
  
  return(result)
}

ixval <- function(n, nk){
  imax <- ceiling(n/nk)
  result <- matrix(NA, nrow=nk, ncol=imax)
  i_vector <- sample(1:n, n)
  for(i in 1:imax){
    kk <- min(nk, length(i_vector))
    result[1:kk, i] <- i_vector[1:kk]
    i_vector <- i_vector[-seq(1,kk)]
  }
  
  itest <- list()
  for(k in 1:nk){
    itest[[k]] <- as.vector(result[k,])
    if(any(is.na(itest[[k]]))) {
      itest[[k]] <- itest[[k]][-which(is.na(itest[[k]]))]  
    }
  }

  return(itest)
}
# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(runjags)
library(coda)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load functions
for(i in list.files('R')) source(file.path('R',i))

# output directory
outdir <- file.path('out')
dir.create(outdir, recursive=T, showWarnings=F)

# models
fits <- list.files(file.path('out', 'mcmc'), full.names=T)

# setup k folds
nk <- 10
n <- readRDS(file.path(fits[1]))$md$n_countries
itest.samp <- sample(1:n, n)
itest.n <- round(n / nk)
itest <- list()
for(k in 1:nk){
  st <- k * itest.n - itest.n + 1
  en <- st + itest.n - 1
  if(k==nk) en <- n
  
  itest[[k]] <- itest.samp[st:en]
}
rm(itest.samp, itest.n, st, en)

# cross-validate each model
overwrite <- FALSE
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  message(paste0('Cross-validating model: ', model_name))
  
  # output directory
  xvaldir <- file.path(outdir, model_name, 'xval')
  dir.create(xvaldir, showWarnings=F, recursive=T)
  
  saveRDS(itest, file=file.path(xvaldir, 'itest.rds'))
  
  
}

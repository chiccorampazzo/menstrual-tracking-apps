# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# packages
library(coda)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load functions
for(i in list.files('R')) source(file.path('R',i))

# output directory
outdir <- file.path('out', 'eval')
dir.create(outdir, recursive=T, showWarnings=F)

# models
fits <- list.files(file.path('out', 'mcmc'), full.names=T)

for(i in 1:length(fits)){
  
  fit <- readRDS(file.path(fits[i]))
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  
  # trace plots
  trace(fit, 
        tracedir = file.path(outdir, model_name, 'trace'),
        vars = grep('hat', varnames(fit$mcmc), invert=T, value=T))

}


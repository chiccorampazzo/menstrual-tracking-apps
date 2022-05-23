# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
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

# evaluate each model
overwrite <- FALSE
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  message(paste0('Evaluating model: ', model_name))
  
  dir.create(file.path(outdir, model_name))
  
  fit <- readRDS(file.path(fits[i]))
  d <- mcmcToDataframe(fit$mcmc)
  
  # trace plots
  tracedir <- file.path(outdir, model_name, 'trace')
  if(!dir.exists(tracedir) | overwrite){
    trace(fit, 
          tracedir = tracedir,
          vars = grep('hat', varnames(fit$mcmc), invert=T, value=T))
  }
  
  # fit plots
  outfile <- file.path(outdir, model_name, 'fit_pop_mean.jpg')
  if(!file.exists(outfile) | overwrite){
    dat <- plotDat(d, fit$md, obs='pop', hat='pop_hat', alpha=0.1)
    plotFit(dat, file=outfile)
  }
  
  outfile <- file.path(outdir, model_name, 'fit_pop_mean_zoom.jpg')
  if(!file.exists(outfile) | overwrite){
    dat <- plotDat(d, fit$md, obs='pop', hat='pop_hat', alpha=0.1)
    plotFit(dat, file=outfile, zoom=0.8)
  }
  
  outfile <- file.path(outdir, model_name, 'fit_pop_median.jpg')
  if(!file.exists(outfile) | overwrite){
    dat <- plotDat(d, fit$md, obs='pop', hat='pop_hat', alpha=0.1)
    plotFit(dat, predCol='median', file=outfile)
  }

  outfile <- file.path(outdir, model_name, 'fit_pop_median_zoom.jpg')
  if(!file.exists(outfile) | overwrite){
    dat <- plotDat(d, fit$md, obs='pop', hat='pop_hat', alpha=0.1)
    plotFit(dat, predCol='median', file=outfile, zoom=0.8)
  }
  
}


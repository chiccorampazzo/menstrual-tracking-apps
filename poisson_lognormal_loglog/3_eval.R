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

overwrite <- FALSE

# trace plots
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  message(paste0('Evaluating model: ', model_name))
  
  dir.create(file.path(outdir, model_name), showWarnings=F, recursive=T)
  
  fit <- readRDS(file.path(fits[i]))
  d <- mcmcToDataframe(fit$mcmc)
  
  # trace plots
  tracedir <- file.path(outdir, model_name, 'trace')
  if(!dir.exists(tracedir) | overwrite){
    trace(fit, 
          tracedir = tracedir,
          vars = grep('hat', varnames(fit$mcmc), invert=T, value=T))
  }
}

# fit plots
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  
  message(paste0('Evaluating model: ', model_name))
  
  dir.create(file.path(outdir, model_name, 'fit'), showWarnings=F, recursive=T)
  
  fit <- readRDS(file.path(fits[i]))
  
  for(eval_type in c('insample', 'outsample')){
    
    for(response in c('pop', 'installs')){
      
      if(eval_type == 'insample'){
        d <- mcmcToDataframe(fit$mcmc)
      } else {
        d <- readRDS(file.path(outdir, model_name, 'xval', response, 'd.rds'))  
      }
      
      if(response == 'pop') {
        hat <- paste0('pop_hat[',1:fit$md$n_countries,']')
        obs <- fit$md$pop
      } else {
        hat <- paste0('installs_hat[',1:fit$md$n_apps,',1]')
        obs <- fit$md$installs[,1]
      }
      
      basename <- file.path(outdir, model_name, 'fit', paste0(response,'_',eval_type,'_'))
      
      for(stat in c('mean', 'median')){
        
        outfile <- paste0(basename, stat, '.jpg')
        if(!file.exists(outfile) | overwrite){
          dat <- plotDat(d, obs=obs, hat=hat, alpha=0.1)
          plotFit(dat, 
                  file = outfile, 
                  predCol = stat, 
                  main = paste(eval_type, 'fit:', model_name, stat, response))
        }
        
        outfile <- paste0(basename, stat, '_zoom.jpg')
        if(!file.exists(outfile) | overwrite){
          dat <- plotDat(d, obs=obs, hat=hat, alpha=0.1)
          plotFit(dat, 
                  file = outfile, 
                  predCol = stat, 
                  zoom = 0.8, 
                  main = paste(eval_type, 'fit:', model_name, stat, response, '(zoom)'))
        }
      }
    }
  }
}




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
  message(paste0('Generating results: ', model_name))
  
  fit <- readRDS(file.path(fits[i]))
  d <- mcmcToDataframe(fit$mcmc)
  
  dir.create(file.path(outdir, model_name), recursive=T, showWarnings=F)
  
  # parameter summaries
  outfile <- file.path(outdir, model_name, 'summary.csv')
  if(!file.exists(outfile) | overwrite){
    fit.summary <- summary(fit$mcmc)
    fit.summary <- data.frame(cbind(fit.summary$statistics, fit.summary$quantiles))
    
    
    write.csv(fit.summary, file.path(outdir, model_name, 'summary.csv'))
  }
  
  # Chicco plot
  outfile <- file.path(outdir, model_name, 'chicco_plot.jpg')
  if(!file.exists(outfile) | overwrite){
    fit.summary <- read.csv(file.path(outdir, model_name, 'summary.csv'), 
                            stringsAsFactors=F,
                            row.names=1)
    plotChicco(fit.summary, 
               md = fit$md, 
               file = outfile)
  }
}
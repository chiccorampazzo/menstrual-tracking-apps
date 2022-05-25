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

# components of fit analysis
eval_types <- c('insample', 'outsample')
responses <- c('pop', 'installs')

# setup data.frame for residuals analysis
names_residual <- c('model', 'eval_type', 'response', 'stat', 'r2',
                    'bias', 'imprecision', 'inaccuracy',
                    'bias_std', 'imprecision_std', 'inaccuracy_std')
residual <- data.frame(matrix(NA, 
                              nrow=0, 
                              ncol=length(names_residual)))
names(residual) <- names_residual

# fit plots
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  
  message(paste0('Evaluating model: ', model_name))
  
  dir.create(file.path(outdir, model_name, 'fit'), showWarnings=F, recursive=T)
  
  fit <- readRDS(file.path(fits[i]))
  
  for(eval_type in eval_types){
    
    for(response in responses){
      
      if(eval_type == 'insample'){
        d <- mcmcToDataframe(fit$mcmc)
      } else {
        dfile <- file.path(outdir, model_name, 'xval', response, 'd.rds')
        if(!file.exists(dfile)) next
        d <- readRDS(dfile)  
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
        
        # analyze residuals
        hat_values <- apply(d[,hat], 2, ifelse(stat=='mean', mean, median))
        resid <- hat_values - obs
        resid_std <- resid / hat_values
        
        residual_row <- data.frame(matrix(NA, nrow=1, ncol=length(names_residual)))
        names(residual_row) <- names_residual
        
        residual_row$model <- model_name
        residual_row$eval_type <- eval_type
        residual_row$response <- response
        residual_row$stat <- stat
        residual_row$r2 <- cor(obs, hat_values, use='pairwise.complete.obs')^2
        residual_row$bias <- mean(resid, na.rm=T)
        residual_row$imprecision <- sd(resid, na.rm=T)
        residual_row$inaccuracy <- mean(abs(resid), na.rm=T)
        residual_row$bias_std <- mean(resid_std, na.rm=T)
        residual_row$imprecision_std <- sd(resid_std, na.rm=T)
        residual_row$inaccuracy_std <- mean(abs(resid_std), na.rm=T)
        
        residual <- rbind(residual, residual_row)
      }
    }
  }
}

write.csv(residual, file.path(outdir, 'residual_analysis.csv'), row.names=F)

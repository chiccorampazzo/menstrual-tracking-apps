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

# alpha-level (alpha=0.05 will provide 95% credible intervals)
alpha = 0.05

# overwrite previous results?
overwrite <- FALSE

# list models
fits <- list.files(file.path('out', 'mcmc'), full.names=T)

# trace plots and parameter summaries
for(i in 1:length(fits)){
  
  model_name <- tools::file_path_sans_ext(basename(fits[i]))
  message(paste0('Trace plots: ', model_name))
  
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
  
  # parameter summaries
  outfile <- file.path(outdir, model_name, 'summary.csv')
  if(!file.exists(outfile) | overwrite){
    fit.summary <- summary(fit$mcmc)
    fit.summary <- data.frame(cbind(fit.summary$statistics, fit.summary$quantiles))
    
    write.csv(fit.summary, file.path(outdir, model_name, 'summary.csv'))
  }
}

# setup fit analysis
eval_types <- c('insample', 'outsample')
responses <- c('pop', 'installs')

# setup data.frame for residuals analysis
names_residual <- c('model', 'response', 'eval_type', 'stat', 'r2',
                    'bias', 'bias_std', 
                    'imprecision', 'imprecision_std', 
                    'inaccuracy', 'inaccuracy_std')

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
  
  for(response in responses){
    
    for(eval_type in eval_types){
    
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
          dat <- plotDat(d, obs=obs, hat=hat, alpha=alpha)
          plotFit(dat, 
                  file = outfile, 
                  predCol = stat, 
                  main = paste(eval_type, 'fit:', model_name, stat, response))
        }
        
        outfile <- paste0(basename, stat, '_zoom.jpg')
        if(!file.exists(outfile) | overwrite){
          dat <- plotDat(d, obs=obs, hat=hat, alpha=alpha)
          plotFit(dat, 
                  file = outfile, 
                  predCol = stat, 
                  zoom = 0.8, 
                  main = paste(eval_type, 'fit:', model_name, stat, response, '(zoom)'))
        }
        
        # analyze residuals
        dat <- plotDat(d, obs=obs, hat=hat, alpha=alpha)
        dat$resid <- dat[,stat] - dat$obs
        dat$resid_std <- resid / dat$obs
        
        residual_row <- data.frame(matrix(NA, nrow=1, ncol=length(names_residual)))
        names(residual_row) <- names_residual
        
        residual_row$model <- model_name
        residual_row$eval_type <- eval_type
        residual_row$response <- response
        residual_row$stat <- stat
        residual_row$r2 <- cor(dat$obs, dat[,stat], use='pairwise.complete.obs')^2
        residual_row$bias <- mean(dat$resid, na.rm=T)
        residual_row$imprecision <- sd(dat$resid, na.rm=T)
        residual_row$inaccuracy <- mean(abs(dat$resid), na.rm=T)
        residual_row$bias_std <- mean(dat$resid_std, na.rm=T)
        residual_row$imprecision_std <- sd(dat$resid_std, na.rm=T)
        residual_row$inaccuracy_std <- mean(abs(dat$resid_std), na.rm=T)
        
        residual <- rbind(residual, residual_row)
      }
    }
  }
}

# save residuals analysis (bias, imprecision, inaccuracy)
write.csv(residual, file.path(outdir, 'residual_analysis.csv'), row.names=F)


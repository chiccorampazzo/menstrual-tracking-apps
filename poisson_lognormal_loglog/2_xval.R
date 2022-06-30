# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(runjags)
library(coda)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load functions
for(i in list.files('R')) source(file.path('R',i))

# user options
overwrite <- FALSE
nk <- 10
psrf_threshold <- 1.1

# output directory
outdir <- file.path('out')
dir.create(outdir, recursive=T, showWarnings=F)

# models
fits <- list.files(file.path('out', 'mcmc'), full.names=T)

# setup k folds
if(file.exists(file.path(outdir, 'xval_itest.rds'))){
  itest <- readRDS(file.path(outdir, 'xval_itest.rds'))
  nk <- length(itest[['country']])
} else {
  itest <- list()
  itest[['country']] <- ixval(n = readRDS(file.path(fits[1]))$md$n_countries, nk = nk)
  itest[['app']] <- ixval(n = readRDS(file.path(fits[1]))$md$n_apps, nk = nk)
  saveRDS(itest, file=file.path(outdir, 'xval_itest.rds'))
}

# cross-validate each model
# 1:length(fits)
for(ifit in 1:length(fits)){
  
  for(response in names(itest)){
    
    model_name <- tools::file_path_sans_ext(basename(fits[ifit]))
    
    # output directory
    xvaldir <- file.path(outdir, model_name, 'xval', response)
    dir.create(xvaldir, showWarnings=F, recursive=T)
    
    # load original fit model
    fit_orig <- readRDS(file.path(outdir, 'mcmc', paste0(model_name, '.rds')))
    
    for(k in 1:nk){
      
      outfile <- file.path(xvaldir, paste0('fit_xval', k, '.rds'))
      
      if(overwrite | !file.exists(outfile)){
        
        message(paste0('Cross-validating model: ', model_name , ' (k=', k, ')'))
        
        # parameters to monitor
        if(response == 'country'){
          monitor <- paste0('play_installs_hat[',itest[[response]][[k]],']')
        } else if(response == 'app'){
          monitor <- paste0('installs_hat[',itest[[response]][[k]],',1]')
        }

        # modify data
        md <- fit_orig$md
        if(response == 'country'){
          md$play_installs[itest[[response]][[k]]] <- NA
        } else if(response == 'app'){
          md$installs[itest[[response]][[k]], 1] <- NA
        }
        
        # initials
        init <- fit_orig$init

        # set seed
        set.seed(md$seed)
        
        # mcmc
        fit <- runjags::run.jags(model = 'model.jags.R',
                                 data = md,
                                 monitor = monitor,
                                 inits = init,
                                 n.chains = length(fit_orig$init),
                                 thin = fit_orig$thin,
                                 sample = fit_orig$sample,
                                 burnin = fit_orig$burnin - 1e3,
                                 adapt = 1e3,
                                 summarise = F,
                                 method = 'parallel')
        
        # check convergence
        psrf <- coda::gelman.diag(fit$mcmc, multivariate=F)
        
        # check psrf==NaN
        psrf_check <- !is.finite(psrf$psrf[,'Upper C.I.'])
        for(i in which(psrf_check)){
          message(paste0('WARNING: psrf is not finite for ', names(psrf_check)[i]))
        }

        # extend until converged
        extend_num <- 0
        while(max(psrf$psrf[,'Upper C.I.'], na.rm=T) > psrf_threshold & extend_num < 10){
          
          extend_num <- extend_num + 1
          
          message(paste0('Extending model: ', model_name, '(extension ', extend_num, ')'))
          
          print(psrf$psrf[psrf$psrf[,'Upper C.I.'] > psrf_threshold,])
          
          fit <- runjags::extend.jags(fit)
          
          psrf <- coda::gelman.diag(fit$mcmc, multivariate=F)
        }
        
        # save
        fit$init <- init
        fit$md <- md
        fit$extend <- extend_num
        fit$psrf <- psrf
        
        saveRDS(fit, outfile)
      }
    }
    
    # combine k-results into single dataframe
    d <- mcmcToDataframe(readRDS(file.path(xvaldir, 'fit_xval1.rds'))$mcmc)
    for(k in 2:nk){
      
      dk <- mcmcToDataframe(readRDS(file.path(xvaldir, paste0('fit_xval', k, '.rds')))$mcmc)
      
      if(nrow(d) < nrow(dk)){
        
        dpad <- matrix(NA, ncol=ncol(d), nrow=nrow(dk)-nrow(d))
        colnames(dpad) <- colnames(d)
        d <- rbind(d, dpad)
        
      } else if(nrow(dk) < nrow(d)){
        
        dpad <- matrix(NA, ncol=ncol(dk), nrow=nrow(d)-nrow(dk))
        colnames(dpad) <- colnames(dk)
        dk <- rbind(dk, dpad)
      }
      d <- cbind(d, dk)
    }
    saveRDS(d, file=file.path(xvaldir, 'd.rds'))
  }
}

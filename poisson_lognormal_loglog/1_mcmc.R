# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
# install.packages(c('runjags', 'coda', 'fastDummies'))
library(runjags)
library(coda)
library(fastDummies)

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load functions
for(i in list.files('R')) source(file.path('R',i))

# output directory
outdir <- file.path('out', 'mcmc')
dir.create(outdir, recursive=T, showWarnings=F)

# load data
appstore <- read.csv("../data/appstore_m1.csv", 
                     stringsAsFactors = F, 
                     row.names = 'app')
playstore <- read.csv("../data/playstore_m1.csv", 
                      stringsAsFactors = F, 
                      row.names = 'app')
covs <- read.csv("../data/covariates_installations.csv", 
                 stringsAsFactors = F, 
                 row.names = 'country')
sum_by_country <- read.csv("../data/sum.csv", 
                           stringsAsFactors = F)

# format covariates as numeric
continuous_covs <- which(names(covs)=='CPAnyP'):which(names(covs)=='gdp_pc')
for(i in continuous_covs){ 
  covs[,i] <- as.numeric(covs[,i]) 
}

# covariate correlations
write.csv(cor(covs[,continuous_covs], use='pairwise.complete.obs'),
          file.path(outdir, '..', 'covariate_correlations.csv'))

# covariate sets
cov_sets <- list(m1 = c('intercept', 'CPModP', 'CPTrad', 'UNMP', 'tfr'))
cov_sets[['m2']] <- c(cov_sets[['m1']], 'itu_internet', 'itu_mobile')
cov_sets[['m3']] <- c(cov_sets[['m2']], 'income_low', 'income_upmid', 'income_high')
cov_sets[['m4']] <- cov_sets[['m3']]
cov_sets[['m4bis']] <- cov_sets[['m3']]
cov_sets[['m5']] <- c(cov_sets[['m2']], 'gdp_pc')

# interactions
interact_list <- list()

interact_list[['m4']] <- list(c('income_low','itu_internet'),
                              c('income_upmid','itu_internet'),
                              c('income_high','itu_internet'))

interact_list[['m4bis']] <-  list(c('income_low','CPModP'),
                                  c('income_upmid','CPModP'),
                                  c('income_high','CPModP'))

interact_list[['m5']] <-  list(c('gdp_pc','UNMP'))


#---- run all models ----#
overwrite <- F
for(model_name in names(cov_sets)){
  
  outfile <- file.path(outdir, paste0(model_name, '.rds'))
  
  if(file.exists(outfile) & !overwrite) next
  
  message(paste('Running model:',model_name))
  
  # model data
  md <- datToList(appstore = appstore,
                  playstore = playstore,
                  covs = covs,
                  sum_by_country = sum_by_country,
                  cov_select = cov_sets[[model_name]],
                  interact_select = interact_list[[model_name]])
  
  # set seed
  set.seed(md$seed)
  
  # monitor
  monitor <- c('beta0', 'beta1', 'beta2',
               'alpha', 'sigma', 
               paste0('installs[',1:md$n_apps,',2]'), 
               'installs_country', 'rate')
  
  # monitor for eval
  monitor <- c(monitor, 
               'pop_hat', 
               paste0('installs_hat[',1:md$n_apps,',1]'))
  
  # initials
  init <- inits(md)
  
  # mcmc
  fit <- runjags::run.jags(model = 'model.jags.R',
                           data = md,
                           monitor = monitor,
                           inits = init,
                           n.chains = 3,
                           thin = 10,
                           sample = 10e3,
                           burnin = 50e3,
                           adapt = 1e3,
                           summarise = F,
                           method = 'parallel')
  
  # check convergence
  names.psrf <- coda::varnames(fit$mcmc)
  names.psrf <- names.psrf[!grepl('hat', names.psrf)]
  
  psrf <- coda::gelman.diag(fit$mcmc[,names.psrf], multivariate=F)

  # extend until converged
  extend_num <- 0
  psrf_threshold <- 1.2 # i possible, use 1.2 for testing and 1.1 for final models
  while(max(psrf$psrf[,'Upper C.I.']) > psrf_threshold & extend_num < 10){
    
    extend_num <- extend_num + 1
    
    message(paste0('Extending model: ', model_name, '(extension ', extend_num, ')'))
    
    print(psrf$psrf[psrf$psrf[,'Upper C.I.'] > psrf_threshold,])
    
    fit <- runjags::extend.jags(fit)
    
    psrf <- coda::gelman.diag(fit$mcmc[,names.psrf], multivariate=F)
  }
  
  # save
  fit$init <- init
  fit$md <- md
  fit$extend <- extend_num
  fit$psrf <- psrf
  
  saveRDS(fit, outfile)
}

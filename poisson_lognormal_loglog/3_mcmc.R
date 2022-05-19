# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# packages
library(runjags)
library(coda)

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'..'))

# model name
model_name <- 'poisson_lognormal_loglog'

# load data
dat <- readRDS(file.path(model_name, 'out', 'data.rds'))

# set seed
seed <- dat$seed
set.seed(seed)

# monitor
monitor <- c('beta0', 'beta1', 'beta2',
             'alpha', 'sigma', 
             paste0('installs[',1:dat$n_apps,',2]'), 
             'installs_country', 'rate')

# monitor for xval
monitor <- c(monitor, 
             'pop_hat', 
             paste0('installs_hat[',1:dat$n_apps,',1]'))

# initials
inits <- function(md, nchains=3){
  
  set.seed(md$seed)
  pi_init <- md$pi
  pi_init[pi_init==0] <- min(pi_init[pi_init!=0]) * 0.1
  pi_init <- pi_init / apply(pi_init, 1, sum)
  
  installs_init <- md$installs
  installs_init[,'appstore'] <- md$installs[,'playstore']
  installs_init[,'playstore'] <- NA
  
  result <- list()
  for(i in 1:nchains){
    i.result <- list()
    
    i.result$beta0 <- runif(1, 5, 7)
    i.result$beta1 <- runif(1, -1, 1)
    i.result$beta2 <- runif(1, -1, 1)
    
    i.result$sigma <- runif(1, 0, 1)
    i.result$alpha <- runif(md$n_X, -1, 1)

    # i.result$pi <- pi_init
    i.result$installs <- installs_init
    
    i.result$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    i.result$.RNG.seed = i * md$seed
    result[[i]] <- i.result
  }
  
  return(result)
}
init <- inits(dat)

# mcmc
fit <- run.jags(model = file.path(model_name, '2_model.jags.R'),
                data = dat,
                monitor = monitor,
                inits = init,
                n.chains = 3,
                thin = 10,
                sample = 10e3,
                burnin = 50e3,
                adapt = 1e3,
                summarise = F,
                method = 'parallel')

saveRDS(fit, file.path(model_name, 'out', 'mcmc.rds'))

# extend
extend_num <- 0
names.psrf <- varnames(fit$mcmc)
fit$psrf <- gelman.diag(fit$mcmc[,names.psrf], multivariate=F)
print(fit$psrf$psrf)

while(max(fit$psrf$psrf[,'Upper C.I.']) > 1.1){
  extend_num <- extend_num + 1
  print(paste('*** mcmc extension', extend_num, '***'))
  print(fit$psrf$psrf[fit$psrf$psrf[,'Upper C.I.']>1.1,])
  
  fit <- extend.jags(fit)
  
  fit$psrf <- gelman.diag(fit$mcmc[,names.psrf], multivariate=F)
}

# save
fit$extend_num <- extend_num
fit$init <- init
fit$dat <- dat
fit$seed <- seed

saveRDS(fit, file.path(model_name, 'out', 'mcmc.rds'))

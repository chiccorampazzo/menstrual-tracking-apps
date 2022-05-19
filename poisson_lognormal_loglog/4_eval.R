# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# packages
library(coda)

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'..'))

# model name
model_name <- 'poisson_lognormal_loglog'

# output directory
outdir <- file.path(model_name, 'out', 'eval')
dir.create(outdir, recursive=T, showWarnings=F)

# load data
dat <- readRDS('out/1_data.rds')
fit <- readRDS('out/3_fit.rds')

# trace plots
dir.create(file.path(outdir, 'trace'), recursive=T, showWarnings=F)
vars <- varnames(fit$mcmc)

jpeg(file.path(outdir, 'trace', paste('trace.%02d.jpg', sep='')), quality=100, height=1000, width=1000)
for (i in seq(1, length(vars), 3)){
  plot(fit$mcmc[,vars][,vars[i:min(length(vars), i+2)]], 
       cex = 1.25)
}
dev.off()

# data.frame
d <- jm$mcmc[[1]]
for (i in 2:length(jm$mcmc)){
  d <- rbind(d, jm$mcmc[[i]])
}
d <- as.data.frame(d)


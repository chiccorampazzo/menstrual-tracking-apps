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

# load functions
funs <- list.files('R')
for(fun in funs) source(file.path('R',fun))
rm(fun, funs)

# Read data
appstore <- read.csv("data/appstore_m1.csv", stringsAsFactors=F, row.names='app')
playstore <- read.csv("data/playstore_m1.csv", stringsAsFactors=F, row.names='app')
covs <- read.csv("data/covariates_installations.csv", stringsAsFactors=F, row.names='country')
sum_by_country_orig <- read.csv("data/sum.csv", stringsAsFactors=F)

# country and app names
countries <- sort(sum_by_country_orig$iso_2c)
apps <- sort(rownames(appstore))
  
# format sum_by_country
sum_by_country <- t(sum_by_country_orig[,paste0('app',1:length(apps))])
rownames(sum_by_country) <- rownames(appstore)
colnames(sum_by_country) <- sum_by_country_orig$iso_2c
sum_by_country[is.na(sum_by_country)] <- 0

# sort all data the same
appstore <- appstore[apps,]
playstore <- playstore[apps,]
sum_by_country <- sum_by_country[apps, countries]
covs <- covs[countries,]

# app installations
installs <- matrix(NA, nrow=length(apps), ncol=2)
colnames(installs) <- c('playstore', 'appstore')
rownames(installs) <- apps

installs[,'playstore'] <- playstore$maxInstall


# app reviews
reviews <- matrix(NA, nrow=length(apps), ncol=2)
colnames(reviews) <- c('playstore', 'appstore')
rownames(reviews) <- apps

reviews[,'playstore'] <- playstore$reviews
reviews[,'appstore'] <- appstore$reviews


# app ratings
ratings <- matrix(NA, nrow=nrow(appstore), ncol=2)
colnames(ratings) <- c('playstore', 'appstore')
rownames(ratings) <- apps

ratings[,'playstore'] <- playstore$ratings
ratings[,'appstore'] <- appstore$ratings


# pi (proportion of app installations in each country)
totals_by_app <- apply(sum_by_country, 1, sum, na.rm=T)
pi <- sum_by_country / totals_by_app


# women of reproductive age
population <- covs$popFage * 1e3
names(population) <- countries


# jags data
md <- list(n_apps = length(apps),
           n_countries = length(countries),
           n_stores = 2,
           installs = installs,
           reviews = reviews,
           ratings = ratings,
           pop = population,
           sum_by_country = sum_by_country,
           totals_by_app = totals_by_app
           )


# monitor
monitor <- c('beta0', 'beta1', 'beta2',
             'alpha0', 'sigma', 
             'pi', 'rate', 'installs_country')


# initials
inits <- function(md, nchains=3){
  
  pi_init <- pi
  pi_init[pi_init==0] <- min(pi_init[pi_init!=0]) * 0.1
  pi_init <- pi_init / apply(pi_init, 1, sum)
  
  installs_init <- md$installs
  installs_init[,'appstore'] <- installs_init[,'playstore']
  installs_init[,'playstore'] <- NA

  result <- list()
  for(i in 1:nchains){
    result[[i]] <- list()
    result[[i]][['pi']] <- pi_init
    result[[i]][['installs']] <- installs_init
  }
  
  return(result)
}
init <- inits(md)


# mcmc
jm <- run.jags(model = "./models/poisson_lognormal_loglog.jags.R",
               data = md,
               monitor = monitor,
               inits <- inits(md),
               n.chains = 3,
               thin = 1,
               sample = 20e3,
               burnin = 20e3,
               adapt = 2e3,
               summarise = F,
               method = 'parallel')

# data.frame
d <- jm$mcmc[[1]]
for (i in 2:length(jm$mcmc)){
  d <- rbind(d, jm$mcmc[[i]])
}
d <- as.data.frame(d)



for(k in 1:md$n_countries){
  traceplot(jm$mcmc[,paste0('p[',k,']')])
  hist(d[,paste0('p[',k,']')], main=countries[k])
}

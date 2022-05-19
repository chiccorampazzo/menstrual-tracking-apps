# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# packages
library(runjags)
library(coda)
library(fastDummies)

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


# prepare covs
covs$CPAnyP <- as.numeric(covs$CPAnyP)
covs$CPModP <- as.numeric(covs$CPModP)
covs$CPTrad <- as.numeric(covs$CPTrad)
covs$itu_female_m <- as.numeric(covs$itu_female_m)
covs$UNMP <- as.numeric(covs$UNMP)
covs$UNMModP <- as.numeric(covs$UNMModP)
covs <- dummy_cols(covs, select_columns = "region")

X <- data.frame(intercept = rep(1, nrow(covs)),
                CPAnyP = as.vector(scale(covs$CPAnyP)), 
                CPModP = as.vector(scale(covs$CPModP)),
                CPTrad = as.vector(scale(covs$CPTrad)),
                UNMP = as.vector(scale(covs$UNMP)), 
                UNMModP = as.vector(scale(covs$UNMModP)), 
                tfr = as.vector(scale(covs$tfr)), 
                itu_internet = as.vector(scale(covs$itu_internet)),
                itu_mobile = as.vector(scale(covs$itu_mobile)), 
                itu_female_m = as.vector(scale(covs$itu_female_m)),
                gender_in_index = as.vector(scale(covs$gender_in_index)), 
                adolescent_birth =as.vector(scale(covs$adolescent_birth)), 
                education_f =as.vector(scale(covs$education_f)), 
                labour_f_f =as.vector(scale(covs$labour_f_f)), 
                gdp_pc17 =as.vector(scale(covs$gdp_pc17)),
                gdp_pc =as.vector(scale(covs$gdp_pc)),

                region_Australia_New_Zealand = covs$`region_Australia and New Zealand`,
                region_Central_Asia = covs$`region_Central Asia`,
                region_Eastern_Asia = covs$`region_Eastern Asia`,
                region_Eastern_EU = covs$`region_Eastern Europe`,
                region_Latin_America_Caribbean = covs$`region_Latin America and the Caribbean`,
                region_Melanesia = covs$region_Melanesia,
                region_Northern_Africa = covs$`region_Northern Africa`,
                region_Northern_EU = covs$`region_Northern Europe`,
                #region_Polynesia = covs$region_Polynesia,
                region_South_Eastern_Asia = covs$`region_South-eastern Asia`,
                region_Southern_Asia = covs$`region_Southern Asia`,
                region_Southern_EU = covs$`region_Southern Europe`,
                region_Sub_Saharan_Africa = covs$`region_Sub-Saharan Africa`,
                region_Western_Asia = covs$`region_Western Asia`, 
                region_Western_EU = covs$`region_Western Europe`
)

X <- as.matrix(X)
n_X <- ncol(X)
rownames(X) <- countries


# missing data
i_xmiss <- which(apply(X, 2, function(x) any(is.na(x))))


# countries with zero comments+reviews for each app
nonzero_boolean <- apply(sum_by_country!=0, 1, function(x) which(x))
zero_boolean <- apply(sum_by_country==0, 1, function(x) which(x))

k_nonzero <- matrix(NA, 
                    nrow = length(apps), 
                    ncol = max(unlist(lapply(nonzero_boolean, length))),
                    dimnames = list(app=apps))
k_zero <- matrix(NA, 
                 nrow = length(apps), 
                 ncol = max(unlist(lapply(zero_boolean, length))),
                 dimnames = list(app=apps))

n_zero <- n_nonzero <- c()

for(i in 1:length(apps)){
  n_nonzero[i] <- length(nonzero_boolean[[i]])
  k_nonzero[names(nonzero_boolean)[i],1:n_nonzero[i]] <- nonzero_boolean[[i]]
  
  n_zero[i] <- length(zero_boolean[[i]])
  k_zero[names(zero_boolean)[i],1:n_zero[i]] <- zero_boolean[[i]]
}

# jags data
md <- list(n_apps = length(apps),
           n_countries = length(countries),
           n_stores = 2,
           n_X = n_X,
           installs = installs,
           reviews = reviews,
           ratings = ratings,
           pop = population,
           sum_by_country = sum_by_country,
           totals_by_app = totals_by_app,
           X = X,
           i_xmiss = i_xmiss,
           n_xmiss = length(i_xmiss),
           k_nonzero = k_nonzero,
           n_nonzero = n_nonzero,
           k_zero = k_zero,
           n_zero = n_zero
           )

# monitor
monitor <- c('beta0', 'beta1', 'beta2',
             'alpha', 'sigma', 
             'installs', 'pi', 'rate')


# initials
inits <- function(md, nchains=3){
  
  pi_init <- pi
  pi_init[pi_init==0] <- min(pi_init[pi_init!=0]) * 0.1
  pi_init <- pi_init / apply(pi_init, 1, sum)
  
  installs_init <- md$installs
  installs_init[,'appstore'] <- installs[,'playstore']
  installs_init[,'playstore'] <- NA
  
  result <- list()
  for(i in 1:nchains){
    i.result <- list()
    
    i.result$beta0 <- runif(1, 5, 7)
    i.result$beta1 <- runif(1, -1, 1)
    i.result$beta2 <- runif(1, -1, 1)
    
    i.result$sigma <- runif(1, 0, 1)
    i.result$alpha <- runif(md$n_X, -1, 1)

    i.result$pi <- pi_init
    i.result$installs <- installs_init
    
    i.result$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    i.result$.RNG.seed = i
    result[[i]] <- i.result
  }
  
  return(result)
}


# mcmc
jm <- run.jags(model = "./models/poisson_lognormal_loglog_dirch.jags.R",
               data = md,
               monitor = monitor,
               inits <- inits(md),
               n.chains = 3,
               thin = 1,
               sample = 10e3,
               burnin = 10e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')

# data.frame
d <- jm$mcmc[[1]]
for (i in 2:length(jm$mcmc)){
  d <- rbind(d, jm$mcmc[[i]])
}
d <- as.data.frame(d)



for(k in 1:md$n_countries){
  traceplot(jm$mcmc[,paste0('rate[',k,']')])
  hist(d[,paste0('rate[',k,']')], main=countries[k])
}

traceplot(jm$mcmc[,'rate[1]'])

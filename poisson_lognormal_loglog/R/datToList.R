library(fastDummies)
datToList <- function(appstore, playstore, covs, sum_by_country, 
                      cov_select=c('intercept'), interact_select=list()){
  
  # format sum_by_country
  sum_country <- t(sum_by_country[,paste0('app',1:nrow(appstore))])
  rownames(sum_country) <- rownames(appstore)
  colnames(sum_country) <- sum_by_country$iso_2c
  sum_country[is.na(sum_country)] <- 0
  
  # country and app names
  apps <- sort(rownames(appstore))
  countries <- colnames(sum_country)[apply(sum_country, 2, sum)>0]
  
  # sort all data the same
  appstore <- appstore[apps,]
  playstore <- playstore[apps,]
  sum_country <- sum_country[apps, countries]
  covs <- covs[countries,]
  
  # pi (proportion of app comments+reviews in each country)
  totals_by_app <- apply(sum_country, 1, sum, na.rm=T)
  pi <- sum_country / totals_by_app
  
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
  
  names(covs)[which(names(covs)=='Income.group')] <- 'income'
  
  covs <- fastDummies::dummy_cols(covs, select_columns = "region")
  covs <- fastDummies::dummy_cols(covs, select_columns = "income")
  
  # create X
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
                  region_Western_EU = covs$`region_Western Europe`,
                  
                  income_low = covs$`income_Low income`,
                  income_lowmid = covs$`income_Lower middle income`,
                  income_upmid = covs$`income_Upper middle income`,
                  income_high = covs$`income_High income`
  )
  
  # format X
  cov_names <- names(X)
  X <- as.matrix(X)

  # select covariates
  X <- X[,cov_select]
  n_covs <- length(cov_select)
  
  # identify missing data
  i_xcont <- which(apply(X, 2, function(x) length(unique(x)) > 2))
  i_xmiss <- which(apply(X, 2, function(x) any(is.na(x))))
  i_xmiss <- i_xmiss[i_xmiss %in% i_xcont]

  # define interactions
  n_interacts <- length(interact_select)
  if(n_interacts==0){
    interact <- 0
  } else {
    interact <- matrix(NA, nrow=n_interacts, ncol=2)
    for(i in 1:n_interacts){
      interact[i,] <- which(colnames(X) %in% interact_select[[i]])
    }
  }
  
  # jags data
  md <- list(n_apps = length(apps),
              n_countries = length(countries),
              n_stores = 2,
              n_covs = n_covs,
              installs = installs,
              reviews = reviews,
              ratings = ratings,
              pop = population,
              pi = pi,
              X = X,
              i_xmiss = i_xmiss,
              n_xmiss = length(i_xmiss),
              n_interacts = n_interacts,
              interact = interact,
              seed = runif(1, 1, 4242)
              )
  
  if(n_interacts==0) md$interactions <- matrix(0, nrow=md$n_countries, ncol=1)
  
  return(md)

}
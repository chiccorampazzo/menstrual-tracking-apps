model {
  
  ## Likelihood Poisson-LogNormal
  for(j in 1:123){
    
    # y = count of app installations
    y[j] ~ dpois(pop[j] * p[j])
    
    # p = rate of app installations per reproductive age woman
    p[j] ~ dlnorm(mu[j], pow(sigma, -2))
    
    # yhat = predicted count of app installations
    yhat[j] ~ dpois(pop[j] * phat[j])
    
    # phat = predicted rate of app installations per reproductive age woman
    phat[j] ~ dlnorm(mu[j], pow(sigma, -2))
    
    # mu = expected value of p
    mu[j] <- delta0 + 
      
      # Contraceptive prevalence: Any modern method (Percent)
      delta[1] * CPModP[j] +  
      # Contraceptive prevalence: Any traditional method (Percent)
      delta[2] * CPTrad[j] + 
      # "Unmet need for family planning: Any method (Percent)"     
      delta[3] * UNMP[j] + 
      # Total Fertility Rate
      delta[4] * tfr[j] +
      
      # ITU Percentage of Individuals Using Internet (2017)
      delta[5] * itu_internet[j] + 
      # ITU Mobile-cellular subscriptions per 100 inhabitants (2020)
      delta[6] * itu_mobile[j] +
      # ITU Percentage of Women Using the Internet (most recent year)
      #delta[7] * itu_female_m[j]+
      
      # Regions
      delta[7] * income_low_income[j] +
      delta[8] * income_uppmid_income[j] +
      delta[9] * income_high_income[j] 
    
    
    
    # Missing values
    CPModP[j] ~ dnorm(0, 1)
    CPTrad[j] ~ dnorm(0, 1)
    UNMP[j] ~ dnorm(0, 1)
    popFage[j] ~ dnorm(0, 1)
    tfr[j] ~ dnorm(0, 1)
    itu_internet[j] ~ dnorm(0, 1)
    labour_f_f[j] ~ dnorm(0, 1)
    adolescent_birth[j] ~ dnorm(0, 1)
    gdp_pc17[j] ~ dnorm(0, 1)
    gender_in_index[j] ~ dnorm(0, 1)
    itu_female_m[j] ~ dnorm(0, 1)
    education_f[j] ~ dnorm(0, 1)
    pop[j] ~ dlnorm(0, 1)
  } 
  
  ## Priors
  sigma ~ dunif(0, 10)
  
  delta0 ~ dnorm(0, pow(10, -2))
  
  for(k in 1:9){
    delta[k] ~ dnorm(0, pow(10, -2))
  }}
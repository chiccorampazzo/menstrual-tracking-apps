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
      delta[1] * CPModP[j] +   
      delta[2] * CPTrad[j] + 
      delta[3] * UNMP[j] + 
      delta[4] * tfr[j] + 
      delta[5] * popFage[j] + 
      
      delta[6] * itu_internet[j] + 
      delta[7] * itu_mobile[j] +
      
      delta[8] * itu_female_m[j]+
      
      # Gender Inequality Index
      delta[9] * gender_in_index[j] +
      
      #delta[10] * adolescent_birth[j] +
      
      # Female Population with at least some secondary education percentage ages 25 and older
      delta[10] * education_f[j]+ 
      
      # GDP
      delta[11] * gdp_pc17[j]+
      
      #-- Regions --#
      # Africa
      delta[12] * region_Northern_Africa[j] +
      delta[13] * region_Sub_Saharan_Africa[j] +
      
      # Americas
      delta[14] * region_Latin_America_Caribbean[j] +
      
      # Asia
      delta[15] * region_Central_Asia[j] +
      delta[16] * region_Eastern_Asia[j] +
      delta[17] * region_South_Eastern_Asia[j] +
      delta[18] * region_Southern_Asia[j] +
      delta[19] * region_Western_Asia[j] +
      
      # Europe
      delta[20] * region_Eastern_EU[j] +
      delta[21] * region_Northern_EU[j] +
      delta[22] * region_Southern_EU[j] +
      delta[23] * region_Western_EU[j] +
      
      # Oceania
      delta[24] * region_Australia_New_Zealand[j] +
      delta[25] * region_Melanesia[j] +
      # Missing Polynesia
      
      
      #-- Interactions --#
      
      # Africa
      delta[26] * region_Northern_Africa[j] * itu_internet[j] +
      delta[27] * region_Sub_Saharan_Africa[j] * itu_internet[j]+
    
      # Americas
      delta[28] * region_Latin_America_Caribbean[j] * itu_internet[j] +
    
      # Asia
      delta[28] * region_Central_Asia[j] * itu_internet[j]+
      delta[30] * region_Eastern_Asia[j] * itu_internet[j] +
      delta[31] * region_South_Eastern_Asia[j] * itu_internet[j] +
      delta[32] * region_Southern_Asia[j] * itu_internet[j]+
      delta[33] * region_Western_Asia[j] * itu_internet[j]+
    
      # Europe
      delta[34] * region_Eastern_EU[j] * itu_internet[j]+
      delta[35] * region_Northern_EU[j] * itu_internet[j]+
      delta[36] * region_Southern_EU[j] * itu_internet[j]+
      delta[37] * region_Western_EU[j] * itu_internet[j]+
    
      # Oceania
      delta[38] * region_Australia_New_Zealand[j] * itu_internet[j]+
      delta[39] * region_Melanesia[j] * itu_internet[j]
    
    
    # Missing values
    CPModP[j] ~ dlnorm(0, 1)
    CPTrad[j] ~ dlnorm(0, 1)
    UNMP[j] ~ dlnorm(0, 1)
    popFage[j] ~ dlnorm(0, 1)
    tfr[j] ~ dlnorm(0, 1)
    itu_internet[j] ~ dlnorm(0, 1)
    adolescent_birth[j] ~ dlnorm(0, 1)
    gdp_pc17[j] ~ dlnorm(0, 1)
    gender_in_index[j] ~ dlnorm(0, 1)
    itu_female_m[j] ~ dlnorm(0, 1)
    education_f[j] ~ dlnorm(0, 1)
    pop[j] ~ dlnorm(0, 1)
  } 
  
  ## Priors
  sigma ~ dunif(0, 10)
  
  delta0 ~ dnorm(0, pow(10, -2))
  
  for(k in 1:39){
    delta[k] ~ dnorm(0, pow(10, -2))
  }}
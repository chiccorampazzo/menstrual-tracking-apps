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
      #delta[5] * popFage[j] + 
      
      delta[5] * itu_internet[j] + 
      delta[6] * itu_mobile[j] +
      delta[7] * itu_female_m[j]+
      
      # Gender Inequality Index
      delta[8] * gender_in_index[j] +
      
      # Adolescent Birth
      delta[9] * adolescent_birth[j] +
      
      # Female Population with at least some secondary education percentage (% ages 25 and older)
      delta[10] * education_f[j]+ 
      
      # Labour force participation rate Female (% ages 15 and older)
      delta[11] * labour_f_f[j]+  

      # GDP
      delta[12] * gdp_pc17[j]+
      
      #-- Regions --#
      # Africa
      delta[13] * region_Northern_Africa[j] +
      delta[14] * region_Sub_Saharan_Africa[j] +
    
      # Americas
      delta[15] * region_Latin_America_Caribbean[j] +
      
      # Asia
      delta[16] * region_Central_Asia[j] +
      delta[17] * region_Eastern_Asia[j] +
      delta[18] * region_South_Eastern_Asia[j] +
      delta[19] * region_Southern_Asia[j] +
      delta[20] * region_Western_Asia[j] +
      
      # Europe
      delta[21] * region_Eastern_EU[j] +
      delta[22] * region_Northern_EU[j] +
      delta[23] * region_Southern_EU[j] +
      delta[24] * region_Western_EU[j] +
  
     # Oceania
      delta[25] * region_Australia_New_Zealand[j] +
      delta[26] * region_Melanesia[j] 
      # Missing Polynesia
      
      
    #-- Interactions --#
    
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
  
  for(k in 1:26){
    delta[k] ~ dnorm(0, pow(10, -2))
  }}
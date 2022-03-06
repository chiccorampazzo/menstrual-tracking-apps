model {
  
  ## Likelihood Negative Binomial
  for(j in 1:123){
    
    y[j] ~ dnegbin(p[j], r)
    
    yhat[j] ~ dnegbin(p[j], r)
    
    p[j] <- r / (r + mu[j]) 
    
    log(mu[j]) <- delta0 + 
      delta[1] * CPModP[j] +   
      delta[2] * CPTrad[j] + 
      delta[3] * UNMP[j] + 
      delta[4] * tfr[j] + 
      delta[5] * popFage[j] + 
      
      # ITU 
      
      delta[6] * itu_internet[j] + 
      delta[7] * itu_mobile[j] +
      #delta[8] * itu_female_m[j]+
      
      # Gender Inequality Index
      delta[8] * gender_in_index[j] +
      
      # Female Population with at least some secondary education percentage ages 25 and older
      delta[9] * education_f[j]+ 
      
      # Adolescent birth rate
      #delta[11] * adolescent_birth[j] +
      
      # GDP
      delta[10] * gdp_pc17[j]+
      
      
      #-- Regions --#
      # Africa
      delta[11] * region_Northern_Africa[j] +
      delta[12] * region_Sub_Saharan_Africa[j] +
      
      # Americas
      delta[13] * region_Latin_America_Caribbean[j] +
      
      # Asia
      delta[14] * region_Central_Asia[j] +
      delta[15] * region_Eastern_Asia[j] +
      delta[16] * region_South_Eastern_Asia[j] +
      delta[17] * region_Southern_Asia[j] +
      delta[18] * region_Western_Asia[j] +
      
      # Europe
      delta[19] * region_Eastern_EU[j] +
      delta[20] * region_Northern_EU[j] +
      delta[21] * region_Southern_EU[j] +
      delta[22] * region_Western_EU[j] +
      
      # Oceania
      delta[23] * region_Australia_New_Zealand[j] +
      delta[24] * region_Melanesia[j] +
      # Missing Polynesia
      
      
      #-- Interactions --#
      # Africa
      delta[25] * region_Northern_Africa[j] * itu_internet[j] +
      delta[26] * region_Sub_Saharan_Africa[j] * itu_internet[j]+
      
      # Americas
      delta[27] * region_Latin_America_Caribbean[j] * itu_internet[j] +
      
      # Asia
      delta[28] * region_Central_Asia[j] * itu_internet[j]+
      delta[29] * region_Eastern_Asia[j] * itu_internet[j] +
      delta[30] * region_South_Eastern_Asia[j] * itu_internet[j] +
      delta[31] * region_Southern_Asia[j] * itu_internet[j]+
      delta[32] * region_Western_Asia[j] * itu_internet[j]+
      
      # Europe
      delta[33] * region_Eastern_EU[j] * itu_internet[j]+
      delta[34] * region_Northern_EU[j] * itu_internet[j]+
      delta[35] * region_Southern_EU[j] * itu_internet[j]+
      delta[36] * region_Western_EU[j] * itu_internet[j]+
      
      # Oceania
      delta[37] * region_Australia_New_Zealand[j] * itu_internet[j]+
      delta[38] * region_Melanesia[j] * itu_internet[j]
    
    
    # Missing values
    CPModP[j] ~ dnorm(0, 1)
    CPTrad[j] ~ dnorm(0, 1)
    UNMP[j] ~ dnorm(0, 1)
    popFage[j] ~ dnorm(0, 1)
    tfr[j] ~ dnorm(0, 1)
    itu_internet[j] ~ dnorm(0, 1)
    #adolescent_birth[j] ~ dnorm(0, 1)
    gdp_pc17[j] ~ dnorm(0, 1)
    gender_in_index[j] ~ dnorm(0, 1)
    #itu_female_m[j] ~ dnorm(0, 1)
    education_f[j] ~ dnorm(0, 1)
    
  } 
  
  ## Priors
  r ~ dunif(0, 50)
  
  delta0 ~ dnorm(0, pow(10, -2))
  
  for(k in 1:38){
    delta[k] ~ dnorm(0, pow(10, -2))
  }
}
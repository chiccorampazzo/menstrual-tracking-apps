model {
  
  #---- likelihood 1 (process model): rates of fertility app usage by country ----#
  
  for(k in 1:n_countries){
    
    # likelihood: rate of app installations per woman (pop) by country (k)
    pop[k] ~ dpois(installs_country[k] / rate[k])
    rate[k] ~ dlnorm(mu[k], pow(sigma, -2))
    
    mu[k] <- effects[k] + sum(interactions[k,])
    
    # posterior predictions
    pop_hat[k] ~ dpois(installs_country[k] / rate_hat[k])
    rate_hat[k] ~ dlnorm(mu[k], pow(sigma, -2))
    
  }
  
  # covariate effects
  effects <- X %*% alpha[1:n_covs]
  
  # covariate interactions
  for(k in 1:n_countries){
    for(i in 1:n_interacts){
      interactions[k,i] <- alpha[n_covs+i] * X[k,interact[i,1]] * X[k,interact[i,2]]
    }
  }
  
  # missing values
  for(i in 1:n_xmiss){
    for(k in 1:n_countries){
      X[k, i_xmiss[i]] ~ dnorm(0, pow(1, -2))
    }
  }
  
  # priors
  sigma ~ dunif(0, 5)
  for(i in 1:(n_covs + n_interacts)) {
    alpha[i] ~ dnorm(0, pow(10, -2))
  }
  
  
  #---- likelihood 2: observations of app installations ----#
  
  for(i in 1:n_apps){
    for(j in 1:n_stores){
      
      # likelihood: installations per app from playstore (observed) and appstore (predicted)
      installs[i,j] ~ dpois(lambda[i,j])
      log(lambda[i,j]) <- beta0 + beta1 * log(reviews[i,j]) + beta2 * log(ratings[i,j]) 
      
      # posterior predictions
      installs_hat[i,j] ~ dpois(lambda[i,j])
    }
  }
  
  # priors
  beta0 ~ dnorm(0, pow(10, -2))
  beta1 ~ dnorm(0, pow(10, -2))
  beta2 ~ dnorm(0, pow(10, -2))
  
  
  #---- derived quantities: app installs by country (all stores and apps) ----#
  
  for(k in 1:n_countries){
    
    # total installs by country
    installs_country[k] <- sum(installs_app_country[1:n_apps, k])
    
    for(i in 1:n_apps){
      
      # installs per app per country
      installs_app_country[i,k] <- sum(installs[i, 1:n_stores]) * pi[i,k]
    }
  }
  
  # derived for posterior predictions
  for(k in 1:n_countries){
    installs_country_hat[k] <- sum(installs_app_country_hat[1:n_apps, k])
    for(i in 1:n_apps){
      installs_app_country_hat[i,k] <- sum(installs_hat[i, 1:n_stores]) * pi[i,k]
    }
  }  
}
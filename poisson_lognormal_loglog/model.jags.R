model {
  
  
  #---- model 1 (process model): rates of fertility app usage by country ----#
  
  for(k in 1:n_countries){
    
    # rate of app installations per woman (pop) by country (k)
    rate[k] ~ dlnorm(mu[k], pow(sigma, -2))
    rate_hat[k] ~ dlnorm(mu[k], pow(sigma, -2))

    mu[k] <- effects[k] + sum(interactions[k,])
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
  for(k in 1:n_countries){
    for(i in 1:n_xmiss){
      X[k, i_xmiss[i]] ~ dnorm(0, pow(1, -2))
    }
  }

  # priors
  sigma ~ dunif(0, 5)
  for(i in 1:(n_covs + n_interacts)) {
    alpha[i] ~ dnorm(0, pow(10, -2))
  }
  
  
  #---- model 2: total app installations per country ----#
  for(k in 1:n_countries){
    
    play_installs[k] ~ 
      dpois(max(0, pop[k] * rate[k] - sum(store_installs_app[1:n_apps, k])))
    
    play_installs_hat[k] ~ 
      dpois(max(0, pop[k] * rate_hat[k] - sum(store_installs_app_hat[1:n_apps, k])))
    
    # app store installs per app per country
    for(i in 1:n_apps){
      store_installs_app[i,k] <- installs[i, 2] * pi[i,k]
      store_installs_app_hat[i,k] <- installs_hat[i, 2] * pi[i,k]
    }
  }
  
  
  #---- model 3: installations by app and store ----#
  
  for(i in 1:n_apps){
    for(j in 1:n_stores){
      
      # installations per app from playstore (observed) and appstore (predicted)
      installs[i,j] ~ dpois(lambda[i,j]) # dnegbin(r / (r + lambda[i,j]), r)  
      installs_hat[i,j] ~ dpois(lambda[i,j]) 
      
      log(lambda[i,j]) <- beta0 + beta1 * log(reviews[i,j]) + beta2 * log(ratings[i,j]) 
    }
  }
  
  # priors
  beta0 ~ dnorm(0, pow(10, -2))
  beta1 ~ dnorm(0, pow(10, -2))
  beta2 ~ dnorm(0, pow(10, -2))
  # r ~ dunif(1, 100)
}

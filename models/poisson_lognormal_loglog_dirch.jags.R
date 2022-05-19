model {
  
  #---- likelihood 3: observations of app installations ----#
  
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
  

  #---- likelihood 2: observations of app feedback by country ----#
  
  for(i in 1:n_apps){
    
    # likelihood: proportion of comments+ratings by country for each app
    sum_by_country[i, 1:n_nonzero[i]] ~ 
      dmulti(pi[i, 1:n_nonzero[i]], totals_by_app[i])
    
    # posterior prediction
    sum_by_country_hat[i, 1:n_nonzero[i]] ~ 
      dmulti(pi[i, 1:n_nonzero[i]], totals_by_app[i])

    # priors
    pi[i, 1:n_nonzero[i]] ~ ddirch(rep(1, n_nonzero[i]))

  }
  
  
  #---- derived quantities: app installs by country (all stores and apps) ----#
  
  # installs per app per country
  for(i in 1:n_apps){
    for(j in 1:n_nonzero[i]){
      installs_app_country[i, k_nonzero[i,j]] <- 
        sum(installs[i, 1:n_stores]) * pi[i, 1:n_nonzero[i]]
    }
    for(j in 1:n_zero[i]){
      installs_app_country[i, k_zero[i,j]] <- 0
    }
  }
  
  # total installs by country
  for(k in 1:n_countries){
    installs_country[k] <- sum(installs_app_country[1:n_apps, k])
  }
  
  
  # derived for posterior predictions
  for(i in 1:n_apps){
    for(j in 1:n_nonzero[i]){
      installs_app_country_hat[i, k_nonzero[i,j]] <- 
        sum(installs_hat[i, 1:n_stores]) * pi[i, 1:n_nonzero[i]]
    }
    for(j in 1:n_zero[i]){
      installs_app_country_hat[i, k_zero[i,j]] <- 0
    }
  }
  for(k in 1:n_countries){
    installs_country_hat[k] <- sum(installs_app_country_hat[1:n_apps, k])
  }
  
  
  #---- likelihood 1 (process model): rates of fertility app usage by country ----#
  
  for(k in 1:n_countries){
    
    # likelihood: app installations per woman (rate) by country
    pop[k] ~ dpois(installs_country[k] / rate[k])
    rate[k] ~ dlnorm(mu[k], pow(sigma, -2))
    
    # posterior predictions
    pop_hat[k] ~ dpois(installs_country_hat[k] / rate_hat[k])
    rate_hat[k] ~ dlnorm(mu[k], pow(sigma, -2))
    
  }
  
  # regression
  mu <- X %*% alpha
  
  # missing values
  for(i in 1:n_xmiss){
    for(k in 1:n_countries){
      X[k, i_xmiss[i]] ~ dnorm(0, pow(1, -2))
    }
  }
  
  # priors
  sigma ~ dunif(0, 5)
  for(i in 1:n_X) {
    alpha[i] ~ dnorm(0, pow(5, -2))
  }
}


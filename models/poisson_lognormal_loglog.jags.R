model {
  
  ## likelihood 1: installations per app
  for(i in 1:n_apps){
    installs_hat[i] ~ dpois(lambda[i,1])
    for(j in 1:n_stores){
      installs[i,j] ~ dpois(lambda[i,j])
      log(lambda[i,j]) <- beta0 + beta1 * log(reviews[i,j]) + beta2 * log(ratings[i,j]) 
    }
  }
  
  # priors: likelihood 1
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  beta2 ~ dnorm(0,0.01)
  
  # derive total installs by country
  for(i in 1:n_apps){
    for(k in 1:n_countries){
      installs_app_country[i,k] <- sum(installs[i, 1:n_stores]) * pi[i,k]
    }
  }
  for(k in 1:n_countries){
    installs_country[k] <- sum(installs_app_country[1:n_apps,k])
  }
  
  ## likelihood 2: app installations per woman by country
  for(k in 1:n_countries){
    # installs_country[k] ~ dpois(pop[k] * p[k])
    pop[k] ~ dpois((installs_country[k]+1) / p[k])
    p[k] ~ dlnorm(mu[k], pow(sigma, -2))
    mu[k] = alpha0
  }
  
  # priors: likelihood 2
  sigma ~ dunif(0, 10)
  alpha0 ~ dnorm(0, pow(10, -2))
}
total <- runif(1, 1000, 5000)
part1 <- total * runif(1, 0 ,1)
part2 <- total - part1

part1_hat <- total * 1.01

total - part1
total - part1_hat

total * (1 - part1 / total)
total * (1 - part1_hat / total)


#---------------------#

n <- 1e5
mu <- 100
r <- 1

x <- rnbinom(n, 
             mu = mu, 
             size = r)
hist(x)

x <- rnbinom(n, 
             prob = r / (r + mu), 
             size = r)
hist(x)

#------------------#

pop = 1251
rate = 1.3
store_installs = pop * rate * 0.6

installs_country <- pop * rate
rho <- (installs_country - store_installs) / installs_country


installs_country * rho

pop * rate * rho

pop * rate * (installs_country - store_installs) / installs_country

(pop * rate) * ((pop * rate) - store_installs) / (pop * rate)

pop * rate - store_installs

#-----------------------#
i = 11 # 20
init[[2]]$installs[i,2] * md$pi[i,]


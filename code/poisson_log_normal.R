# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# packages
library(tidyverse)
library(readr)
library(scales)
library(rjags)
library(coda)
library(MCMCvis)
library(maps)
library(sf)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(fastDummies)
library(runjags)

#######################################################################################################################
# Read files
#######################################################################################################################

# working directory
#setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'..'))

# load functions
funs <- list.files('R')
for(fun in funs) source(file.path('R',fun))
rm(fun, funs)

# load data
data <- read_csv("data/covariates_installations.csv")
data$CPAnyP <- as.numeric(data$CPAnyP)
data$CPModP <- as.numeric(data$CPModP)
data$CPTrad <- as.numeric(data$CPTrad)
data$itu_female_m<-as.numeric(data$itu_female_m)
data$UNMP <- as.numeric(data$UNMP)
data$UNMModP <- as.numeric(data$UNMModP)

# data to list
data_list <- datToList(data)

# run model
jm <- run.jags(model = "./models/poisson_lognormal.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')

model.samples = jm$mcmc

summary1 <- summary(model.samples)$statistics
summary2 <- summary(model.samples)$quantiles
summary <- cbind(summary1,summary2)
summary <- data.frame(summary)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv <- gelman.diag(model.samples)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv$psrf[conv$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples[,!grepl('yhat', varnames(model.samples))])


#---- in-sample model fit ----#

# mcmc list to data.frame
d <- mcmcToDataframe(model.samples)

# data for plot
plotdat <- plotDat(d, data_list)

# fit plots
plotFit(plotdat)
plotFit(plotdat, zoom=0.8)

plotFit(plotdat, predCol='median')
plotFit(plotdat, predCol='median', zoom=0.8)



#-----------------------------#

delta <- summary[, -c(1:4)]
delta <- delta[c(2:28),]

delta <- delta %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta <- tibble::rownames_to_column(delta, "delta")

delta <- delta %>% mutate(delta = recode(delta, 
                                          delta0="Intercept", 
                                         'delta[1]'="CPMC", 
                                         'delta[2]'="CPTC",
                                         'delta[3]'="UNMP",
                                         'delta[4]'="TFR",
                                         'delta[5]'="ITU Internet",
                                         'delta[6]'="ITU Mobile", 
                                         'delta[7]'="ITU Mobile Female",
                                         'delta[8]'="Gender Inequality Index",
                                         'delta[9]'="Adolescent Birth Rate",
                                         'delta[10]'="Female Secondary Education",
                                         'delta[11]'="Female Labour Force",
                                         'delta[12]'="GDP",
                                         'delta[13]'="Northern Africa",
                                         'delta[14]'="Sub Saharan Africa",
                                         'delta[15]'="Latin America Caribbean",
                                         'delta[16]'="Central Asia", 
                                         'delta[17]'="Eastern Asia", 
                                         'delta[18]'="South Eastern Asia", 
                                         'delta[19]'="Southern Asia",
                                         'delta[20]'="Western Asia", 
                                         'delta[21]'="Eastern EU", 
                                         'delta[22]'="Northern EU",
                                         'delta[23]'="Southern EU",
                                         'delta[24]'="Western EU", 
                                         'delta[25]'="Australia New Zealand", 
                                         'delta[26]'="Melanesia"))

# Add rows for reference category
delta <- delta %>% add_row("delta"="Northern America", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)
#delta <- delta %>% add_row("delta"="UNMP - Northern America", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)


# Split into two groups
delta <- delta %>% mutate(group = recode(delta, 
                                         "Intercept"="group1", 
                                         "CPMC"="group1", 
                                         "CPTC"="group1",
                                         "UNMP"="group1",
                                         "TFR"="group1",
                                         "ITU Internet"="group1",
                                         "ITU Mobile"="group1", 
                                         "ITU Mobile Female"="group1",
                                         "Gender Inequality Index"="group1",
                                         "Adolescent Birth Rate"="group1",
                                         "Female Secondary Education"="group1",
                                         "Female Labour Force"="group1",
                                         "GDP"="group1",
                                         "Northern Africa"="group1",
                                         
                                         "Sub Saharan Africa"="group2",
                                         "Northern America"= "group2",
                                         "Latin America Caribbean"="group2",
                                         "Central Asia"="group2", 
                                         "Eastern Asia"="group2", 
                                         "South Eastern Asia"="group2", 
                                         "Southern Asia"="group2",
                                         "Western Asia"="group2", 
                                         "Eastern EU"="group2", 
                                         "Northern EU"="group2",
                                         "Southern EU"="group2",
                                         "Western EU"="group2", 
                                         "Australia New Zealand"="group2", 
                                         "Melanesia"="group2"))

delta <- delta %>% mutate(group_c = recode(delta, 
                                           "Intercept"="group1", 
                                           "CPMC"="group2", 
                                           "CPTC"="group2",
                                           "UNMP"="group2",
                                           "TFR"="group2",
                                           "ITU Internet"="group3",
                                           "ITU Mobile"="group3", 
                                           "ITU Mobile Female"="group3",
                                           "Gender Inequality Index"="group2",
                                           "Adolescent Birth Rate"="group2",
                                           "Female Secondary Education"="group2",
                                           "Female Labour Force"="group2",
                                           "GDP"="group2",
                                           "Northern Africa"="group4",
                                           
                                           "Sub Saharan Africa"="group4",
                                           "Northern America"= "group5",
                                           "Latin America Caribbean"="group4",
                                           "Central Asia"="group4", 
                                           "Eastern Asia"="group4", 
                                           "South Eastern Asia"="group4", 
                                           "Southern Asia"="group4",
                                           "Western Asia"="group4", 
                                           "Eastern EU"="group4", 
                                           "Northern EU"="group4",
                                           "Southern EU"="group4",
                                           "Western EU"="group4", 
                                           "Australia New Zealand"="group4", 
                                           "Melanesia"="group4"))






delta %>% 
  mutate(delta = fct_relevel(delta,
                             "Melanesia",
                             "Australia New Zealand", 
                             "Western EU", 
                             "Southern EU",
                             "Northern EU",
                             "Eastern EU", 
                             "Western Asia", 
                             "Southern Asia",
                             "South Eastern Asia", 
                             "Eastern Asia", 
                             "Central Asia", 
                             "Latin America Caribbean",
                             "Northern America",
                             "Sub Saharan Africa",
                             "Northern Africa",
                             "ITU Mobile Female",
                             "ITU Mobile", 
                             "ITU Internet",
                             "GDP",
                             "Adolescent Birth Rate",
                             "Female Labour Force",
                             "Female Secondary Education",
                             "Gender Inequality Index",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("black","#2888bc", "#bc2888" , "#88bc28", "grey"))+
  coord_flip()+
  xlab("") + ylab("")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")+
  facet_wrap(~group, scales="free_y")

ggsave("figures/model.pdf", width = 40, height = 28, units = "cm")







##################################################################################################################
##################################################################################################################
##################################################################################################################


# Include ITU Mobile Female


# Model
cat("
model {
  
  ## Likelihood Negative Binomial
    for(j in 1:133){
      y[j] ~ dnegbin(p[j],r)
      p[j] <- r/(r+mu[j]) 
      log(mu[j]) <- delta0 + delta1 * CPModP[j] +   delta2 * CPTrad[j] + delta3 * UNMP[j] + delta4 * tfr[j] + delta5 * popFage[j] + 
     
     # ITU
      delta6 * itu_internet[j] + delta7 * itu_mobile[j] + delta8 * itu_mobile_f[j]+
      
          
          # Regions
          
          # Africa
          
          delta9 * region_Northern_Africa[j] +
          delta10 * region_Sub_Saharan_Africa[j] +
          
          # Americas
          
          delta11 * region_Latin_America_Caribbean[j] +
          
          # Asia
          delta12 * region_Central_Asia[j] +
          delta13 * region_Eastern_Asia[j] +
          delta14 * region_South_Eastern_Asia[j] +
          delta15 * region_Southern_Asia[j] +
          delta16 * region_Western_Asia[j] +
        
          # Europe
          delta17 * region_Eastern_EU[j] +
          delta18 * region_Northern_EU[j] +
          delta19 * region_Southern_EU[j] +
          delta20 * region_Western_EU[j] +

          # Oceania

          delta21 * region_Australia_New_Zealand[j] +
          delta22 * region_Melanesia[j] +
          delta23 * region_Polynesia[j] +
      
          
          # Interactions
          
          
          # Africa
          
          delta24 * region_Northern_Africa[j] * UNMP[j] +
          delta25 * region_Sub_Saharan_Africa[j] * UNMP[j]+
          
          # Americas
          
          delta26 * region_Latin_America_Caribbean[j] * UNMP[j] +
          
          # Asia
          delta27 * region_Central_Asia[j] * UNMP[j]+
          delta28 * region_Eastern_Asia[j] * UNMP[j] +
          delta29 * region_South_Eastern_Asia[j] * UNMP[j] +
          delta30 * region_Southern_Asia[j] * UNMP[j]+
          delta31 * region_Western_Asia[j] * UNMP[j]+
          
          # Europe
          delta32 * region_Eastern_EU[j] * UNMP[j]+
          delta33 * region_Northern_EU[j] * UNMP[j]+
          delta34 * region_Southern_EU[j] * UNMP[j]+
          delta35 * region_Western_EU[j] * UNMP[j]+

          # Oceania

          delta36 * region_Australia_New_Zealand[j] *UNMP[j]+
          delta37 * region_Melanesia[j] *UNMP[j]+
          delta38 * region_Polynesia[j] *UNMP[j]
      

      
      # Missing values
      itu_internet[j] ~ dnorm(0, 1)
      itu_mobile_f[j] ~ dnorm(0, 1)
    } 
    
  ## Priors
    delta0 ~ dnorm(0,100)
    delta1 ~ dnorm(0,100)
    delta2 ~ dnorm(0,100)
    delta3 ~ dnorm(0,100)
    delta4 ~ dnorm(0,100)
    delta5 ~ dnorm(0,100)
    delta6 ~ dnorm(0,100)
    delta7 ~ dnorm(0,100)
    delta8 ~ dnorm(0,100)
    delta9 ~ dnorm(0,100)
    delta10 ~ dnorm(0,100)
    delta11 ~ dnorm(0,100)
    delta12 ~ dnorm(0,100)
    delta13 ~ dnorm(0,100)
    delta14 ~ dnorm(0,100)
    delta15 ~ dnorm(0,100)
    delta16 ~ dnorm(0,100)
    delta17 ~ dnorm(0,100)
    delta18 ~ dnorm(0,100)
    delta19 ~ dnorm(0,100)
    delta20 ~ dnorm(0,100)
    delta21 ~ dnorm(0,100)
    delta22 ~ dnorm(0,100)
    delta23 ~ dnorm(0,100)
    delta24 ~ dnorm(0,100)
    delta25 ~ dnorm(0,100)
    delta26 ~ dnorm(0,100)
    delta27 ~ dnorm(0,100)
    delta28 ~ dnorm(0,100)
    delta29 ~ dnorm(0,100)
    delta30 ~ dnorm(0,100)
    delta31 ~ dnorm(0,100)
    delta32 ~ dnorm(0,100)
    delta33 ~ dnorm(0,100)
    delta34 ~ dnorm(0,100)
    delta35 ~ dnorm(0,100)
    delta36 ~ dnorm(0,100)
    delta37 ~ dnorm(0,100)
    delta38 ~ dnorm(0,100)
    
    r ~ dunif(0,50)

}
", file="./models/negbin_regions_int.txt")

iterations <- 10000
burnin <- 1001
chains <- 3

model.fit <- jags.model(file="./models/negbin_regions_int.txt",
                        
                        data=list(
                          y=installation,
                          CPTrad=CPTrad,
                          UNMP=UNMP, 
                          UNMModP=UNMModP, 
                          tfr=tfr, 
                          popFage=ratiof, 
                          itu_internet=itu_internet,
                          itu_mobile=itu_mobile, 
                          itu_mobile_f=itu_mobile_f,
                          
                          region_Australia_New_Zealand=region_Australia_New_Zealand,
                          region_Central_Asia=region_Central_Asia,
                          region_Eastern_Asia=region_Eastern_Asia,
                          region_Eastern_EU=region_Eastern_EU,
                          region_Latin_America_Caribbean=region_Latin_America_Caribbean,
                          region_Melanesia=region_Melanesia,
                          region_Northern_Africa=region_Northern_Africa,
                          region_Northern_EU=region_Northern_EU,
                          region_Polynesia=region_Polynesia,
                          region_South_Eastern_Asia=region_South_Eastern_Asia,
                          region_Southern_Asia=region_Southern_Asia,
                          region_Southern_EU=region_Southern_EU,
                          region_Sub_Saharan_Africa=region_Sub_Saharan_Africa,
                          region_Western_Asia=region_Western_Asia, 
                          region_Western_EU=region_Western_EU
                          
                        ),
                        
                        n.chains = chains)

model.samples <- coda.samples(model.fit, c("delta0", "delta1", "delta2", "delta3", "delta4", "delta5", "delta6", "delta7", "delta8", "delta9", "delta10", "delta11", "delta12", 
                                           "delta13", "delta14", "delta15", "delta16", "delta17", "delta18", "delta19", "delta20", 
                                           "delta21", "delta22", "delta23", "delta24", "delta25", "delta26", "delta27", "delta28", "delta29", 
                                           "delta30", "delta31", "delta32", "delta33", "delta34", "delta35", "delta36", "delta37", "delta38"), n.iter=iterations, thin=10)

summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)


delta <- summary[, -c(1:4)]
delta <- delta %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta <- tibble::rownames_to_column(delta, "delta")

delta <- delta %>% mutate(delta = recode(delta, 
                                         delta0="Intercept", 
                                         delta1="CPMC", 
                                         delta2="CPTC",
                                         delta3="UNMP",
                                         delta4="TFR",
                                         delta5="Female 15-49/Population",
                                         delta6="ITU Internet",
                                         delta7="ITU Mobile", 
                                         delta8="ITU Mobile Female",
                                         delta9="Northern Africa",
                                         delta10="Sub Saharan Africa",
                                         delta11="Latin America Caribbean",
                                         delta12="Central Asia", 
                                         delta13="Eastern Asia", 
                                         delta14="South Eastern Asia", 
                                         delta15="Southern Asia",
                                         delta16="Western Asia", 
                                         delta17="Eastern EU", 
                                         delta18="Northern EU",
                                         delta19="Southern EU",
                                         delta20="Western EU", 
                                         delta21="Australia New Zealand", 
                                         delta22="Melanesia",
                                         delta23="Polynesia", 
                                         delta24="UNMP - Northern Africa", 
                                         delta25="UNMP - Sub Saharan Africa",
                                         delta26="UNMP - Latin America Caribbean",
                                         delta27="UNMP - Central Asia",
                                         delta28="UNMP - Eastern Asia",
                                         delta29="UNMP - South Eastern Asia", 
                                         delta30="UNMP - Southern Asia",
                                         delta31="UNMP - Western Asia",
                                         delta32="UNMP - Eastern EU",
                                         delta33="UNMP - Northern EU",
                                         delta34="UNMP - Southern EU",
                                         delta35="UNMP - Western EU",
                                         delta36="UNMP - Australia New Zealand", 
                                         delta37="UNMP - Melanesia", 
                                         delta38="UNMP - Polynesia"))

# Add rows for reference category
delta <- delta %>% add_row("delta"="Northern America", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)
delta <- delta %>% add_row("delta"="UNMP - Northern America", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)


# Split into two groups
delta <- delta %>% mutate(group = recode(delta, 
                                         "Intercept"="group1", 
                                         "CPMC"="group1", 
                                         "CPTC"="group1",
                                         "UNMP"="group1",
                                         "TFR"="group1",
                                         "Female 15-49/Population"="group1",
                                         "ITU Internet"="group1",
                                         "ITU Mobile"="group1", 
                                         "ITU Mobile Female"="group1",
                                         "Northern Africa"="group1",
                                         "Sub Saharan Africa"="group1",
                                         "Northern America"= "group1",
                                         "Latin America Caribbean"="group1",
                                         "Central Asia"="group1", 
                                         "Eastern Asia"="group1", 
                                         "South Eastern Asia"="group1", 
                                         "Southern Asia"="group1",
                                         "Western Asia"="group1", 
                                         "Eastern EU"="group1", 
                                         "Northern EU"="group1",
                                         "Southern EU"="group1",
                                         "Western EU"="group1", 
                                         "Australia New Zealand"="group2", 
                                         "Melanesia"="group2",
                                         "Polynesia"="group2", 
                                         "UNMP - Northern Africa"="group2", 
                                         "UNMP - Sub Saharan Africa"="group2",
                                         "UNMP - Northern America"="group2",
                                         "UNMP - Latin America Caribbean"="group2",
                                         "UNMP - Central Asia"="group2",
                                         "UNMP - Eastern Asia"="group2",
                                         "UNMP - South Eastern Asia"="group2", 
                                         "UNMP - Southern Asia"="group2",
                                         "UNMP - Western Asia"="group2",
                                         "UNMP - Eastern EU"="group2",
                                         "UNMP - Northern EU"="group2",
                                         "UNMP - Southern EU"="group2",
                                         "UNMP - Western EU"="group2",
                                         "UNMP - Australia New Zealand"="group2", 
                                         "UNMP - Melanesia"="group2", 
                                         "UNMP - Polynesia"="group2"))

delta <- delta %>% mutate(group_c = recode(delta, 
                                           "Intercept"="group1", 
                                           "CPMC"="group2", 
                                           "CPTC"="group2", 
                                           "UNMP"="group2", 
                                           "TFR"="group2", 
                                           "Female 15-49/Population"="group2", 
                                           "ITU Internet"="group3", 
                                           "ITU Mobile"="group3", 
                                           "ITU Mobile Female"="group3",
                                           "Western Asia"="group4", 
                                           "Western EU"="group4", 
                                           "Sub Saharan Africa"="group4", 
                                           "Southern EU"="group4", 
                                           "Southern Asia"="group4", 
                                           "South Eastern Asia"="group4", 
                                           "Polynesia"="group4", 
                                           "Northern EU"="group4", 
                                           "Northern Africa"="group4", 
                                           "Melanesia"="group4", 
                                           "Northern America"= "group6",
                                           "Latin America Caribbean"="group4", 
                                           "Eastern EU"="group4", 
                                           "Eastern Asia"="group4", 
                                           "Central Asia"="group4", 
                                           "Australia New Zealand"="group4", 
                                           "UNMP - Northern Africa"="group1", 
                                           "UNMP - Sub Saharan Africa"="group5",
                                           "UNMP - Northern America"="group6",
                                           "UNMP - Latin America Caribbean"="group1",
                                           "UNMP - Central Asia"="group1",
                                           "UNMP - Eastern Asia"="group1",
                                           "UNMP - South Eastern Asia"="group1", 
                                           "UNMP - Southern Asia"="group1",
                                           "UNMP - Western Asia"="group5",
                                           "UNMP - Eastern EU"="group1",
                                           "UNMP - Northern EU"="group1",
                                           "UNMP - Southern EU"="group1",
                                           "UNMP - Western EU"="group1",
                                           "UNMP - Australia New Zealand"="group1", 
                                           "UNMP - Melanesia"="group1", 
                                           "UNMP - Polynesia"="group1"))




delta %>%
  mutate(delta = fct_relevel(delta,
                             "UNMP - Polynesia",
                             "UNMP - Melanesia",
                             "UNMP - Australia New Zealand", 
                             "UNMP - Western EU",
                             "UNMP - Southern EU",
                             "UNMP - Northern EU",
                             "UNMP - Eastern EU",
                             "UNMP - Western Asia",
                             "UNMP - Southern Asia",
                             "UNMP - South Eastern Asia", 
                             "UNMP - Eastern Asia",
                             "UNMP - Central Asia",
                             "UNMP - Latin America Caribbean",
                             "UNMP - Sub Saharan Africa",
                             "UNMP - Northern Africa", 
                             "Polynesia", 
                             "Melanesia",
                             "Australia New Zealand", 
                             "Western EU", 
                             "Southern EU",
                             "Northern EU",
                             "Eastern EU", 
                             "Western Asia", 
                             "Southern Asia",
                             "South Eastern Asia", 
                             "Eastern Asia", 
                             "Central Asia", 
                             "Latin America Caribbean",
                             "Sub Saharan Africa",
                             "Northern Africa",
                             "ITU Mobile Female", 
                             "ITU Mobile", 
                             "ITU Internet",
                             "Female 15-49/Population",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, ),size=0.25) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`), size=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  coord_flip()+
  # Interactions
  annotate("rect", fill = "#bc5c28", alpha = 0.4, 
           xmin = 10.5, xmax =  11.5,
           ymin = -Inf, ymax = Inf) + 
  annotate("rect", fill = "#bc5c28", alpha = 0.4, 
           xmin = 12.5, xmax =  13.5,
           ymin = -Inf, ymax = Inf) + 
  # Regions
  annotate("rect", fill = "#88bc28", alpha = 0.4, 
           xmin = 13.5, xmax =  28.5,
           ymin = -Inf, ymax = Inf) + 
  # Tecnhology
  annotate("rect", fill = "#bc2888", alpha = 0.4, 
           xmin = 28.5, xmax =  30.5,
           ymin = -Inf, ymax = Inf) + 
  # Fertility
  annotate("rect", fill = "#2888bc", alpha = 0.4, 
           xmin = 30.5, xmax =  35.5,
           ymin = -Inf, ymax = Inf) + 
  xlab("") + ylab("")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        legend.position="top",
        legend.box="horizontal",
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank())




delta %>%
  mutate(delta = fct_relevel(delta,
                             "UNMP - Polynesia",
                             "UNMP - Melanesia",
                             "UNMP - Australia New Zealand", 
                             "UNMP - Western EU",
                             "UNMP - Southern EU",
                             "UNMP - Northern EU",
                             "UNMP - Eastern EU",
                             "UNMP - Western Asia",
                             "UNMP - Southern Asia",
                             "UNMP - South Eastern Asia", 
                             "UNMP - Eastern Asia",
                             "UNMP - Central Asia",
                             "UNMP - Latin America Caribbean",
                             "UNMP - Northern America",
                             "UNMP - Sub Saharan Africa",
                             "UNMP - Northern Africa", 
                             "Polynesia", 
                             "Melanesia",
                             "Australia New Zealand", 
                             "Western EU", 
                             "Southern EU",
                             "Northern EU",
                             "Eastern EU", 
                             "Western Asia", 
                             "Southern Asia",
                             "South Eastern Asia", 
                             "Eastern Asia", 
                             "Central Asia", 
                             "Latin America Caribbean",
                             "Northern America",
                             "Sub Saharan Africa",
                             "Northern Africa",
                             "ITU Mobile Female", 
                             "ITU Mobile", 
                             "ITU Internet",
                             "Female 15-49/Population",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("black","#2888bc", "#bc2888" , "#88bc28", "#bc5c28", "grey"))+
  coord_flip()+
  xlab("") + ylab("")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none")+
  facet_wrap(~group, scales="free_y")

ggsave("figures/model.pdf", width = 40, height = 28, units = "cm")
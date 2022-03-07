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

#######################################################################################################################
# Model 1
#######################################################################################################################


# run model
jm <- run.jags(model = "./models/poisson_lg_m1.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')


model.samples.m1 = jm$mcmc

summary1.m1 <- summary(model.samples.m1)$statistics
summary2.m1 <- summary(model.samples.m1)$quantiles
summary.m1 <- cbind(summary1.m1,summary2.m1)
summary.m1 <- data.frame(summary.m1)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv.m1 <- gelman.diag(model.samples.m1)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv.m1$psrf[conv.m1$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples.m1[,!grepl('yhat', varnames(model.samples.m1))])


#---- in-sample model fit ----#

# In-sample forecast is the process of formally evaluating the predictive capabilities of the models developed using observed data to see how effective the algorithms are in reproducing data. 

# mcmc list to data.frame
d.m1 <- mcmcToDataframe(model.samples.m1)

# data for plot
par(mfrow = c(2, 2))
plotdat.m1 <- plotDat(d.m1, data_list)

# fit plots
plotFit(plotdat.m1)
plotFit(plotdat.m1, zoom=0.8)

# 0.335

plotFit(plotdat.m1, predCol='median')
plotFit(plotdat.m1, predCol='median', zoom=0.8)

# 0.325

#-----------------------------#

delta_m1 <- summary.m1[, -c(1:4)]
delta_m1 <- delta_m1[c(2:6),]

delta_m1 <- delta_m1 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta_m1 <- tibble::rownames_to_column(delta_m1, "delta")

delta_m1 <- delta_m1 %>% mutate(delta = recode(delta, 
                                               delta0="Intercept", 
                                               'delta[1]'="CPMC", 
                                               'delta[2]'="CPTC",
                                               'delta[3]'="UNMP",
                                               'delta[4]'="TFR"))

delta_m1 <- delta_m1 %>% mutate(group_c = recode(delta, 
                                                 "Intercept"="group1", 
                                                 "CPMC"="group2", 
                                                 "CPTC"="group2",
                                                 "UNMP"="group2",
                                                 "TFR"="group2"))



delta_m1 %>% 
  mutate(delta = fct_relevel(delta,
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B"))+
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
        legend.position = "none")

ggsave("figures/model1.pdf", width = 40, height = 28, units = "cm")


delta_m1 <- delta_m1 %>% mutate(model="Model 1")

#######################################################################################################################
# Model 2
#######################################################################################################################


# run model
jm <- run.jags(model = "./models/poisson_lg_m2.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')

model.samples.m2 = jm$mcmc

summary1.m2 <- summary(model.samples.m2)$statistics
summary2.m2 <- summary(model.samples.m2)$quantiles
summary.m2 <- cbind(summary1.m2,summary2.m2)
summary.m2 <- data.frame(summary.m2)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv.m2 <- gelman.diag(model.samples.m2)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv.m2$psrf[conv.m2$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples.m2[,!grepl('yhat', varnames(model.samples.m2))])


#---- in-sample model fit ----#

# In-sample forecast is the process of formally evaluating the predictive capabilities of the models developed using observed data to see how effective the algorithms are in reproducing data. 

# mcmc list to data.frame
d.m2 <- mcmcToDataframe(model.samples.m2)

# data for plot
par(mfrow = c(2, 2))
plotdat.m2 <- plotDat(d.m2, data_list)

# fit plots
plotFit(plotdat.m2)
plotFit(plotdat.m2, zoom=0.8)

# 0.768

plotFit(plotdat.m2, predCol='median')
plotFit(plotdat.m2, predCol='median', zoom=0.8)

# 0.767

#-----------------------------#


delta_m2 <- summary.m2[, -c(1:4)]
delta_m2 <- delta_m2[c(2:8),]

delta_m2 <- delta_m2 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta_m2 <- tibble::rownames_to_column(delta_m2, "delta")

delta_m2 <- delta_m2 %>% mutate(delta = recode(delta, 
                                               delta0="Intercept", 
                                               'delta[1]'="CPMC", 
                                               'delta[2]'="CPTC",
                                               'delta[3]'="UNMP",
                                               'delta[4]'="TFR",
                                               'delta[5]'="ITU Internet",
                                               'delta[6]'="ITU Mobile"))


delta_m2 <- delta_m2 %>% mutate(group_c = recode(delta, 
                                                 "Intercept"="group1", 
                                                 "CPMC"="group2", 
                                                 "CPTC"="group2",
                                                 "UNMP"="group2",
                                                 "TFR"="group2",
                                                 "ITU Internet"="group3",
                                                 "ITU Mobile"="group3"))

delta_m2 %>% 
  mutate(delta = fct_relevel(delta,
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR",
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B"))+
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
        legend.position = "none")

ggsave("figures/model2.pdf", width = 40, height = 28, units = "cm")

delta_m2 <- delta_m2 %>% mutate(model="Model 2")


#######################################################################################################################
# Model 3
#######################################################################################################################


# run model
jm <- run.jags(model = "./models/poisson_lg_m3.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')


model.samples.m3 = jm$mcmc

summary1.m3 <- summary(model.samples.m3)$statistics
summary2.m3 <- summary(model.samples.m3)$quantiles
summary.m3 <- cbind(summary1.m3,summary2.m3)
summary.m3 <- data.frame(summary.m3)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv.m3 <- gelman.diag(model.samples.m3)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv.m3$psrf[conv.m3$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples.m3[,!grepl('yhat', varnames(model.samples.m3))])


#---- in-sample model fit ----#

# In-sample forecast is the process of formally evaluating the predictive capabilities of the models developed using observed data to see how effective the algorithms are in reproducing data. 

# mcmc list to data.frame
d.m3 <- mcmcToDataframe(model.samples.m3)

# data for plot
par(mfrow = c(2, 2))
plotdat.m3 <- plotDat(d.m3, data_list)

# fit plots
plotFit(plotdat.m3)
plotFit(plotdat.m3, zoom=0.8)

# 0.782

plotFit(plotdat.m3, predCol='median')
plotFit(plotdat.m3, predCol='median', zoom=0.8)

# 0.783

#-----------------------------#

delta_m3 <- summary.m3[, -c(1:4)]
delta_m3 <- delta_m3[c(2:11),]

delta_m3 <- delta_m3 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta_m3 <- tibble::rownames_to_column(delta_m3, "delta")

delta_m3 <- delta_m3 %>% mutate(delta = recode(delta, 
                                         delta0="Intercept", 
                                         'delta[1]'="CPMC", 
                                         'delta[2]'="CPTC",
                                         'delta[3]'="UNMP",
                                         'delta[4]'="TFR",
                                         'delta[5]'="ITU Internet",
                                         'delta[6]'="ITU Mobile", 
                                         'delta[7]'="Low-income economies",
                                         'delta[8]'="Upper-middle-income economies",
                                         'delta[9]'="High-income economies"))

# Add rows for reference category
delta_m3 <- delta_m3 %>% add_row("delta"="Lower-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)


delta_m3 <- delta_m3 %>% mutate(group_c = recode(delta, 
                                           "Intercept"="group1", 
                                           "CPMC"="group2", 
                                           "CPTC"="group2",
                                           "UNMP"="group2",
                                           "TFR"="group2",
                                           "ITU Internet"="group3",
                                           "ITU Mobile"="group3", 
                                           "Low-income economies"="group4",
                                           "Lower-middle-income economies"="group5",
                                           "Upper-middle-income economies"="group4",
                                           "High-income economies"="group4"))


delta_m3 %>% 
  mutate(delta = fct_relevel(delta,
                             "Low-income economies",
                             "Lower-middle-income economies",
                             "Upper-middle-income economies",
                             "High-income economies",
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B"))+
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
        legend.position = "none")

ggsave("figures/model3.pdf", width = 40, height = 28, units = "cm")


delta_m3 <- delta_m3 %>% mutate(model="Model 3")
#######################################################################################################################
# Models Figure
#######################################################################################################################

delta <- rbind(delta_m1, delta_m2, delta_m3)

delta %>% 
  mutate(delta = fct_relevel(delta,
                             "Low-income economies",
                             "Lower-middle-income economies",
                             "Upper-middle-income economies",
                             "High-income economies",
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c, shape=model),size=0.5, position = position_dodge(0.8)) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c, shape=model), size=0.75, position = position_dodge(0.8)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B"))+
  coord_flip()+
  xlab("") + ylab("")+
  guides(color = "none",
         shape = guide_legend(title.position = "top", title.hjust = 0.5,
                              override.aes = list(size=1.5, alpha = 0.8)))+
  scale_shape_manual(name = "Models",
                     labels = c("First", "Second", "Third"),
                     values = c(15, 16, 17)) + 
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=18),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top")

ggsave("figures/3models.pdf", width = 40, height = 28, units = "cm")

#######################################################################################################################
# Model 4
#######################################################################################################################


# run model
jm <- run.jags(model = "./models/poisson_lg_m4.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')


model.samples.m4 = jm$mcmc

summary1.m4 <- summary(model.samples.m4)$statistics
summary2.m4 <- summary(model.samples.m4)$quantiles
summary.m4 <- cbind(summary1.m4,summary2.m4)
summary.m4 <- data.frame(summary.m4)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv.m4 <- gelman.diag(model.samples.m4)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv.m4$psrf[conv.m4$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples.m4[,!grepl('yhat', varnames(model.samples.m4))])


#---- in-sample model fit ----#

# In-sample forecast is the process of formally evaluating the predictive capabilities of the models developed using observed data to see how effective the algorithms are in reproducing data. 

# mcmc list to data.frame
d.m4 <- mcmcToDataframe(model.samples.m4)

# data for plot
par(mfrow = c(2, 2))
plotdat.m4 <- plotDat(d.m4, data_list)

# fit plots
plotFit(plotdat.m4)
plotFit(plotdat.m4, zoom=0.8)

# 0.762

plotFit(plotdat.m4, predCol='median')
plotFit(plotdat.m4, predCol='median', zoom=0.8)

# 0.775

#-----------------------------#

delta_m4 <- summary.m4[, -c(1:4)]
delta_m4 <- delta_m4[c(2:14),]

delta_m4 <- delta_m4 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta_m4 <- tibble::rownames_to_column(delta_m4, "delta")

delta_m4 <- delta_m4 %>% mutate(delta = recode(delta, 
                                               delta0="Intercept", 
                                               'delta[1]'="CPMC", 
                                               'delta[2]'="CPTC",
                                               'delta[3]'="UNMP",
                                               'delta[4]'="TFR",
                                               'delta[5]'="ITU Internet",
                                               'delta[6]'="ITU Mobile", 
                                               'delta[7]'="Low-income economies",
                                               'delta[8]'="Upper-middle-income economies",
                                               'delta[9]'="High-income economies",
                                               
                                               'delta[10]'="ITU Internet - Low-income economies", 
                                               'delta[11]'="ITU Internet - Upper-middle-income economies",
                                               'delta[12]'="ITU Internet - High-income economies"))

# Add rows for reference category
delta_m4 <- delta_m4 %>% add_row("delta"="Lower-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)
delta_m4 <- delta_m4 %>% add_row("delta"="ITU Internet - Lower-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)


delta_m4 <- delta_m4 %>% mutate(group_c = recode(delta, 
                                                 "Intercept"="group1", 
                                                 "CPMC"="group2", 
                                                 "CPTC"="group2",
                                                 "UNMP"="group2",
                                                 "TFR"="group2",
                                                 "ITU Internet"="group3",
                                                 "ITU Mobile"="group3", 
                                                 "Low-income economies"="group4",
                                                 "Lower-middle-income economies"="group5",
                                                 "Upper-middle-income economies"="group4",
                                                 "High-income economies"="group4",
                                                 "ITU Internet - Low-income economies"="group6",
                                                 "ITU Internet - Upper-middle-income economies"="group6",
                                                 "ITU Internet - Lower-middle-income economies"="group5",
                                                 "ITU Internet - High-income economies"="group6"))


delta_m4 %>% 
  mutate(delta = fct_relevel(delta,
                             "ITU Internet - Low-income economies",
                             "ITU Internet - Lower-middle-income economies",
                             "ITU Internet - Upper-middle-income economies",
                             "ITU Internet - High-income economies",
                             "Low-income economies",
                             "Lower-middle-income economies",
                             "Upper-middle-income economies",
                             "High-income economies",
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B", "#7d7aff"))+
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
        legend.position = "none")

ggsave("figures/model4.pdf", width = 40, height = 28, units = "cm")


delta_m4 <- delta_m4 %>% mutate(model="Model 4")

#######################################################################################################################
# Models Figure
#######################################################################################################################

delta <- rbind(delta_m1, delta_m2, delta_m3, delta_m4)

delta %>% 
  mutate(delta = fct_relevel(delta,
                             "ITU Internet - Low-income economies",
                             "ITU Internet - Lower-middle-income economies",
                             "ITU Internet - Upper-middle-income economies",
                             "ITU Internet - High-income economies",
                             "Low-income economies",
                             "Lower-middle-income economies",
                             "Upper-middle-income economies",
                             "High-income economies",
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c, shape=model),size=0.5, position = position_dodge(0.8)) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c, shape=model), size=0.75, position = position_dodge(0.8)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B",  "#7d7aff" ))+
  coord_flip()+
  xlab("") + ylab("")+
  guides(color = "none",
         shape = guide_legend(title.position = "top", title.hjust = 0.5,
                              override.aes = list(size=1.5, alpha = 0.8)))+
  scale_shape_manual(name = "Models",
                     labels = c("First", "Second", "Third", "Thourth"),
                     values = c(15, 16, 17, 18)) + 
  theme_minimal()+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title=element_text(size=18),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "top")

ggsave("figures/4models.pdf", width = 40, height = 28, units = "cm")

#######################################################################################################################
# Model 5 - aka Model 4 BIS
#######################################################################################################################


# run model
jm <- run.jags(model = "./models/poisson_lg_m4bis.jags.R",
               data = data_list,
               monitor = c("sigma", "delta0", "delta", "yhat"),
               n.chains = 3,
               thin = 10,
               sample = 10e3,
               burnin = 1e3,
               adapt = 1e3,
               summarise = F,
               method = 'parallel')


model.samples.m5 = jm$mcmc

summary1.m5 <- summary(model.samples.m5)$statistics
summary2.m5 <- summary(model.samples.m5)$quantiles
summary.m5 <- cbind(summary1.m5,summary2.m5)
summary.m5 <- data.frame(summary.m5)


#---- convergence ----#

# Gelman-Rubin convergence diagnostic
conv.m5 <- gelman.diag(model.samples.m5)

# subset parameters not converged (note: 1.2 is a less conservative threshold that can be used)
conv.m5$psrf[conv.m5$psrf[,'Upper C.I.'] > 1.1,]

# trace plots (for all parameters except yhat)
traceplot(model.samples.m5[,!grepl('yhat', varnames(model.samples.m5))])


#---- in-sample model fit ----#

# In-sample forecast is the process of formally evaluating the predictive capabilities of the models developed using observed data to see how effective the algorithms are in reproducing data. 

# mcmc list to data.frame
d.m5 <- mcmcToDataframe(model.samples.m5)

# data for plot
par(mfrow = c(2, 2))
plotdat.m5 <- plotDat(d.m5, data_list)

# fit plots
plotFit(plotdat.m5)
plotFit(plotdat.m5, zoom=0.8)

# 0.774

plotFit(plotdat.m5, predCol='median')
plotFit(plotdat.m5, predCol='median', zoom=0.8)

# 0.779

#-----------------------------#

delta_m5 <- summary.m5[, -c(1:4)]
delta_m5 <- delta_m5[c(2:14),]

delta_m5 <- delta_m5 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select('2.5%', '25%', '50%','75%','97.5%')

delta_m5 <- tibble::rownames_to_column(delta_m5, "delta")

delta_m5 <- delta_m5 %>% mutate(delta = recode(delta, 
                                               delta0="Intercept", 
                                               'delta[1]'="CPMC", 
                                               'delta[2]'="CPTC",
                                               'delta[3]'="UNMP",
                                               'delta[4]'="TFR",
                                               'delta[5]'="ITU Internet",
                                               'delta[6]'="ITU Mobile", 
                                               'delta[7]'="Low-income economies",
                                               'delta[8]'="Upper-middle-income economies",
                                               'delta[9]'="High-income economies",
                                               
                                               'delta[10]'="CPMC - Low-income economies", 
                                               'delta[11]'="CPMC - Upper-middle-income economies",
                                               'delta[12]'="CPMC - High-income economies"))

# Add rows for reference category
delta_m5 <- delta_m5 %>% add_row("delta"="Lower-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)
delta_m5 <- delta_m5 %>% add_row("delta"="CPMC - Lower-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)


delta_m5 <- delta_m5 %>% mutate(group_c = recode(delta, 
                                                 "Intercept"="group1", 
                                                 "CPMC"="group2", 
                                                 "CPTC"="group2",
                                                 "UNMP"="group2",
                                                 "TFR"="group2",
                                                 "ITU Internet"="group3",
                                                 "ITU Mobile"="group3", 
                                                 "Low-income economies"="group4",
                                                 "Lower-middle-income economies"="group5",
                                                 "Upper-middle-income economies"="group4",
                                                 "High-income economies"="group4",
                                                 "CPMC - Low-income economies"="group6",
                                                 "CPMC - Upper-middle-income economies"="group6",
                                                 "CPMC - Lower-middle-income economies"="group5",
                                                 "CPMC - High-income economies"="group6"))


delta_m5 %>% 
  mutate(delta = fct_relevel(delta,
                             "CPMC - Low-income economies",
                             "CPMC - Lower-middle-income economies",
                             "CPMC - Upper-middle-income economies",
                             "CPMC - High-income economies",
                             "Low-income economies",
                             "Lower-middle-income economies",
                             "Upper-middle-income economies",
                             "High-income economies",
                             "ITU Mobile", 
                             "ITU Internet",
                             "TFR", 
                             "UNMP",
                             "CPTC", 
                             "CPMC", 
                             "Intercept")) %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_c),size=0.5) +
  geom_pointrange(mapping=aes(x=delta, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_c), size=0.75) +
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B", "#7d7aff"))+
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
        legend.position = "none")

ggsave("figures/model5.pdf", width = 40, height = 28, units = "cm")



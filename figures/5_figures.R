# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(tidyverse)
library(viridis)


# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read summary files

m1 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out/m1/summary.csv")
m2 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out/m2/summary.csv")
m3 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out//m3/summary.csv")
m4 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out//m4/summary.csv")
m5 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out//m5/summary.csv")
m6 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out//m6/summary.csv")
m7 <- read_csv("~/Desktop/GitHub/fertility-apps/out/out//m7/summary.csv")


# Add some columns and merge

m1 <- m1 %>% mutate(model="Model 1")

m1 <- m1 %>% mutate(`...1` = recode(`...1`, 
                              'alpha[1]'="Intercept", 
                              'alpha[2]'="CPMC", 
                              'alpha[3]'="CPTC",
                              'alpha[4]'="UNMP",
                              'alpha[5]'="TFR"))


m2 <- m2 %>% mutate(model="Model 2")

m2 <- m2 %>% mutate(`...1` = recode(`...1`, 
                              'alpha[1]'="Intercept", 
                              'alpha[2]'="CPMC", 
                              'alpha[3]'="CPTC",
                              'alpha[4]'="UNMP",
                              'alpha[5]'="TFR",
                              'alpha[6]'="ITU Internet",
                              'alpha[7]'="ITU Mobile"))

m3 <- m3 %>% add_row(`...1`="HIC", "X2.5."=0,  "X25."=0,   "X50."=0,   "X75."=0,   "X97.5."=0)
m3 <- m3 %>% mutate(model="Model 3")

m3 <- m3 %>% mutate(`...1` = recode(`...1`, 
                              'alpha[1]'="Intercept", 
                              'alpha[2]'="CPMC", 
                              'alpha[3]'="CPTC",
                              'alpha[4]'="UNMP",
                              'alpha[5]'="TFR",
                              'alpha[6]'="ITU Internet",
                              'alpha[7]'="ITU Mobile", 
                              'alpha[8]'="LIC",
                              'alpha[9]'="LMIC",
                              'alpha[10]'="MIC"))

m4 <- m4 %>% add_row(`...1`="HIC", "X2.5."=0,  "X25."=0,   "X50."=0,   "X75."=0,   "X97.5."=0)
m4 <- m4 %>% mutate(model="Model 4")

m4 <- m4 %>% mutate(`...1` = recode(`...1`, 
                              'alpha[1]'="Intercept", 
                              'alpha[2]'="CPMC", 
                              'alpha[3]'="CPTC",
                              'alpha[4]'="UNMP",
                              'alpha[5]'="TFR",
                              'alpha[6]'="ITU Internet",
                              'alpha[7]'="ITU Mobile", 
                              'alpha[8]'="LIC",
                              'alpha[9]'="LMIC",
                              'alpha[10]'="MIC", 
                              'alpha[11]'="LIC x CPMC",
                              'alpha[12]'="LMIC x CPMC",
                              'alpha[13]'="MIC x CPMC"))

m5 <- m5 %>% add_row(`...1`="HIC", "X2.5."=0,  "X25."=0,   "X50."=0,   "X75."=0,   "X97.5."=0)
m5 <- m5 %>% mutate(model="Model 5")

m5 <- m5 %>% mutate(`...1` = recode(`...1`, 
                                'alpha[1]'="Intercept", 
                                'alpha[2]'="CPMC", 
                                'alpha[3]'="CPTC",
                                'alpha[4]'="UNMP",
                                'alpha[5]'="TFR",
                                'alpha[6]'="ITU Internet",
                                'alpha[7]'="ITU Mobile", 
                                'alpha[8]'="LIC",
                                'alpha[9]'="LMIC",
                                'alpha[10]'="MIC", 
                                'alpha[11]'="LIC x UNMP",
                                'alpha[12]'="LMIC x UNMP",
                                'alpha[13]'="MIC x UNMP"))

m6 <- m6 %>% add_row(`...1`="HIC", "X2.5."=0,  "X25."=0,   "X50."=0,   "X75."=0,   "X97.5."=0)
m6 <- m6 %>% mutate(model="Model 6")

m6 <- m6 %>% mutate(`...1` = recode(`...1`, 
                                      'alpha[1]'="Intercept", 
                                      'alpha[2]'="CPMC", 
                                      'alpha[3]'="CPTC",
                                      'alpha[4]'="UNMP",
                                      'alpha[5]'="TFR",
                                      'alpha[6]'="ITU Internet",
                                      'alpha[7]'="ITU Mobile", 
                                      'alpha[8]'="LIC",
                                      'alpha[9]'="LMIC",
                                      'alpha[10]'="MIC", 
                                      'alpha[11]'="LIC x TFR",
                                      'alpha[12]'="LMIC x TFR",
                                      'alpha[13]'="MIC x TFR"))

m7 <- m7 %>% add_row(`...1`="HIC", "X2.5."=0,  "X25."=0,   "X50."=0,   "X75."=0,   "X97.5."=0)
m7 <- m7 %>% mutate(model="Model 7")

m7 <- m7 %>% mutate(`...1` = recode(`...1`, 
                                'alpha[1]'="Intercept", 
                                'alpha[2]'="CPMC", 
                                'alpha[3]'="CPTC",
                                'alpha[4]'="UNMP",
                                'alpha[5]'="TFR",
                                'alpha[6]'="ITU Internet",
                                'alpha[7]'="ITU Mobile", 
                                'alpha[8]'="LIC",
                                'alpha[9]'="LMIC",
                                'alpha[10]'="MIC", 
                                'alpha[11]'="LIC x ITU Internet",
                                'alpha[12]'="LMIC x ITU Internet",
                                'alpha[13]'="MIC x ITU Internet"))


m <- rbind(m1, m2, m3, m4, m5, m6, m7)

m <- m%>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
  select(`...1`,'2.5%', '25%', '50%','75%','97.5%', 'model')

# Recode names of Rows
unique(m$X1)

m <- m %>% mutate(X1=`...1`)

#m <- m %>% filter(X1=="alpha[1]" | X1=="alpha[2]" | X1=="alpha[3]" | X1=="alpha[4]" | X1=="alpha[5]" | X1=="alpha[6]" | X1=="alpha[7]" | X1=="alpha[8]" | X1=="alpha[9]" | X1=="alpha[10]" | X1=="alpha[11]" | X1=="alpha[12]" | X1=="alpha[13]")
m <- m %>% filter(X1=="Intercept" | X1=="CPMC" | X1=="CPTC" | X1=="UNMP" | X1=="TFR" | X1=="ITU Internet" | X1=="ITU Mobile" |
                  X1=="LIC" |  X1=="MIC" | X1=="HIC" | 
                  # Intercations 
                  X1=="LMIC" |  X1=="LMIC x CPMC" | X1=="LMIC x UNMP" | X1=="LMIC x TFR" | X1=="LMIC x ITU Internet" |
                  X1=="LIC x CPMC"  | X1=="MIC x CPMC" |
                  X1=="LIC x UNMP"  | X1=="MIC x UNMP" |
                  X1=="LIC x TFR"  | X1=="MIC x TFR" |
                  X1=="LIC x ITU Internet"  | X1=="MIC x ITU Internet")



#'alpha[11]',
#'alpha[12]',
#'alpha[13]'

# Add rows for reference category
#m <- m %>% add_row(`X1`="Upper x MIC", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)

unique(m$X1)

m <- m %>% mutate(group_c = recode(X1, 
                                                 "Intercept"="group1", 
                                                 "CPMC"="group2", 
                                                 "CPTC"="group2",
                                                 "UNMP"="group2",
                                                 "TFR"="group2",
                                                 "ITU Internet"="group3",
                                                 "ITU Mobile"="group3", 
                                   
                                                 "LIC"="group4",
                                                 "LMIC"="group4",
                                                 "MIC"="group4",
                                                 "HIC"="group5",
                                   
                                                  "LIC x CPMC" ="group6", 
                                                  "LMIC x CPMC"="group6",
                                                  "MIC x CPMC"= "group6",
                                   
                                                  "LIC x UNMP" ="group6", 
                                                  "LMIC x UNMP"="group6",
                                                  "MIC x UNMP"= "group6",
                                   
                                                  "LIC x TFR" ="group6", 
                                                  "LMIC x TFR"="group6",
                                                  "MIC x TFR"= "group6",
                                   
                                                 "LIC x ITU Internet" ="group6", 
                                                 "LMIC x ITU Internet"="group6",
                                                 "MIC x ITU Internet"= "group6",
                                   
                                                  ))

m <- m %>% mutate(group_2 = recode(X1, 
                                   "Intercept"="Intercept", 
                                   "CPMC"="Family Planning Indicators", 
                                   "CPTC"="Family Planning Indicators",
                                   "UNMP"="Family Planning Indicators",
                                   "TFR"="Family Planning Indicators",
                                   "ITU Internet"="Technology Indicators",
                                   "ITU Mobile"="Technology Indicators", 
                                   
                                   "LIC"="Regions Indicators",
                                   "LMIC"="Regions Indicators",
                                   "MIC"="Regions Indicators",
                                   "HIC"="Reference Category",
                                   
                                   "LIC x CPMC" ="Interactions", 
                                   "LMIC x CPMC"="Interactions",
                                   "MIC x CPMC"= "Interactions",
                                   
                                   "LIC x UNMP" ="Interactions", 
                                   "LMIC x UNMP"="Interactions",
                                   "MIC x UNMP"= "Interactions",
                                   
                                   "LIC x TFR" ="Interactions", 
                                   "LMIC x TFR"="Interactions",
                                   "MIC x TFR"= "Interactions",
                                   
                                   "LIC x ITU Internet" ="Interactions", 
                                   "LMIC x ITU Internet"="Interactions",
                                   "MIC x ITU Internet"= "Interactions"
                                   
))


unique(m$X1)

m$X1 <- as.factor(m$X1)
m$group_c <- as.factor(m$group_c)




m$group_2 <- as.factor(m$group_2)


d=data.frame(x=c(4.5, 6.5, 10.5), y=c(4.5, 6.5, 10.5))

show_col(viridis(10))

f1<-m %>% 
  mutate('X1' = fct_relevel(X1,
                            
                            "LIC x ITU Internet",
                            "LMIC x ITU Internet",
                            "MIC x ITU Internet",
                            
                            "LIC x TFR",
                            "LMIC x TFR",
                            "MIC x TFR",
                            
                            "LIC x UNMP",
                            "LMIC x UNMP",
                            "MIC x UNMP",
                            
                            "LIC x CPMC",
                            "LMIC x CPMC",
                            "MIC x CPMC",
                            
                            "LIC",
                            "LMIC",
                            "MIC",
                            "HIC",
                            
                            "ITU Mobile", 
                            "ITU Internet",
                            "TFR", 
                            "UNMP",
                            "CPTC", 
                            "CPMC", 
                            "Intercept")) %>%
  mutate(group_2 = fct_relevel(group_2, 
                               "Intercept", 
                               "Family Planning Indicators", 
                               "Technology Indicators",
                               
                               "Regions Indicators",
                               
                               "Reference Category",
                               
                               "Interactions"
  )) %>%
  filter(model=="Model 1" | model=="Model 2" | model=="Model 3") %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=X1, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_2, group=model),size=0.5, position = position_dodge(0.8)) +
  geom_pointrange(mapping=aes(x=X1, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_2, group=model), size=0.75, position = position_dodge(0.8)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B", "blue", "orange", "pink", "green", "grey"),
  scale_color_manual(values=c("#440154ff","#21908CFF", "#472D7BFF" , "#FDE725FF", "#AADC32FF", "#5DC863FF", "#27AD81FF", "#2C728EFF", "#3B528BFF", "grey"),
                     name = "Covariates")+
  guides(colour=guide_legend(title.position="top", 
                           title.hjust =0.5))+
  coord_flip()+
  geom_vline(data=d, mapping=aes(xintercept=x), color="grey70", linetype="dashed") +
  xlab("") + ylab("")+
  theme_light()+
  facet_wrap(~model)+
  facet_wrap(~model, ncol=4, scales ="free_x")+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        #legend.text=element_text(size=20),
        #legend.text=element_text(size=20),
        #legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.text.x = element_text(size=20),
        #legend.position = "top",
        legend.position = "none")

f1 

d=data.frame(x=c(3.5, 6.5, 9.5, 12.5, 16.5, 18.5, 22.5), y=c(3.5, 6.5, 9.5, 12.5, 16.5, 18.5, 22.5))


f2 <-m %>% 
  mutate('X1' = fct_relevel(X1,
                            
                            "LIC x ITU Internet",
                            "LMIC x ITU Internet",
                            "MIC x ITU Internet",
                            
                            "LIC x TFR",
                            "LMIC x TFR",
                            "MIC x TFR",
                            
                            "LIC x UNMP",
                            "LMIC x UNMP",
                            "MIC x UNMP",
                            
                            "LIC x CPMC",
                            "LMIC x CPMC",
                            "MIC x CPMC",
                            
                            "LIC",
                            "LMIC",
                            "MIC",
                            "HIC",
                            
                            "ITU Mobile", 
                            "ITU Internet",
                            "TFR", 
                            "UNMP",
                            "CPTC", 
                            "CPMC", 
                            "Intercept")) %>%
  mutate(group_2 = fct_relevel(group_2, 
                               "Intercept", 
                               "Family Planning Indicators", 
                               "Technology Indicators",
                               
                               "Regions Indicators",
                               "Reference Category",
                               
                               "Interactions"
  )) %>%
  filter(model=="Model 4" | model=="Model 5" | model=="Model 6" | model=="Model 7") %>%
  ggplot() + 
  geom_pointrange(mapping=aes(x=X1, y=`50%`, ymin=`2.5%`, ymax=`97.5%`, colour=group_2, group=model),size=0.5, position = position_dodge(0.8)) +
  geom_pointrange(mapping=aes(x=X1, y=`50%`, ymin=`25%`, ymax=`75%`, colour=group_2, group=model), size=0.75, position = position_dodge(0.8)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #scale_color_manual(values=c("#ac8e68","#ff6961", "#409cff" , "#ffb340", "#30DB5B", "#66d4cf"),
                     scale_color_manual(values=c("#440154ff","#21908CFF", "#472D7BFF" , "#FDE725FF", "#AADC32FF", "#5DC863FF", "#66d4cf"),
                     name = "Covariates")+
  guides(colour=guide_legend(title.position="top", 
                             title.hjust =0.5))+
  coord_flip()+
  geom_vline(data=d, mapping=aes(xintercept=x), color="grey70", linetype="dashed") +
  xlab("") + ylab("")+
  theme_light()+
  facet_wrap(~model, ncol=4, scales ="free_x")+
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        #legend.text=element_text(size=20),
        #legend.text=element_text(size=20),
        #legend.title=element_text(size=20),
        plot.title = element_text(size = 20, hjust =0.5),
        plot.caption = element_text(size=20),
        strip.text.x = element_text(size=20),
        #legend.position = "top",
        legend.position = "none")

f2

library(patchwork)
f1 / f2

ggsave("~/Desktop/GitHub/fertility-apps/figures/models_combo_viridis.pdf", width = 60, height = 45, units = "cm")


library(tidyverse)

plotChicco <- function(fit.summary, md, file=NULL){
  
  if(!is.null(file)) jpeg(file)
  
  delta_m3 <- fit.summary[grep('alpha', row.names(fit.summary), value=T), -c(1:4)]

  delta_m3 <- delta_m3 %>% mutate('2.5%'=(X2.5.),'25%'=(X25.),'50%'=(X50.),'75%'=(X75.),'97.5%'=(X97.5.)) %>%
    select('2.5%', '25%', '50%','75%','97.5%')
  
  delta_m3$delta <- colnames(md$X)
  
  # Add rows for reference category
  delta_m3 <- delta_m3 %>% add_row("delta"="Upper-middle-income economies", "2.5%"=0,  "25%"=0,   "50%"=0,   "75%"=0,   "97.5%"=0)
  
  
  delta_m3 <- delta_m3 %>% mutate(group_c = recode(delta, 
                                                   "Intercept"="group1", 
                                                   "CPMC"="group2", 
                                                   "CPTC"="group2",
                                                   "UNMP"="group2",
                                                   "TFR"="group2",
                                                   "ITU Internet"="group3",
                                                   "ITU Mobile"="group3", 
                                                   "Low-income economies"="group4",
                                                   "Lower-middle-income economies"="group4",
                                                   "Upper-middle-income economies"="group5",
                                                   "High-income economies"="group4"))
  
  
  #--- I stopped here, needs more work below ----#
  
  
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
  
  if(!is.null(file)) dev.off()
  
}
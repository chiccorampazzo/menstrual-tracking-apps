
library(tidyverse)
library(readr)
library(scales)
library(rjags)
library(coda)
library(MCMCvis)
library(maps)
library(sf)
library(countrycode)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgdal)

#######################################################################################################################
# Read files
#######################################################################################################################


# Read data
appstore <- read_csv("./Data/appstore_m1.csv")
playstore <- read_csv("./Data/playstore_m1.csv")

# Read sum reviews and ratings by country from App Store
sum <- read_csv("./Data/sum.csv")

# Data from playstore
y<-playstore$maxInstall
x1<-playstore$reviews
x2<-playstore$ratings

# Data from appstore
x1_app<-appstore$reviews
x2_app<-appstore$ratings

#######################################################################################################################
# Plot Ratings and Reviews
#######################################################################################################################

rare_playstore <- playstore %>% select(app, ratings, reviews) %>% mutate(source="Google Play")
rare_appstore <- appstore %>% select(app, ratings, reviews) %>% mutate(source="App Store")

rare <- rbind(rare_playstore, rare_appstore)

rare %>%
  ggplot()+
  geom_point(aes(log(reviews), log(ratings), colour=source), size=6)+
  geom_abline(coef = c(0,1), size=1, linetype="dashed")+
  scale_color_manual(values=c("#5eb0e5","#fbbc05"), #"#fbbc05" #ea4335
                     name="Source",
                     guide=guide_legend(title.position="top", title.hjust= 0.5, legend.hjust=0.5)) +
  xlim(0,15)+ ylim(0,15)+
  theme_light()+
  theme(legend.position="top", 
        legend.box="horizontal",
        axis.title=element_text(size=18),
        axis.text.x=element_text(size=18), 
        axis.text.y=element_text(size=18),
        legend.text=element_text(size=18), 
        legend.title=element_text(size=18))+
  annotate("text", x = 2, y = 13, label = "+ Ratings \n- Reviews", size=8)+
  annotate("text", x = 13, y = 2, label = "- Ratings \n+ Reviews", size=8)+
  annotate(
    geom = "curve", x = 2, y = 12, xend = 5, yend = 8.5, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = 13, y = 3, xend = 9.65, yend = 8.25, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) 
#######################################################################################################################

country <- sum %>% select(iso_2c)


#sum <- sum %>% select(-a2)
sum_r <- sum %>% select(-iso_2c, -X1)
ratio <- sum %>% replace(is.na(.), 0) %>% mutate(rowsum = rowSums(.[3:27])) 
# 123 countries
country <- ratio$iso_2c
ratio <- ratio %>% select(-X1, -iso_2c, -rowsum)
ratio <- t(ratio)
sum_r <-sum_r %>% replace(is.na(.), 0) %>% summarise(across(everything(), ~ sum(., is.na(.), 0)))
#sum_r <- sum %>% replace(is.na(.), 0)
sum_r <-t(as.vector(sum_r))



# Model
cat("
model {

  # Log-log model
  for (i in 1:N) {
     y[i] ~ dpois(lambda[i])
     log(lambda[i]) <- beta0 + beta1*log(x1[i]) + beta2*log(x2[i]) 
  }
  beta0 ~ dnorm(0,0.01)
  beta1 ~ dnorm(0,0.01)
  beta2 ~ dnorm(0,0.01)
  
  # Prediction
  for (i in 1:N) {
     y_app[i] <- exp(beta0 + beta1*log(x1_app[i]) + beta2*log(x2_app[i])) 
  }
  
  #Dirichlet 
  
  for (j in 1:M)
  {alpha[j] <- 1}

  for (i in 1:N){
  pi[i,1:M] ~ ddirch(alpha[1:M])}

  for (i in 1:N) {
  ratio[i,1:M] ~ dmulti(pi[i,1:M], sum_r[i,])}
  
  # Proportion
  
  for (i in 1:N) {
  for (j in 1:M) {

	proportion_apple[i,j] <- y_app[i]*pi[i,j]
	proportion_google[i,j] <- y[i]*pi[i,j]
	proportion_combo[i,j] <- (y[i]+y_app[i])*pi[i,j]
  }}
  
  for (j in 1:M) {
  sum.proportion_apple[j] <- sum(proportion_apple[,j])
  sum.proportion_google[j] <- sum(proportion_google[,j])
  sum.proportion_combo[j] <- sum(proportion_combo[,j])
  }

}
", file="./models/model.txt")

iterations <- 100000
burnin <- 1001
chains <- 3

model.fit <- jags.model(file="./models/model.txt",
                        
                        data=list(y=as.vector(y),
                                  x1=as.vector(x1),
                                  x2=as.vector(x2), 
                                  x1_app=as.vector(x1_app),
                                  x2_app=as.vector(x2_app),
                                  N=nrow(playstore), 
                                  ratio=ratio,
                                  M=ncol(ratio),
                                  sum_r=sum_r),
                        n.chains = chains)
model.samples <- coda.samples(model.fit, c("beta0", "beta1", "beta2", 
                                           "y_app", 
                                           "proportion_apple", "proportion_google", "proportion_combo", 
                                           "sum.proportion_apple", "sum.proportion_google", "sum.proportion_combo"), 
                              n.iter=iterations, thin=10)

#model.samples <- coda.samples(model.fit, c("y_app", "pi"), n.iter=iterations, thin=10)


#summary(model.samples)
summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)

MCMCsummary(model.samples)
MCMCtrace(model.samples,
          par=c("beta0", "beta1", "beta2", "y_app"),
          ISB = TRUE,
          ind=TRUE,
          Rhat=TRUE,
          pdf = TRUE,
          open_pdf = FALSE,
          filename = 'model.samples_1',
          wd = './Output')

#######################################################################################################################
#######################################################################################################################

model.samples <- coda.samples(model.fit, c("y_app"), n.iter=iterations, thin=10)

summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)
# Installations - y_app
app_install <-summary[1:25,-c(1:4)]
appstore<-cbind(appstore, app_install)

appstore<- appstore %>% 
  mutate('2.5%'=round(X2.5.),'25%'=round(X25.),'50%'=round(X50.),'75%'=round(X75.),'97.5%'=round(X97.5.), source="App Store")%>%
  dplyr::select(app, reviews, ratings, '2.5%', '25%', '50%','75%','97.5%', source)

playstore <- playstore %>%   mutate('2.5%'=NA,'25%'=NA,'50%'=maxInstall,'75%'=NA,'97.5%'=NA, source="Play Store")%>%
  dplyr::select(app, reviews, ratings, '2.5%', '25%', '50%','75%','97.5%', source)

app_data<-rbind(appstore, playstore)

app_data %>%
  mutate(app = fct_reorder(app, `50%`)) %>%
  ggplot() +
  #geom_point(aes(y = `50%`, x=app, colour=source), size=2.5)+
  geom_col(aes(y= `50%`, x=reorder(app, `50%`), 
               group=source, 
               fill=source), 
           position = "dodge")+
  scale_fill_manual(values=c("#5eb0e5","#fbbc05"), #"#fbbc05" #ea4335
                    labels=c("App Store - Predicted", "Play Store - Downloaded"), 
                    name="Source",
                    guide=guide_legend(title.position="top", title.hjust= 0.5, legend.hjust=0.5)) +
  theme(axis.text.x=element_text(),
        axis.title=element_text(size=14),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=45))+
  coord_flip()+
  expand_limits(y=c(0,80000000))+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), 
                     breaks=seq(0, 1600000000, by = 15000000))+
  xlab("") + ylab("Absolute Numbers")+
  theme_minimal()+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        plot.title = element_text(size = 14, hjust =0.5),
        legend.position="top",
        legend.box="horizontal") #+
  #ggtitle("Comparison of number of installation between App Store (estimated) and Play Store")

ggsave("figures/figure1.pdf", width = 30, height = 25, units = "cm")
#######################################################################################################################
#######################################################################################################################

model.samples <- coda.samples(model.fit, c("sum.proportion_combo"), n.iter=iterations, thin=10)

summary1<-summary(model.samples)$statistics
summary2<-summary(model.samples)$quantiles
summary<-cbind(summary1,summary2)
summary<-data.frame(summary)


sum.prop_combo <- summary[1:123,-c(1:4)]

quantile(sum.prop_combo$`X50.`)

quantile(sum.prop_combo$`X50.`, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))


df <- cbind(country, sum.prop_combo) 
df <- df %>% mutate(iso_a2=country)

world <- ne_countries(scale = "medium", returnclass = "sf")
dataset<-left_join(world,df, by="iso_a2")


## Let's ditch many of the unnecessary elements
plain <- theme(
  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  legend.position = "top",
  legend.box = "vertical",
  panel.background = element_rect(fill = "white"), 
  plot.margin = unit(c(1,1,1,1), "cm")
)


# Breaks
pretty_breaks <- quantile(sum.prop_combo$`X50.`, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
# find the extremes
minVal <- min(dataset$`X50.`, na.rm = T)
maxVal <- max(dataset$`X50.`, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

labels <-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")

#labels <- labels[1:length(labels)-1]
# define a new variable on the data set just as above

dataset$brks <- cut(
  dataset$`X50.`,
  breaks = brks,
  include.lowest = TRUE,
  labels = labels)


ggplot(data = dataset %>% filter(!admin %in% c("Antarctica"))) +
  #geom_sf(data = worldmap, colour="black") +
  geom_sf(aes(fill = brks), color = "white", lwd=0.25) +
  coord_sf(crs = st_crs('ESRI:54030')) + # 54009
  scale_fill_viridis_d(#option = "plasma", #trans = "sqrt",
    na.value = "grey",
    name = "Deciles",
    guide = guide_legend(
      direction = 1,
      keyheight = unit(5, units = "mm"),
      keywidth = unit(20, units = "mm"),
      title.position = 'top',
      # I shift the labels around, the should be placed
      # exactly at the right end of each legend key
      title.hjust = 0.5,
      label.hjust = 0.5,
      nrow = 1,
      byrow = T,
      # also the guide needs to be reversed
      reverse = F,
      label.position = "bottom"
    ))+
  plain

ggsave("figures/figure2.pdf", width = 30, height = 25, units = "cm")
#######################################################################################################################
#######################################################################################################################


# Negative Binomial Regression

installation_c <- trunc(summary$`X50.`)
installation_c <- as.vector(installation_c)

df <- read_csv("./Data/covariates.csv")

df$installation_c <- installation_c

write.csv(df, "data/covariates_installations.csv")
#######################################################################################################################


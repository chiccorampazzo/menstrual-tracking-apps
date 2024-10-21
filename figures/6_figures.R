# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(tidyverse)
library(scales)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")

# working directory
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Read summary files

m3 <- read_csv("out_doug/m3/summary.csv")


# Figure 1 - Installations

# Installations - y_app
app_install <-m3[15:39,-c(2:4)]

# Data from the stores 
appstore <- read_csv("~/Desktop/Github/fertility-apps/data/appstore_m1.csv")
appstore <- appstore[,c(-1)]
playstore <- read_csv("~/Desktop/Github/fertility-apps/data/playstore_m1.csv")

# Alphabetical order
appstore <- appstore[order(appstore$app),]
playstore <- playstore[order(playstore$app),]

appstore<-cbind(appstore, app_install)
names(appstore)
appstore<- appstore %>% 
  mutate('2.5%'=round(X2.5.),'25%'=round(X25.),'50%'=round(X50.),'75%'=round(X75.),'97.5%'=round(X97.5.), source="App Store") %>%
  select(app, reviews, ratings, '2.5%', '25%', '50%','75%','97.5%', source)

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
  scale_fill_viridis_d(
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
        legend.box="horizontal")

ggsave("figures/figure1_viridis.pdf", width = 35, height = 25, units = "cm")


# Figure 2 - Map

# Installations by country
app_install <-m3[40:151,-c(2:4)]

# Find list of countries 
sum_by_country <- read.csv("~/Desktop/Github/fertility-apps/data/sum.csv", 
                           stringsAsFactors = F)



# Covariates for countries
covariates <- read.csv("~/Desktop/Github/fertility-apps/data/covariates.csv", 
                           stringsAsFactors = F)
covariates <- covariates %>% mutate(iso_a2=country)

# format sum_by_country
sum_country <- t(sum_by_country[,paste0('app',1:nrow(appstore))])
rownames(sum_country) <- rownames(appstore)
colnames(sum_country) <- sum_by_country$iso_2c
sum_country[is.na(sum_country)] <- 0

# 112 countries in the analysis
countries <- colnames(sum_country)[apply(sum_country, 2, sum)>0]

# Merge
df <- cbind(countries, app_install)
df <- df %>% mutate(iso_a2=countries)
# merge also with covariates
df <- left_join(df, covariates, by="iso_a2")
df <- df %>% mutate(in_pc=`X50.`/popFage)

df$in_pc
world <- ne_countries(scale = "medium", returnclass = "sf")
dataset<-left_join(world, df, by="iso_a2")

# Drop PW Palau
#dataset <- dataset %>% filter("iso_a2"!="PW")

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
pretty_breaks <- quantile(dataset$in_pc, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)
# find the extremes
minVal <- min(dataset$in_pc, na.rm = T)
maxVal <- max(dataset$in_pc, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

labels <-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")

dataset$brks <- cut(
  dataset$in_pc,
  breaks = brks,
  include.lowest = TRUE,
  labels = labels)

ggplot(data = dataset %>% filter(!admin %in% c("Antarctica"))) +
  #geom_sf(data = worldmap, colour="black") +
  geom_sf(aes(fill = brks), color = "white", lwd=0.25) +
  coord_sf(crs = st_crs('ESRI:54030')) + # 54009
  scale_fill_viridis_d(
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

ggsave("figures/figure2.pdf", width = 30, height = 18, units = "cm")



# Maps by App

# Installations by country
app_country <-m3[152:2951,-c(2:4)]
colnames(app_country)[1] <- "installs_app_country"

# Each App
app1 <- app_country[grepl("\\[1,", app_country$installs_app_country),] %>% mutate(app="Ava fertility tracker")
app2 <- app_country[grepl("\\[2,", app_country$installs_app_country),] %>% mutate(app="Bellabeat")
clue <-app_country[grepl("\\[3", app_country$installs_app_country),] %>% mutate(app="Clue")
app4 <-app_country[grepl("\\[4", app_country$installs_app_country),] %>% mutate(app="Easy Period")
app5 <-app_country[grepl("\\[5", app_country$installs_app_country),] %>% mutate(app="Eve Period Tracker")
app6 <-app_country[grepl("\\[6", app_country$installs_app_country),] %>% mutate(app="FEMM")
app7 <-app_country[grepl("\\[7", app_country$installs_app_country),] %>% mutate(app="Fertility Friend")
flo <-app_country[grepl("\\[8", app_country$installs_app_country),] %>% mutate(app="Flo")
app9 <-app_country[grepl("\\[9", app_country$installs_app_country),] %>% mutate(app="Glow")
app10 <-app_country[grepl("\\[10", app_country$installs_app_country),] %>% mutate(app="Kindara")
app11 <-app_country[grepl("\\[11", app_country$installs_app_country),] %>% mutate(app="Ladytimer")
app12 <-app_country[grepl("\\[12", app_country$installs_app_country),] %>% mutate(app="Maya")
app13 <-app_country[grepl("\\[13", app_country$installs_app_country),] %>% mutate(app="Natural Cycles")
app14 <-app_country[grepl("\\[14", app_country$installs_app_country),] %>% mutate(app="Nurx")
app15 <-app_country[grepl("\\[15", app_country$installs_app_country),] %>% mutate(app="Ovagraph")
app16 <-app_country[grepl("\\[16", app_country$installs_app_country),] %>% mutate(app="Ovia")
app17 <-app_country[grepl("\\[17", app_country$installs_app_country),] %>% mutate(app="Period Tracker")
app18 <-app_country[grepl("\\[18", app_country$installs_app_country),] %>% mutate(app="Period Tracker & Diary")
app19 <-app_country[grepl("\\[19", app_country$installs_app_country),] %>% mutate(app="Period Tracker by Pinkbird")
app20 <-app_country[grepl("\\[20", app_country$installs_app_country),] %>% mutate(app="Period Tracker for woman")
ptl <-app_country[grepl("\\[21", app_country$installs_app_country),] %>% mutate(app="Period Tracker Lite")
app22 <-app_country[grepl("\\[22", app_country$installs_app_country),] %>% mutate(app="Temdrop")
app23 <-app_country[grepl("\\[23", app_country$installs_app_country),] %>% mutate(app="Woman Log & Ovulation Tracker")
app24 <-app_country[grepl("\\[24", app_country$installs_app_country),] %>% mutate(app="WomanLog")
app25 <-app_country[grepl("\\[25", app_country$installs_app_country),] %>% mutate(app="WOOM")

apps <- rbind(app1, app2, clue, app4, app5, app6, app7, flo, app9, app10, app11, app12, app13, app14, app15, 
              app16, app17, app18, app19, app20, ptl, app22, app23, app24, app25)


data <- left_join(df, world, by="iso_a2") %>% select("iso_a2", "continent.y")
iso_a2 <- data$iso_a2
continent <- data$continent.y

apps$country <- rep(iso_a2, each=25) 
apps$continent <- rep(continent, each=25) 

rank <- apps %>% group_by(continent, app) %>% summarise(sum=sum(X50.))

unique(apps$continent)
# Africa 
rank %>% filter(continent=="Africa") %>% arrange(-sum)
# Asia
rank %>% filter(continent=="Asia") %>% arrange(-sum)
# Europe
rank %>% filter(continent=="Europe") %>% arrange(-sum)
# North America
rank %>% filter(continent=="North America") %>% arrange(-sum)
# South America
rank %>% filter(continent=="South America") %>% arrange(-sum)
# Oceania
rank %>% filter(continent=="Oceania") %>% arrange(-sum)

# Seven seas (open ocean)
rank %>% filter(continent=="Seven seas (open ocean)") %>% arrange(-sum)



clue[clue == 0] <- NA
flo[flo == 0] <- NA
ptl[ptl == 0] <- NA



unique(continent)

#app_country %>% filter(continent=="Europe") %>% summarise "X50." %>% top_n("X50.")

# Clue

# Merge
clue <- cbind(countries, clue)
clue <- clue %>% mutate(iso_a2=countries)
clue <- left_join(clue, covariates, by="iso_a2")
clue <- clue %>% mutate(in_pc=`X50.`/popFage)

clue<-left_join(world, clue, by="iso_a2")


# Breaks
pretty_breaks <- quantile(clue$in_pc, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)
# find the extremes
minVal <- min(clue$in_pc, na.rm = T)
maxVal <- max(clue$in_pc, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

labels <-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")

clue$brks <- cut(
  clue$in_pc,
  breaks = brks,
  include.lowest = TRUE,
  labels = labels)


ggplot(data = clue %>% filter(!admin %in% c("Antarctica"))) +
  #geom_sf(data = worldmap, colour="black") +
  geom_sf(aes(fill = brks), color = "white", lwd=0.25) +
  coord_sf(crs = st_crs('ESRI:54030')) + # 54009
  scale_fill_viridis_d(
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

ggsave("figures/clue.pdf", width = 30, height = 18, units = "cm")

# Flo

# Merge
flo <- cbind(countries, flo)
flo <- flo %>% mutate(iso_a2=countries)
flo <- left_join(flo, covariates, by="iso_a2")
flo <- flo %>% mutate(in_pc=`X50.`/popFage)

flo<-left_join(world, flo, by="iso_a2")


# Breaks
pretty_breaks <- quantile(flo$in_pc, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)
# find the extremes
minVal <- min(flo$in_pc, na.rm = T)
maxVal <- max(flo$in_pc, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

labels <-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")

flo$brks <- cut(
  flo$in_pc,
  breaks = brks,
  include.lowest = TRUE,
  labels = labels)

ggplot(data = flo %>% filter(!admin %in% c("Antarctica"))) +
  #geom_sf(data = worldmap, colour="black") +
  geom_sf(aes(fill = brks), color = "white", lwd=0.25) +
  coord_sf(crs = st_crs('ESRI:54030')) + # 54009
  scale_fill_viridis_d(
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

ggsave("figures/flo.pdf", width = 30, height = 18, units = "cm")

# Ptl

# Merge
ptl <- cbind(countries, ptl)
ptl <- ptl %>% mutate(iso_a2=countries)
ptl <- left_join(ptl, covariates, by="iso_a2")
ptl <- ptl %>% mutate(in_pc=`X50.`/popFage)

ptl<-left_join(world, ptl, by="iso_a2")


# Breaks
pretty_breaks <- quantile(ptl$in_pc, probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)
# find the extremes
minVal <- min(ptl$in_pc, na.rm = T)
maxVal <- max(ptl$in_pc, na.rm = T)
# compute labels
labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)

labels <-c("First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth")

ptl$brks <- cut(
  ptl$in_pc,
  breaks = brks,
  include.lowest = TRUE,
  labels = labels)

ggplot(data = ptl %>% filter(!admin %in% c("Antarctica"))) +
  #geom_sf(data = worldmap, colour="black") +
  geom_sf(aes(fill = brks), color = "white", lwd=0.25) +
  coord_sf(crs = st_crs('ESRI:54030')) + # 54009
  scale_fill_viridis_d(
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

ggsave("figures/ptl.pdf", width = 30, height = 18, units = "cm")

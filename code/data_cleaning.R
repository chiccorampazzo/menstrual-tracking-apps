# Cleaning the data

library(tidyverse)
library(maps)
library(sf)
library(countrycode)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgdal)
library(janitor)
library(readxl)
options(scipen=999)
#######################################################################################################################
# Read files
#######################################################################################################################

# Countries with App Store and Play Store
markets_country <- read_csv("./data/markets_country.csv")
markets_country <- markets_country %>% mutate(country=iso_a2)

# Remove countries with same values as the US
markets_country<-markets_country %>% filter(
  country!="MZ", 
  country!="BS", 
  country!="KZ", 
  country!="KN", 
  country!="SC", 
  country!="KR",
  country!="DO", 
  country!="TT", 
  country!="BJ", 
  country!="KG", 
  country!="TM", 
  country!="MD",
  country!="MR")

# Select only countries in which both markets are available
markets_country <- markets_country %>% filter(markets=="Both Markets")
markets_country <- data.frame(markets_country)

markets_country$continent <- countrycode(sourcevar = markets_country[, "iso_a2"],
                                         origin = "iso2c",
                                         destination = "continent")

markets_country$region <- countrycode(sourcevar = markets_country[, "iso_a2"],
                                      origin = "iso2c",
                                      destination = "un.regionsub.name")
# Fix Taiwan - Not Matched
markets_country[markets_country$country=="TW", "region"] <- "Eastern Asia"
markets_country <- markets_country %>% mutate(iso_2c =iso_a2) %>% select(iso_2c, country, continent, region)



# Read Reviews App Store
reviews_app_store <- read_csv("./data/reviews-app-store.csv")

# Remove countries with same values as the US
reviews_app_store<-reviews_app_store %>% filter(
  country!="MZ", 
  country!="BS", 
  country!="KZ", 
  country!="KN", 
  country!="SC", 
  country!="KR",
  country!="DO", 
  country!="TT", 
  country!="BJ", 
  country!="KG", 
  country!="TM", 
  country!="MD",
  country!="MR")



reviews_app_store <- reviews_app_store %>% drop_na()
appstore <- reviews_app_store %>% group_by(app)%>% summarise(reviews=sum(reviews), ratings=sum(reviewsCount), )

# Recode the Apps' names
appstore<-appstore %>% mutate(app=recode(app, 
                                         "1210553544"="Period tracker by PinkBird",                      
                                         "1064018574"="Ava fertility tracker",    
                                         "1095084063"="Woman Log & Ovulation Tracker",               
                                         "1064911742"="Period Tracker & Diary",                 
                                         "720796332"="Period Tracker",                    
                                         "657189652"="Clue",                          
                                         "585373332"="OvaGraph",               
                                         "638021335"="GLOW",                           
                                         "1002275138"="Eve Period Tracker",                       
                                         "1209759365"="Easy Period",                      
                                         "522674372"="Kindara",                           
                                         "406762826"="Ladytimer",            
                                         "765535549"="Natural Cycles",                  
                                         "1213141301"="Nurx",                                   
                                         "570244389"="Ovia",                      
                                         "1477587818"="Lunar",
                                         "330376830"="Period Tracker Lite",                    
                                         "443919067"="Fertility Friend",                
                                         "1199061141"="Tempdrop",             
                                         "1423628793"="Period tracker for women",                 
                                         "421360650"="WomanLog",                               
                                         "1255164043"="WOOM",                  
                                         "492534636"="Maya",                  
                                         "944880989"="FEMM",                        
                                         "1038369065"="Flo",                
                                         "436762566"="Bellabeat",                    
                                         "1010506484"="Ovulation Calculator",
                                         "1279295922"="Premom Ovulation Tracker"))

# Remove app that have been dismessed in one or both markets (from 28 to 25 apps)
appstore <- appstore %>% filter(app!="Lunar" & app!="Premom Ovulation Tracker" & app!="Ovulation Calculator")

write.csv(appstore, "./Data/appstore_m1.csv")


#######################################################################################################################

# Review and Installations from Google Play Store
reviews_play_store <- read_csv("./data/reviews-play-store.csv")
playstore1 <- reviews_play_store %>% group_by(app)%>% summarise(reviews=sum(reviews))
playstore2 <- reviews_play_store %>% select(app, maxInstall) %>% unique()
playstore3 <- reviews_play_store %>% select(app, ratings) %>% unique()
playstore <- merge(playstore1, playstore2, by="app")
playstore <- merge(playstore, playstore3, by="app")

playstore<-playstore %>% mutate(app=recode(app, 
                                           "co.quanyong.pinkbird"="Period tracker by PinkBird",                      
                                           "com.avawomen.com.ava_android.production"="Ava fertility tracker",    
                                           "com.AvvaStyle.femalecalendar"="Woman Log & Ovulation Tracker",               
                                           "com.brc.PeriodTrackerDiary"="Period Tracker & Diary",                 
                                           "com.cg.android.ptracker"="Period Tracker",                    
                                           "com.clue.android"="Clue",                          
                                           "com.fairhavenhealth.ovagraph"="OvaGraph",               
                                           "com.glow.android"="GLOW",                           
                                           "com.glow.android.eve"="Eve Period Tracker",                       
                                           "com.imzhiqiang.period"="Easy Period",                      
                                           "com.kindara.pgap"="Kindara",                           
                                           "com.ladytimer.ovulationcalendar"="Ladytimer",            
                                           "com.naturalcycles.cordova"="Natural Cycles",                  
                                           "com.nurx"="Nurx",       
                                           "com.ovuline.fertility"="Ovia",  
                                           "com.period.calendar.ovulation.tracker.lunar"="Lunar",
                                           "com.period.tracker.lite"="Period Tracker Lite",                    
                                           "com.periodapp.period"="Period Tracker INC",                        
                                           "com.tamtris.fertilityfriend"="Fertility Friend",                
                                           "com.tempdrop.tempdropmobileapp"="Tempdrop",             
                                           "com.wachanga.womancalendar"="Period tracker for women",                 
                                           "com.womanlog"="WomanLog",                               
                                           "com.woomfertility.woomapp"="WOOM",                  
                                           "in.plackal.lovecyclesfree"="Maya",                  
                                           "org.femmhealth.femm"="FEMM",                        
                                           "org.iggymedia.periodtracker"="Flo",                
                                           "org.nanobit.perioddiary"="Bellabeat",                    
                                           "ovulationcalculator.com.ovulationcalculator"="Ovulation Calculator",
                                           "premom.eh.com.ehpremomapp"="Premom Ovulation Tracker", 
                                           "com.facebook.katana"="Facebook", 
                                           "com.instagram.android"="Instagram"))

# Remove app that have been dismessed in one or both markets (from 28 to 25 apps)
playstore<- playstore %>% filter(app!="Lunar" & app!="Premom Ovulation Tracker" & app!="Ovulation Calculator" &
                                app!="Facebook" & app!="Instagram")

playstore<- playstore %>% group_by(app) %>% top_n(1,ratings)

write.csv(playstore, "./Data/playstore_m1.csv")

#######################################################################################################################

appstore_c <- reviews_app_store %>% group_by(app, country)%>% summarise(reviews=sum(reviews), ratings=sum(reviewsCount))

appstore_c<- appstore_c %>% mutate(app=recode(app, 
                                         "1210553544"="Period tracker by PinkBird",                      
                                         "1064018574"="Ava fertility tracker",    
                                         "1095084063"="Woman Log & Ovulation Tracker",               
                                         "1064911742"="Period Tracker & Diary",                 
                                         "720796332"="Period Tracker",                    
                                         "657189652"="Clue",                          
                                         "585373332"="OvaGraph",               
                                         "638021335"="GLOW",                           
                                         "1002275138"="Eve Period Tracker",                       
                                         "1209759365"="Easy Period",                      
                                         "522674372"="Kindara",                           
                                         "406762826"="Ladytimer",            
                                         "765535549"="Natural Cycles",                  
                                         "1213141301"="Nurx",                                   
                                         "570244389"="Ovia",                      
                                         "1477587818"="Lunar",
                                         "330376830"="Period Tracker Lite",                    
                                         "443919067"="Fertility Friend",                
                                         "1199061141"="Tempdrop",             
                                         "1423628793"="Period tracker for women",                 
                                         "421360650"="WomanLog",                               
                                         "1255164043"="WOOM",                  
                                         "492534636"="Maya",                  
                                         "944880989"="FEMM",                        
                                         "1038369065"="Flo",                
                                         "436762566"="Bellabeat",                    
                                         "1010506484"="Ovulation Calculator",
                                         "1279295922"="Premom Ovulation Tracker", 
                                         "284882215"="Facebook", 
                                         "389801252"="Instagram"))

appstore_c <- appstore_c %>% filter(app!="Lunar" & app!="Premom Ovulation Tracker" & app!="Ovulation Calculator" &
                                    app!="Facebook" & app!="Instagram")


appstore_c<-na.omit(appstore_c)
appstore_c<-appstore_c %>% group_by(app) %>% mutate(sum=(reviews+ratings),  a2=country)

appstore_c<-na.omit(appstore_c)
appstore_c<-appstore_c %>% group_by(app) %>% mutate(sum=(reviews+ratings),  a2=country)

# Change country to capital letter
appstore_c$a2<-casefold(appstore_c$a2, upper=T)

world <- iso3166

sum1<-appstore_c %>% ungroup() %>% filter(app=="Period Tracker Lite") %>% dplyr::select(a2, sum)
sum1<-left_join(world,sum1, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum2<-appstore_c %>% ungroup() %>% filter(app=="Ladytimer") %>% dplyr::select(a2, sum)
sum2<-left_join(world,sum2, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum3<-appstore_c %>% ungroup() %>% filter(app=="Woman Log & Ovulation Tracker") %>% dplyr::select(a2, sum)
sum3<-left_join(world,sum3, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum4<-appstore_c %>% ungroup() %>% filter(app=="Bellabeat") %>% dplyr::select(a2, sum)
sum4<-left_join(world,sum4, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum5<-appstore_c %>% ungroup() %>% filter(app=="Fertility Friend") %>% dplyr::select(a2, sum)
sum5<-left_join(world,sum5, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum6<-appstore_c %>% ungroup() %>% filter(app=="Maya") %>% dplyr::select(a2, sum)
sum6<-left_join(world,sum6, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum7<-appstore_c %>% ungroup() %>% filter(app=="Kindara") %>% dplyr::select(a2, sum)
sum7<-left_join(world,sum7, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum8<-appstore_c %>% ungroup() %>% filter(app=="Ovia") %>% dplyr::select(a2, sum)
sum8<-left_join(world,sum8, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum9<-appstore_c %>% ungroup() %>% filter(app=="OvaGraph") %>% dplyr::select(a2, sum)
sum9<-left_join(world,sum9, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum10<-appstore_c %>% ungroup() %>% filter(app=="GLOW") %>% dplyr::select(a2, sum)
sum10<-left_join(world,sum10, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum11<-appstore_c %>% ungroup() %>% filter(app=="Clue") %>% dplyr::select(a2, sum)
sum11<-left_join(world,sum11, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum12<-appstore_c %>% ungroup() %>% filter(app=="Period Tracker") %>% dplyr::select(a2, sum)
sum12<-left_join(world,sum12, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum13<-appstore_c %>% ungroup() %>% filter(app=="Natural Cycles") %>% dplyr::select(a2, sum)
sum13<-left_join(world,sum13, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum14<-appstore_c %>% ungroup() %>% filter(app=="FEMM") %>% dplyr::select(a2, sum)
sum14<-left_join(world,sum14, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum15<-appstore_c %>% ungroup() %>% filter(app=="Eve Period Tracker") %>% dplyr::select(a2, sum)
sum15<-left_join(world,sum15, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum16<-appstore_c %>% ungroup() %>% filter(app=="Flo") %>% dplyr::select(a2, sum)
sum16<-left_join(world,sum16, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum17<-appstore_c %>% ungroup() %>% filter(app=="Ava fertility tracker") %>% dplyr::select(a2, sum)
sum17<-left_join(world,sum17, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum18<-appstore_c %>% ungroup() %>% filter(app=="Period Tracker & Diary") %>% dplyr::select(a2, sum)
sum18<-left_join(world,sum18, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum19<-appstore_c %>% ungroup() %>% filter(app=="Woman Log & Ovulation Tracker") %>% dplyr::select(a2, sum)
sum19<-left_join(world,sum19, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum20<-appstore_c %>% ungroup() %>% filter(app=="Tempdrop") %>% dplyr::select(a2, sum)
sum20<-left_join(world,sum20, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum21<-appstore_c %>% ungroup() %>% filter(app=="Easy Period") %>% dplyr::select(a2, sum)
sum21<-left_join(world,sum21, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum22<-appstore_c %>% ungroup() %>% filter(app=="Period tracker by PinkBird") %>% dplyr::select(a2, sum)
sum22<-left_join(world,sum22, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum23<-appstore_c %>% ungroup() %>% filter(app=="Nurx") %>% dplyr::select(a2, sum)
sum23<-left_join(world,sum23, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum24<-appstore_c %>% ungroup() %>% filter(app=="WOOM") %>% dplyr::select(a2, sum)
sum24<-left_join(world,sum24, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum25<-appstore_c %>% ungroup() %>% filter(app=="Period tracker for women") %>% dplyr::select(a2, sum)
sum25<-left_join(world,sum25, by="a2") %>% ungroup() %>% dplyr::select(a2, sum) %>% unique()

sum1<- sum1 %>% mutate(app1=sum) %>% dplyr::select(a2,app1)
sum2<- sum2 %>% mutate(app2=sum) %>% dplyr::select(app2)
sum3<- sum3 %>% mutate(app3=sum) %>% dplyr::select(app3)
sum4<- sum4 %>% mutate(app4=sum) %>% dplyr::select(app4)
sum5<- sum5 %>% mutate(app5=sum) %>% dplyr::select(app5)
sum6<- sum6 %>% mutate(app6=sum) %>% dplyr::select(app6)
sum7<- sum7 %>% mutate(app7=sum) %>% dplyr::select(app7)
sum8<- sum8 %>% mutate(app8=sum) %>% dplyr::select(app8)
sum9<- sum9 %>% mutate(app9=sum) %>% dplyr::select(app9)
sum10<- sum10 %>% mutate(app10=sum) %>% dplyr::select(app10)
sum11<- sum11 %>% mutate(app11=sum) %>% dplyr::select(app11)
sum12<- sum12 %>% mutate(app12=sum) %>% dplyr::select(app12)
sum13<- sum13 %>% mutate(app13=sum) %>% dplyr::select(app13)
sum14<- sum14 %>% mutate(app14=sum) %>% dplyr::select(app14)
sum15<- sum15 %>% mutate(app15=sum) %>% dplyr::select(app15)
sum16<- sum16 %>% mutate(app16=sum) %>% dplyr::select(app16)
sum17<- sum17 %>% mutate(app17=sum) %>% dplyr::select(app17)
sum18<- sum18 %>% mutate(app18=sum) %>% dplyr::select(app18)
sum19<- sum19 %>% mutate(app19=sum) %>% dplyr::select(app19)
sum20<- sum20 %>% mutate(app20=sum) %>% dplyr::select(app20)
sum21<- sum21 %>% mutate(app21=sum) %>% dplyr::select(app21)
sum22<- sum22 %>% mutate(app22=sum) %>% dplyr::select(app22)
sum23<- sum23 %>% mutate(app23=sum) %>% dplyr::select(app23)
sum24<- sum24 %>% mutate(app24=sum) %>% dplyr::select(app24)
sum25<- sum25 %>% mutate(app25=sum) %>% dplyr::select(app25)


sum <- cbind(sum1, sum2, sum3, sum4, sum5, sum6, sum7, sum8, sum9, 
             sum10, sum11, sum12, sum13, sum14, sum15, sum16, sum17, sum18, sum19, 
             sum20, sum21, sum22, sum23, sum24, sum25) 


sum <- sum %>% filter(a2!="??")
# Select only the 123 countries with Both Markets and unique values
sum <- sum %>% mutate(iso_2c=a2)
sum_c <- left_join(markets_country, sum, by="iso_2c")

sum_c <- sum_c %>% select(-country, -continent, -region, -a2)

write.csv(sum_c, "./Data/sum.csv")

#######################################################################################################################
# UN data
#######################################################################################################################

# https://www.un.org/development/desa/pd/data/world-contraceptive-use
# https://www.un.org/development/desa/pd/node/3288
# https://www.un.org/development/desa/pd/sites/www.un.org.development.desa.pd/files/undesa_pd_2021_wcu_fp-indicators_documentation.pdf

un_data <- read_csv("data/Data_FamilyPlanningIndicators_2021.csv")
# Filter Year 2021
un_data <- un_data %>% filter(Time=="2021")
# Filter All Women
un_data <- un_data %>% filter(Category=="All women")
# Filter Only Median
un_data <- un_data %>% filter(Variant=="Median")
# Pivoting 
un_data <- un_data  %>% select("Location", "IndicatorShortName", "Value") %>% 
  pivot_wider(names_from = "IndicatorShortName", values_from= c("Value"))
# Change Reunion and Core D'Ivoire
un_data$Location[80] <- "Cote d'Ivoire"
un_data$Location[133] <- "Reunion"

# Country names
un_data$iso_2c <- countrycode(un_data$Location, origin="country.name", destination="iso2c")
un_data <-un_data %>% drop_na(iso_2c)
un_data$iso_2c
un_data <- left_join(markets_country, un_data, by="iso_2c")
un_data <- as.data.frame(un_data)
class(un_data)

# Hong Kong, Taiwan, Macu and Micronesia are missing 
data_miss <- read_csv("data/un_missing.csv")
# Filter Year 2021
data_miss <- data_miss %>% filter(Time=="2021")
# Filter All Women
data_miss <- data_miss %>% filter(EstimateTypeId=="All women")
# Filter Only Median
data_miss <- data_miss %>% filter(Variant=="Median")
# Pivoting 
data_miss <- data_miss  %>% select("Location", "IndicatorShortName", "Value") %>% 
  pivot_wider(names_from = "IndicatorShortName", values_from= c("Value"))
# Country names
data_miss$iso_2c <- countrycode(data_miss$Location, origin="country.name", destination="iso2c")


# Hong Kong
un_data$`CPAnyP`[46] <- 47.4 
un_data$`CPTrad`[46] <- 2.8 
un_data$`CPModP`[46] <- 44.6
un_data$`UNMP`[46] <- 7.8
un_data$`UNMModP`[46] <- 10.6
un_data$`DEMTot`[46] <- 55.2
un_data$`DEMMod`[46] <- 80.8
# Taiwan
un_data$`CPAnyP`[107] <- 53.5 
un_data$`CPTrad`[107] <- 4.4 
un_data$`CPModP`[107] <- 49.1
un_data$`UNMP`[107] <- 6.2
un_data$`UNMModP`[107] <- 10.6
un_data$`DEMTot`[107] <- 59.7
un_data$`DEMMod`[107] <- NA


# UN data
# https://rdrr.io/cran/wpp2019/man/wpp2019-package.html
library(wpp2019)
data(tfr)

tfr <- tfr %>% select(name, `2015-2020`)
# Country names
tfr$iso_2c <- countrycode(tfr$name, origin="country.name", destination="iso2c")
tfr <- tfr %>% drop_na(iso_2c)

df <- left_join(un_data, tfr, by="iso_2c")

df <- df %>% mutate(tfr=`2015-2020`) %>% select(-name, -`2015-2020`)

# Total Female Population
data(popFT)
popFT <- popFT %>% select(name, `2020`)
# Country names
popFT$iso_2c <- countrycode(popFT$name, origin="country.name", destination="iso2c")
popFT <- popFT %>% drop_na(iso_2c)

df <- left_join(df, popFT, by="iso_2c")

df <- df %>% mutate(popFT=`2020`) %>% select(-name, -`2020`)

# Female Population 15-49
data(popF)
popF <- popF %>% select(name,age, `2020`)
# Select Age Groups
popF <- popF %>% filter(age=="15-19" | age=="20-24" | age=="25-29" &
                          age=="30-34" | age=="35-39" | age=="40-44" | age=="45-49")
# Summarise
popF<-popF %>% group_by(name) %>% summarise(popFage=sum(`2020`))

# Country names
popF$iso_2c <- countrycode(popF$name, origin="country.name", destination="iso2c")
popF <- popF %>% drop_na(iso_2c)

df <- left_join(df, popF, by="iso_2c")
df <- df %>% select(-name)


# Total Population
data(pop)
pop <- pop %>% select(name, `2020`)
# Country names
pop$iso_2c <- countrycode(pop$name, origin="country.name", destination="iso2c")
pop <- pop %>% drop_na(iso_2c)

pop <- pop %>% mutate(totpop=`2020`)


df <- left_join(df, pop, by="iso_2c")
df <- df %>% select(-name, -`2020`)

# ITU data
# https://www.itu.int/en/ITU-D/Statistics/Pages/stat/default.aspx
# https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwjaz7jhj4L2AhWKUMAKHQP4B08QFnoECAMQAQ&url=https%3A%2F%2Fwww.itu.int%2Fen%2FITU-D%2FStatistics%2FDocuments%2Fstatistics%2F2021%2FPercentIndividualsUsingInternet.xlsx&usg=AOvVaw3tILLjAIeH1fq2LOxpn5og
itu_Internet <- read_excel("data/PercentIndividualsUsingInternet.xlsx")
itu_Internet <- itu_Internet %>% mutate(country=ShortName) 
itu_Internet <- itu_Internet %>% select(country, `2017_value`)
itu_Internet$iso_2c <- countrycode(itu_Internet$country, origin = "country.name", destination="iso2c")

itu_Internet <-  itu_Internet %>% mutate(itu_internet=`2017_value`) %>% select(itu_internet, iso_2c)
itu_Internet <-  itu_Internet %>% drop_na(iso_2c)

df <- left_join(df, itu_Internet, by="iso_2c")


itu_mobile <- read_excel("data/MobileCellularSubscriptions_2000-2020.xlsx", sheet = "i911")
itu_mobile <- itu_mobile %>% mutate(country=Country) 
itu_mobile <- itu_mobile  %>% select(country, `2020_value`)
itu_mobile$iso_2c <- countrycode(itu_mobile$country, origin = "country.name", destination="iso2c")

itu_mobile <-  itu_mobile %>% mutate(itu_mobile=`2020_value`) %>% select(itu_mobile, iso_2c)
itu_mobile <-  itu_mobile %>% drop_na(iso_2c)

#itu_mobile %>% mutate(itu_m_r=(itu_mobile/sum(itu_mobile)))

df<- left_join(df, itu_mobile, by="iso_2c")


itu_mobile_gen <- read_excel("data/IndividualsUsingInternetByGender.xlsx", skip = 2)
itu_mobile_gen <- itu_mobile_gen %>% mutate(country=`Economy name`) 
itu_mobile_gen <- itu_mobile_gen %>% mutate(itu_female_m=`Female...8`) 
itu_mobile_gen <- itu_mobile_gen  %>% select(country, itu_female_m)
itu_mobile_gen <- itu_mobile_gen[-c(119:123), ]


itu_mobile_gen$female_m<-as.numeric(itu_mobile_gen$itu_female_m)
itu_mobile_gen$iso_2c <- countrycode(itu_mobile_gen$country, origin = "country.name", destination="iso2c")
itu_mobile_gen <-  itu_mobile_gen  %>% select(itu_female_m, iso_2c)
itu_mobile_gen <-  itu_mobile_gen %>% drop_na(iso_2c)


df <- df %>% left_join(itu_mobile_gen, by="iso_2c")
#write.csv(un_data, "data/un_data.csv")


# Gender Inequality Index 
un_gender_index <- read_csv("data/un_gender_index.csv")

un_gender_index$iso_2c <- countrycode(un_gender_index$country, origin = "country.name", destination="iso2c")
# as.numeric
un_gender_index$gender_in_index<- as.numeric(un_gender_index$gender_in_index)
un_gender_index$adolescent_birth<- as.numeric(un_gender_index$adolescent_birth)
un_gender_index$education_f<- as.numeric(un_gender_index$education_f)
un_gender_index$labour_f_f<- as.numeric(un_gender_index$labour_f_f)

un_gender_index <-  un_gender_index  %>% select(gender_in_index, adolescent_birth, education_f, labour_f_f, iso_2c)
un_gender_index <-  un_gender_index %>% drop_na(iso_2c)

df <- df %>% left_join(un_gender_index, by="iso_2c")
df <- df %>% mutate(gender_in_index = gender_in_index*100)


# GDP based on 2017
UNdata_GDP_percapita2017 <- read_csv("data/UNdata_GDP_percapita2017.csv")
UNdata_GDP_percapita2017$iso_2c <- countrycode(UNdata_GDP_percapita2017$`Country or Area`, origin = "country.name", destination="iso2c")
UNdata_GDP_percapita2017 <-  UNdata_GDP_percapita2017 %>% mutate(gdp_pc17=`Value`)  %>% select(gdp_pc17, iso_2c)
UNdata_GDP_percapita2017 <-  UNdata_GDP_percapita2017 %>% drop_na(iso_2c)

df <- df %>% left_join(UNdata_GDP_percapita2017, by="iso_2c")

# GDP per capita
UNdata_GDP_percapita <- read_csv("data/UNdata_GDP_percapita.csv")
UNdata_GDP_percapita$iso_2c <- countrycode(UNdata_GDP_percapita$`Country or Area`, origin = "country.name", destination="iso2c")
UNdata_GDP_percapita <-  UNdata_GDP_percapita %>% mutate(gdp_pc=`Value`)  %>% select(gdp_pc, iso_2c)
UNdata_GDP_percapita <-  UNdata_GDP_percapita %>% drop_na(iso_2c)

df <- df %>% left_join(UNdata_GDP_percapita, by="iso_2c")


df$CPAnyP<-as.character(df$CPAnyP)
df$CPModP<-as.character(df$CPModP)
df$CPTrad <- as.character(df$CPTrad)
df$UNMP <- as.character(df$UNMP)
df$DEMTot <- as.character(df$DEMTot)
df$UNMModP <- as.character(df$UNMModP)
df$DEMMod <- as.character(df$DEMMod)
df$CPAnyN <- as.character(df$CPAnyN)
df$DEMAny <- as.character(df$DEMAny)
df$CPModN <- as.character(df$CPModN)
df$UNMN <- as.character(df$UNMN)
df$UNMModN <- as.character(df$UNMModN)
df$CPTradN <- as.character(df$CPTradN)
df$DEMTotN <- as.character(df$DEMTotN)

# https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups
# Classification of the economies 
CLASS <- read_excel("data/CLASS.xlsx")
CLASS$iso_2c <- countrycode(CLASS$Economy, origin = "country.name", destination="iso2c")
CLASS <- CLASS %>% select(iso_2c, `Income group`)
df <- df %>% left_join(CLASS, by="iso_2c")
# Vietnam Lower middle income
df$`Income group`[120] <- "Lower middle income"


write.csv(df, "data/covariates.csv")



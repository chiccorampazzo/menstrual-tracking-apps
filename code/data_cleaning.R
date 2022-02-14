# Cleaning the data

library(tidyverse)
library(maps)
library(sf)
library(countrycode)
library("rnaturalearth")
library("rnaturalearthdata")
library(rgdal)
library(janitor)
options(scipen=999)

#######################################################################################################################
# Read files
#######################################################################################################################

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

write.csv(sum, "./Data/sum.csv")


# 0: App Reviews ####
## 0.1: Import results from Python script ####
rm(list = ls()) # start afresh

library(tidyverse)
library(data.table)
library(gdata)
english <- fread("03_data/02_output/0202_22reviewsenglish.csv", header=TRUE)
nonenglish <- fread("03_data/02_output/0202_22reviewsnonenglish.csv", header=TRUE)

mytopicdocumentmatrix <- rbind(english, nonenglish)

mytopicdocumentmatrix <- nonenglish %>%
  select(id, language, newcomparableid, '0':'21') %>%
  rename(app = id)

# So reviews from google and android for the same app have the same label
mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  mutate(app = case_when(app %in% c("clue-period-cycle-tracker", "com.clue.android") ~ "Clue", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("flo-health-period-tracker", "org.iggymedia.periodtracker") ~ "Flo", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("co.quanyong.pinkbird", "period-tracker-by-pinkbird") ~ "Period tracker by PinkBird", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("glow-period-fertility-tracker", "com.glow.android") ~ "GLOW", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("period-diary-cycle-tracker", "org.nanobit.perioddiary") ~ "Bellabeat", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.womanlog", "womanlog-period-calendar") ~ "WomanLog", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("period-tracker-eve", "com.glow.android.eve") ~ "Eve Period Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.naturalcycles.cordova", "natural-cycles-birth-control") ~ "Natural Cycles", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("fertility-friend-ff-app", "com.tamtris.fertilityfriend") ~ "Fertility Friend", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("ovia-fertility-cycle-tracker", "com.ovuline.fertility") ~ "Ovia", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("kindara-fertility-tracker", "com.kindara.pgap") ~ "Kindara", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.ladytimer.ovulationcalendar", "ladytimer-period-tracker") ~ "Ladytimer", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.period.tracker.lite", "period-tracker-by-gp-apps") ~ "Period Tracker Lite", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("maya-my-period-tracker", "in.plackal.lovecyclesfree") ~ "Maya", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.avawomen.com.ava_android.production", "ava-fertility-tracker") ~ "Ava fertility tracker", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("org.femmhealth.femm", "femm-period-ovulation-tracker") ~ "FEMM", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("tempdrop", "com.tempdrop.tempdropmobileapp") ~ "Tempdrop", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("woom-ovulation-and-fertility", "com.woomfertility.woomapp") ~ "WOOM", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.period.calendar.ovulation.tracker.lunar", "lunar-period-tracker") ~ "Lunar", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.fairhavenhealth.ovagraph", "ovagraph-official-tcoyf-app") ~ "OvaGraph", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("ovulation-tracker-by-premom", "premom.eh.com.ehpremomapp") ~ "Premom Ovulation Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("nurx-healthcare-from-home", "com.nurx") ~ "Nurx", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.cg.android.ptracker", "period-tracker") ~ "Period Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.wachanga.womancalendar", "clover-period-tracker-calendar") ~ "Period tracker for women", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("ovulationcalculator.com.ovulationcalculator", "ovulation-calculator-fertile-tracker-calendar-oc") ~ "Ovulation Calculator", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("com.brc.PeriodTrackerDiary", "my-calendar-period-tracker") ~ "Period Tracker & Diary", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("tracker-ovulation-calendar", "com.AvvaStyle.femalecalendar") ~ "Woman Log & Ovulation Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(app %in% c("easy-period-lite-tracker", "com.imzhiqiang.period") ~ "Easy Period", TRUE ~ app))

numberofapps <- as.data.frame(table(mytopicdocumentmatrix$app)) # checking the frequency of reviews - also useful when removing
write_csv(numberofapps, "03_data/02_output/0300_0_numberofapps.csv")

## changing the name of variable for easier coding (better when non-numeric)
mytopicdocumentmatrix <- mytopicdocumentmatrix %>% rename(x0 = '0', x1 = '1', x2 = '2', x3 = '3', x4 = '4', x5 = '5',
                                                          x6 = '6', x7 = '7', x8 = '8', x9 = '9', x10 = '10',
                                                          x11 = '11', x12 = '12',x13 = '13',x14 = '14', x15 = '15',
                                                          x16 = '16',x17 = '17', x18 = '18', x19 = '19',x20 = '20',
                                                          x21 = '21')

# creating an average for apps
summarised_mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  group_by(app) %>%
  mutate(average_0 = (sum(x0)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_1 = (sum(x1)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19,  x20, x21)))) %>%
  mutate(average_2 = (sum(x2)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19 , x20, x21)))) %>%
  mutate(average_3 = (sum(x3)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_4 = (sum(x4)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_5 = (sum(x5)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_6 = (sum(x6)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_7 = (sum(x7)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_8 = (sum(x8)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_9 = (sum(x9)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_10= (sum(x10)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_11= (sum(x11)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_12= (sum(x12)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_13= (sum(x13)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_14= (sum(x14)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_15= (sum(x15)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_16= (sum(x16)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_17= (sum(x17)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_18= (sum(x18)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_19= (sum(x19)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_20= (sum(x20)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_21= (sum(x21)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%

  distinct(app, .keep_all = TRUE) %>%
  select(app, everything())

# Removing the original and only keeping the average
summarised_mytopicdocumentmatrix <- summarised_mytopicdocumentmatrix %>% select(-contains("x"))
write_csv(summarised_mytopicdocumentmatrix, "03_data/02_output/0300_topicdocumentmatrix_detailed_summarised.csv")
summarised_mytopicdocumentmatrix <- fread("03_data/02_output/0300_topicdocumentmatrix_detailed_summarised.csv", header=TRUE)


## 0.2: Creating broad topics ####
### Based on the top 10 words of each topic, group into broader topics for figures
mytopics <- fread("03_data/02_output/0201_22mytopic_lists.csv", header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")

mytopics$broadergroup <- "" # need an empty column
mytopics <- mytopics %>%
  mutate(broadergroup = case_when(ID == 1 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 2 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 3 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 4 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 5 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 6 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 7 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 8 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 9 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 10 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 11 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 12 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 13 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 14 ~ "Ease of life", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 15 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 16 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 17 ~ "Community", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 18 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 19 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 20 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 21 ~ "Natural contraception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 22 ~ "Design/Recommendation", TRUE ~ broadergroup))
table(mytopics$broadergroup) # check that all topics are assigned

mytopics$topwords <-  mytopics$broadergroup
mytopics <- mytopics %>% select(-broadergroup)
write_csv(mytopics, "03_data/02_output/0301_mytopic_lists_recoded.csv")

### 0.3: Keeping the original topics ####
mytopics <- fread("03_data/02_output/0201_22mytopic_lists.csv", header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")

write_csv(mytopics, "03_data/02_output/0301_mytopic_lists_original_shortened.csv")

### 0.4: Topic look-up ####
mytopics <- fread("03_data/02_output/0201_22mytopic_lists.csv", header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")

mytopics$broadergroup <- "" # need an empty column
mytopics <- mytopics %>%
  mutate(broadergroup = case_when(ID == 1 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 2 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 3 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 4 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 5 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 6 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 7 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 8 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 9 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 10 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 11 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 12 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 13 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 14 ~ "Ease of life", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 15 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 16 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 17 ~ "Community", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 18 ~ "Trying to conceive", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 19 ~ "Design/Recommendation", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 20 ~ "Period tracker", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 21 ~ "Natural contraception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 22 ~ "Design/Recommendation", TRUE ~ broadergroup))
write_csv(mytopics, "03_data/02_output/0301_mytopic_lists_lookup.csv")

## 1: Number of downloads ####
load("03_data/02_output/ldf.Rdata") # this is from google drive the app names

ldf_maxnumberinstalls <- ldf %>%
  select(appId, maxInstalls) %>%
  rename(newapp = appId) %>%
  mutate(app = NA_character_) %>%
  mutate(app = case_when(newapp %in% c("clue-period-cycle-tracker", "com.clue.android") ~ "Clue", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("flo-health-period-tracker", "org.iggymedia.periodtracker") ~ "Flo", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("co.quanyong.pinkbird", "period-tracker-by-pinkbird") ~ "Period tracker by PinkBird", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("glow-period-fertility-tracker", "com.glow.android") ~ "GLOW", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("period-diary-cycle-tracker", "org.nanobit.perioddiary") ~ "Bellabeat", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.womanlog", "womanlog-period-calendar") ~ "WomanLog", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("period-tracker-eve", "com.glow.android.eve") ~ "Eve Period Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.naturalcycles.cordova", "natural-cycles-birth-control") ~ "Natural Cycles", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("fertility-friend-ff-app", "com.tamtris.fertilityfriend") ~ "Fertility Friend", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("ovia-fertility-cycle-tracker", "com.ovuline.fertility") ~ "Ovia", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("kindara-fertility-tracker", "com.kindara.pgap") ~ "Kindara", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.ladytimer.ovulationcalendar", "ladytimer-period-tracker") ~ "Ladytimer", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.period.tracker.lite", "period-tracker-by-gp-apps") ~ "Period Tracker Lite", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("maya-my-period-tracker", "in.plackal.lovecyclesfree") ~ "Maya", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.avawomen.com.ava_android.production", "ava-fertility-tracker") ~ "Ava fertility tracker", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("org.femmhealth.femm", "femm-period-ovulation-tracker") ~ "FEMM", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("tempdrop", "com.tempdrop.tempdropmobileapp") ~ "Tempdrop", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("woom-ovulation-and-fertility", "com.woomfertility.woomapp") ~ "WOOM", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.period.calendar.ovulation.tracker.lunar", "lunar-period-tracker") ~ "Lunar", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.fairhavenhealth.ovagraph", "ovagraph-official-tcoyf-app") ~ "OvaGraph", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("ovulation-tracker-by-premom", "premom.eh.com.ehpremomapp") ~ "Premom Ovulation Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("nurx-healthcare-from-home", "com.nurx") ~ "Nurx", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.cg.android.ptracker", "period-tracker") ~ "Period Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.wachanga.womancalendar", "clover-period-tracker-calendar") ~ "Period tracker for women", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("ovulationcalculator.com.ovulationcalculator", "ovulation-calculator-fertile-tracker-calendar-oc") ~ "Ovulation Calculator", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("com.brc.PeriodTrackerDiary", "my-calendar-period-tracker") ~ "Period Tracker & Diary", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("tracker-ovulation-calendar", "com.AvvaStyle.femalecalendar") ~ "Woman Log & Ovulation Tracker", TRUE ~ app)) %>%
  mutate(app = case_when(newapp %in% c("easy-period-lite-tracker", "com.imzhiqiang.period") ~ "Easy Period", TRUE ~ app))



ldf_maxnumberinstalls <- ldf_maxnumberinstalls %>% rename(App = app)
numberofapps <- numberofapps %>% rename(App = Var1) %>% rename(Numberofreviews = Freq)
numberofappsdescriptions <- full_join(numberofapps, ldf_maxnumberinstalls, by = "App")
numberofappsdescriptions <- numberofappsdescriptions %>%
  filter(!is.na(App)) %>%
  select(-newapp)
numberofappsdescriptions[is.na(numberofappsdescriptions)] <- 10000

write_csv(numberofappsdescriptions, "03_data/02_output/0300_numberofappsdescriptions.csv")


## 2. Languages of reviews ####
rm(list = ls()) # start afresh

library(tidyverse)
library(data.table)
library(gdata)
english <- fread("03_data/02_output/0202_22reviewsenglish.csv", header=TRUE)
nonenglish <- fread("03_data/02_output/0202_22reviewsnonenglish.csv", header=TRUE)


mytopicdocumentmatrix <- rbind(english, nonenglish)
names(mytopicdocumentmatrix) # the topics are labeled 0 - 21 instead of 1 - 22, but that's okay
mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, language, newcomparableid, '0':'21') %>%
  rename(app = id)

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  mutate(language = case_when(language == "en" ~ "English", TRUE ~ language)) %>%
  mutate(language = case_when(language == "pt" ~ "Portuguese", TRUE ~ language)) %>%
  mutate(language = case_when(language == "es" ~ "Spanish", TRUE ~ language)) %>%
  mutate(language = case_when(language == "ru" ~ "Russian", TRUE ~ language)) %>%
  mutate(language = case_when(language == "fr" ~ "French", TRUE ~ language)) %>%
  mutate(language = case_when(language == "de" ~ "German", TRUE ~ language)) %>%
  mutate(language = case_when(language == "it" ~ "Italian", TRUE ~ language)) %>%
  mutate(language = case_when(language == "ko" ~ "Korean", TRUE ~ language)) %>%
  mutate(language = case_when(language == "pl" ~ "Polish", TRUE ~ language)) %>%
  mutate(language = case_when(language == "sv" ~ "Swedish", TRUE ~ language)) %>%
  mutate(language = case_when(language == "tr" ~ "Turkish", TRUE ~ language)) %>%
  mutate(language = case_when(language == "vi" ~ "vietnamese", TRUE ~ language)) %>%
  mutate(language = case_when(language == "ar" ~ "arabic", TRUE ~ language)) %>%
  mutate(language = case_when(language == "nl" ~ "Dutch", TRUE ~ language)) %>%
  mutate(language = case_when(language == "id" ~ "Indonesian", TRUE ~ language))
# So reviews from google and android for the same app have the same label
toplangauges <- as.data.frame(table(mytopicdocumentmatrix$language))
toplangauges <- toplangauges %>% arrange(-Freq) %>%
  rename(language = Var1)

top20languages <- as.character(head(toplangauges$language, 15))


write_csv(toplangauges, "03_data/02_output/0300_top_languages.csv")
write_rds(top20languages,  "03_data/02_output/0300_top_20_languages_character.csv")

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  filter(language %in% c(top20languages))
## changing the name of variable for easier coding (better when non-numeric)
mytopicdocumentmatrix <- mytopicdocumentmatrix %>% rename(x0 = '0', x1 = '1', x2 = '2', x3 = '3', x4 = '4', x5 = '5',
                                                          x6 = '6', x7 = '7', x8 = '8', x9 = '9', x10 = '10',
                                                          x11 = '11', x12 = '12',x13 = '13',x14 = '14', x15 = '15',
                                                          x16 = '16',x17 = '17', x18 = '18', x19 = '19',x20 = '20',
                                                          x21 = '21')


summarised_mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  group_by(language) %>%
  mutate(average_0 = (sum(x0)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_1 = (sum(x1)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19,  x20, x21)))) %>%
  mutate(average_2 = (sum(x2)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19 , x20, x21)))) %>%
  mutate(average_3 = (sum(x3)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_4 = (sum(x4)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_5 = (sum(x5)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_6 = (sum(x6)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_7 = (sum(x7)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_8 = (sum(x8)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_9 = (sum(x9)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,   x11, x12, x13, x14, x15, x16, x17, x18,
                                    x19, x20, x21)))) %>%
  mutate(average_10= (sum(x10)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_11= (sum(x11)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_12= (sum(x12)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_13= (sum(x13)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_14= (sum(x14)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_15= (sum(x15)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_16= (sum(x16)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_17= (sum(x17)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_18= (sum(x18)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_19= (sum(x19)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_20= (sum(x20)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%
  mutate(average_21= (sum(x21)/sum(c(x0,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,  x11, x12, x13, x14, x15, x16, x17, x18,
                                     x19, x20, x21)))) %>%

  distinct(language, .keep_all = TRUE) %>%
  select(language, everything())

# Removing the original and only keeping the average
summarised_mytopicdocumentmatrix <- summarised_mytopicdocumentmatrix %>% select(-contains("x")) %>%
  select(-app, -newcomparableid)
write_csv(summarised_mytopicdocumentmatrix, "03_data/02_output/0300_topicdocumentmatrix_languages.csv")



# 3: App Descriptions ####
rm(list = ls()) # start afresh

library(tidyverse)
library(data.table)
library(gdata)
english <- fread("03_data/02_output/120214descriptionsenglish.csv", header=TRUE)


mytopicdocumentmatrix <- english
mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, language, newcomparableid, '0':'13') %>%
  rename(app = id)

## changing the name of variable for easier coding (better when non-numeric)
mytopicdocumentmatrix <- mytopicdocumentmatrix %>% rename(x0 = '0', x1 = '1', x2 = '2', x3 = '3', x4 = '4', x5 = '5',
                                                          x6 = '6', x7 = '7', x8 = '8', x8 = '8', x9 = '9', x10 = '10',
                                                          x11 = '11', x12 = '12', x13 = '13')
write_csv(mytopicdocumentmatrix, "03_data/02_output/0300_descriptions_documentmatrix.csv")

### Based on the top 10 words of each topic, group into broader topics for figures
mytopics <- fread("03_data/02_output/1202_14mytopic_lists.csv", header=TRUE) # faster than read.csv#
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")

mytopics$broadergroup <- "" # need an empty column
mytopics <- mytopics %>%
  mutate(broadergroup = case_when(ID == 1 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 2 ~ "Menstruation prediction", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 3 ~ "Menstruation prediction", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 4 ~ "Empower", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 5 ~ "Conception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 6 ~ "Conception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 7 ~ "Conception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 8 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 9 ~ "Health", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 10 ~ "Understanding your body", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 11 ~ "Medication", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 12 ~ "Birth control/Contraception", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 13 ~ "Menstruation prediction", TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(ID == 14 ~ "Understanding your body", TRUE ~ broadergroup))
table(mytopics$broadergroup) # check that all topics are assigned

#mytopics$originaltopics <-  mytopics$topwords
# mytopics$topwords <-  mytopics$broadergroup
# mytopics <- mytopics %>% select(-broadergroup)
write_csv(mytopics, "03_data/02_output/0300_descriptions_mytopic_lists_recoded.csv")

## now, move onto script 0301_figures

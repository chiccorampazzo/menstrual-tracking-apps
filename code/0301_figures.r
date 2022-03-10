# 10: Descriptions ####
rm(list = ls()) # start afresh
library(tidyverse)
library(data.table)

apporder <- c(
  'Clue',
  'Flo',
  'Period Tracker Lite',
  'Bellabeat',
  'Period Tracker',
  'WomanLog',
  'GLOW',
  'Ovia',
  'Maya',
  'Eve Period Tracker',
  'Ladytimer',
  'Period tracker for women',
  'Period tracker by PinkBird',
  'Kindara',
  'Natural Cycles',
  'Fertility Friend',
  'Period Tracker & Diary',
  'WOOM',
  'Nurx',
  'Ava fertility tracker',
  'OvaGraph',
  'FEMM',
  'Tempdrop',
  'Easy Period',
  'Woman Log & Ovulation Tracker',
  'Premom Ovulation Tracker',
  'Ovulation Calculator',
  'Lunar')

### Load descriptions data ###
desciptions <- fread("03_data/02_output/0300_descriptions_documentmatrix.csv", header=TRUE) # from 0300
descriptionstopics <- fread("03_data/02_output/0300_descriptions_mytopic_lists_recoded.csv", header=TRUE) # from 0300
descriptionstopics <- descriptionstopics %>% select(-broadergroup)
### cleaning descriptions to get kind of data we want
desciptions_transposed <- desciptions %>%
  select(-c(language, newcomparableid)) %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname,
               values_to = "value", values_transform = list(value = as.character)) %>%
  pivot_wider(names_from=rowname, values_from=value) %>%
  select(-name) # as it starts at 0

names(desciptions_transposed) <- desciptions_transposed[1,]
desciptions_transposed <- desciptions_transposed[-1,]
desciptions_transposed <- rowid_to_column(desciptions_transposed, "ID")

desciptions_merged <- merge(desciptions_transposed, descriptionstopics, by = "ID")
desciptions_merged <- desciptions_merged %>%
  select(-ID) %>%
  select(topwords, everything())

###  some reshaping
desciptions_toplot <-  reshape2::melt(desciptions_merged, id.vars="topwords")
desciptions_toplot$value <- as.numeric(desciptions_toplot$value)

desciptions_toplot <- desciptions_toplot %>%
  group_by(topwords, variable) %>%
  mutate(value = sum(value)) %>%
  distinct(value, topwords, variable) %>%
  group_by(variable) %>%
  group_by(variable) %>%
  mutate(tomultiplyby = sum(value)) %>%
  ungroup() %>%
  mutate(value = value/tomultiplyby) %>%
  select(-tomultiplyby) # this gives us a new weighted value

desciptions_toplot <- desciptions_toplot %>%
  rename(App = variable) %>%
  rename(topwords = topwords)

desciptions_toplot_withtext <- desciptions_toplot %>%
  mutate(completevaluetext = value * 100)
desciptions_toplot_withtext$completevaluetext <-  format(round(desciptions_toplot_withtext$completevaluetext, 1), nsmall = 1)
desciptions_toplot_withtext$completevaluetext <-  paste0(desciptions_toplot_withtext$completevaluetext, "%")

desciptions_toplot_withtext <- desciptions_toplot_withtext %>%
  group_by(topwords) %>%
  mutate(comparativevalueaverage = mean(value)) %>%
  ungroup() %>%
  mutate(comparativevalues = value / comparativevalueaverage)





### 10.1: Setup further ####
desciptions_toplot_withtext$value <- as.numeric(desciptions_toplot_withtext$value)
desciptions_barplot <- desciptions_toplot_withtext %>%
  dplyr::group_by(topwords) %>%
  dplyr::mutate(comparativevalueaverage = mean(value)) %>%
  ungroup() %>%
  mutate(comparativevalues = value / comparativevalueaverage)


reviews_final <- desciptions_barplot %>%
  arrange(value,topwords) %>%
  mutate(topwords=as.character(topwords)) %>%
  mutate(topwords=factor(topwords,levels = unique(topwords),
                         ordered = T))

### 10.2: Getting number of downloads, for successful proportions ####
numberofapps <- fread("03_data/02_output/0300_0_numberofapps.csv") # so we can add this in a label on the left
numberofapps <- numberofapps %>% rename(App = Var1)



reviews_final$App <- as.character(reviews_final$App)

## removing the word description ##
reviews_final$App <- str_replace(reviews_final$App, "Description_",  "")



### 10.3: Rename App names ####
## created 9/3/2022 to see if the names Francesco sent are here
testnames <- as.data.frame(unique(reviews_final$App))
testnames %>% filter(!`unique(reviews_final$App)` %in% apporder)

reviews_final <- reviews_final %>%
  mutate(App = case_when(App == "Ava"  ~ "Ava fertility tracker", TRUE ~ App),
         App = case_when(App == "Pinkbird"  ~ "Period tracker by PinkBird", TRUE ~ App),
         App = case_when(App == "Femm"  ~ "FEMM", TRUE ~ App),
         App = case_when(App == "Woom"  ~ "WOOM", TRUE ~ App),
         App = case_when(App == "Glow"  ~ "GLOW", TRUE ~ App),
         App = case_when(App == "Woom"  ~ "WOOM", TRUE ~ App),
         App = case_when(App == "Woman Log"  ~ "WomanLog", TRUE ~ App),
         App = case_when(App == "Woman Log (AvvaStyle)"  ~ "Woman Log & Ovulation Tracker", TRUE ~ App),
         App = case_when(App == "Period Tracker (Sevenlogics)"  ~ "Period Tracker", TRUE ~ App),
         App = case_when(App == "Period Tracker (Wachanga)"  ~ "Period tracker for women", TRUE ~ App),
         App = case_when(App == "Period Tracker (Living Better)"  ~ "Period Tracker & Diary", TRUE ~ App),
         App = case_when(App == "Evs"  ~ "Eve Period Tracker", TRUE ~ App),
         App = case_when(App == "Ovagraph"  ~ "OvaGraph", TRUE ~ App))


  # mutate(App = case_when(App == "PeriodTrackerLite"  ~ "Period Tracker Lite", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "PeriodTracker_sevenlogics"  ~ "Period Tracker (Sevenlogics)", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "fertilityfriend"  ~ "Fertility Friend", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "naturalcycles"  ~ "Natural Cycles", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "PeriodTrackerLivingBetter"  ~ "Period Tracker (Living Better)", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "PeriodTrackerWachanga"  ~ "Period Tracker (Wachanga)", TRUE ~ App)) %>%
  # mutate(App = case_when(App == "womanlog"  ~ "Woman Log", TRUE ~ App))

reviews_final <- merge(reviews_final, numberofapps, by = "App")
reviews_final$Freq <- format(round(as.numeric(reviews_final$Freq), 1), big.mark=",")
reviews_final$Freq <- gsub("[[:space:]]", "", reviews_final$Freq)




### re-introducing broader group
descriptionstopics_broader <- fread("03_data/02_output/0300_descriptions_mytopic_lists_recoded.csv", header=TRUE) # from 0300

test1 <- merge(reviews_final, descriptionstopics_broader, by = "topwords")
test2 <- test1 %>%
  group_by(broadergroup) %>%
  summarise(value = sum(as.numeric(value)*as.numeric(gsub(",","",Freq)))) %>%
  ungroup() %>%
  mutate(overalltotal = sum(value)) %>%
  mutate(value = value/overalltotal) %>%
  mutate(App = "Total") %>%
  select(-overalltotal)
test1 <- test1 %>% select(App, broadergroup, value)
test2 <- test2 %>% select(App, broadergroup, value)

test4 <- rbind(test1, test2)

test4$App <- factor(test4$App, levels=c(rev(apporder), "Total"))
unique(test4$broadergroup)
broadergoruplevels <- c("Menstruation prediction", "Conception",
                        "Birth control/Contraception", "Understanding your body", "Health", "Empower","Medication")
test4$broadergroup <- factor(test4$broadergroup, levels=c(rev(broadergoruplevels)))

### old colors
# colorsstandard <- c("#13124e", # periodtrackertopics / Menstruation prediction
#                     "#3f458d", # conceptiontopics / Birth control/Contraception
#                     "#9c9bdb", # understandbodytopics / Understanding body
#                     "#f49600", # / Contraception
#                     "#afa10d", # / Health
#                     "#20581c", # / Medication
#                     "#b28573") # / Empower
#                     #"#182931", # easeoflifetopics
#                     #"#dad3de" # community) # easeoflifetopics

### new colors from francesco 9/3/2022
colorsstandard <- c("#ff6961","#ffb340", "#ffd426", "#30DB5B", "#66d4cf", "#64d2ff", "#409cff")#, #"#7d7aff", "#da8fff", "#ac8e68")

ggplot(test4,aes(x=App,y=value, fill=broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    #labels=c("Period tracker", "Conception", "Understanding body", "Contraception", "Empower", "Health", "Medication"),
                    values = c(rev(colorsstandard))
                                         ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$App)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$App)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "none",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_descriptions.pdf"), width = 14, height = 10)


#### 10.4 legend order ####
# note: for the right legend order, need to undeo reverse
forthelegend <- test4

broadergoruplevels <- c("Menstruation prediction", "Conception",
                        "Birth control/Contraception", "Understanding your body", "Health", "Empower","Medication")
forthelegend$broadergroup <- factor(forthelegend$broadergroup, levels=c(broadergoruplevels))



ggplot(forthelegend,aes(x=App,y=value, fill=broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    #labels=c("Period tracker", "Conception", "Understanding body", "Contraception", "Empower", "Health", "Medication"),
                    values = c(colorsstandard)
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$App)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$App)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_descriptions_legend_only.pdf"), width = 17, height = 10)







# 20: Reviews ####
rm(list = ls()) # start afresh

apporder <- c(
  'Clue',
  'Flo',
  'Period Tracker Lite',
  'Bellabeat',
  'Period Tracker',
  'WomanLog',
  'GLOW',
  'Ovia',
  'Maya',
  'Eve Period Tracker',
  'Ladytimer',
  'Period tracker for women',
  'Period tracker by PinkBird',
  'Kindara',
  'Natural Cycles',
  'Fertility Friend',
  'Period Tracker & Diary',
  'WOOM',
  'Nurx',
  'Ava fertility tracker',
  'OvaGraph',
  'FEMM',
  'Tempdrop',
  'Easy Period',
  'Woman Log & Ovulation Tracker',
  'Premom Ovulation Tracker',
  'Ovulation Calculator',
  'Lunar')

periodtrackertopics <- c('point, spot, accuracy', 'cycle, cycles, menstrual', 'period, know, start', 'period, tracking, using',
                         'best, ever, far')
conceptiontopics <- c('days, ovulation, date', 'tool, excellent, lots', 'fertility, easy, chart', 'pregnant, trying, conceive')
understandbodytopics <- c('wish, add, could', 'symptoms, moods, keep')
naturalconceptiontopics <- c('body, control, natural')
easeoflifetopics <- c('keeps, everything, keeping')
communitytopcis <- c('community, love, questions')
designrecommendationtopics <- c('years, year, using', 'free, premium, pay', 'works, job, good', 'insurance, get, service',
                                'simple, friendly, cute', 'must, recommended, thank', 'live, cant, language', 'update, new, open')


### Load reviews data ###
reviews <- fread("03_data/02_output/0300_topicdocumentmatrix_detailed_summarised.csv", header=TRUE) # from 0300
reviewsstopics <- fread("03_data/02_output/0301_mytopic_lists_lookup.csv", header=TRUE)


reviewsstopics <- reviewsstopics %>% select(-broadergroup)
### cleaning descriptions to get kind of data we want
reviews_transposed <- reviews %>%
  select(-c(language, newcomparableid)) %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname,
               values_to = "value", values_transform = list(value = as.character)) %>%
  pivot_wider(names_from=rowname, values_from=value) %>%
  select(-name) # as it starts at 0

names(reviews_transposed) <- reviews_transposed[1,]
reviews_transposed <- reviews_transposed[-1,]
reviews_transposed <- rowid_to_column(reviews_transposed, "ID")

reviews_merged <- merge(reviews_transposed, reviewsstopics, by = "ID")
reviews_merged <- reviews_merged %>%
  select(-ID) %>%
  select(topwords, everything())

### Some reshaping
reviews_toplot <-  reshape2::melt(reviews_merged, id.vars="topwords")
reviews_toplot <- reviews_toplot %>%
  filter(!topwords %in% designrecommendationtopics)

reviews_toplot$value <- as.numeric(reviews_toplot$value)

reviews_toplot <- reviews_toplot %>%
  group_by(topwords, variable) %>%
  mutate(value = sum(value)) %>%
  distinct(value, topwords, variable) %>%
  group_by(variable) %>%
  group_by(variable) %>%
  mutate(tomultiplyby = sum(value)) %>%
  ungroup() %>%
  mutate(value = value/tomultiplyby) %>%
  select(-tomultiplyby) # this gives us a new weighted value

reviews_toplot <- reviews_toplot %>%
  rename(App = variable) %>%
  rename(topwords = topwords)

reviews_toplot_withtext <- reviews_toplot %>%
  mutate(completevaluetext = value * 100)
reviews_toplot_withtext$completevaluetext <-  format(round(reviews_toplot_withtext$completevaluetext, 1), nsmall = 1)
reviews_toplot_withtext$completevaluetext <-  paste0(reviews_toplot_withtext$completevaluetext, "%")

reviews_toplot_withtext <- reviews_toplot_withtext %>%
  group_by(topwords) %>%
  mutate(comparativevalueaverage = mean(value)) %>%
  ungroup() %>%
  mutate(comparativevalues = value / comparativevalueaverage)





### 20.1: Setup further ####
reviews_toplot_withtext$value <- as.numeric(reviews_toplot_withtext$value)
reviews_barplot <- reviews_toplot_withtext %>%
  dplyr::group_by(topwords) %>%
  dplyr::mutate(comparativevalueaverage = mean(value)) %>%
  ungroup() %>%
  mutate(comparativevalues = value / comparativevalueaverage)


reviews_final <- reviews_barplot %>%
  arrange(value,topwords) %>%
  mutate(topwords=as.character(topwords)) %>%
  mutate(topwords=factor(topwords,levels = unique(topwords),
                         ordered = T))

### 20.2: Getting number of downloads, for successful proportions ####
numberofapps <- fread("03_data/02_output/0300_0_numberofapps.csv") # so we can add this in a label on the left
numberofapps <- numberofapps %>% rename(App = Var1)



reviews_final$App <- as.character(reviews_final$App)

## removing the word description ##
reviews_final$App <- str_replace(reviews_final$App, "Description_",  "")

reviews_final <- reviews_final %>%
  mutate(App = case_when(App == "Ava"  ~ "Ava fertility tracker", TRUE ~ App),
         App = case_when(App == "Pinkbird"  ~ "Period tracker by PinkBird", TRUE ~ App),
         App = case_when(App == "Femm"  ~ "FEMM", TRUE ~ App),
         App = case_when(App == "Woom"  ~ "WOOM", TRUE ~ App),
         App = case_when(App == "Glow"  ~ "GLOW", TRUE ~ App),
         App = case_when(App == "Woom"  ~ "WOOM", TRUE ~ App),
         App = case_when(App == "Woman Log"  ~ "WomanLog", TRUE ~ App),
         App = case_when(App == "Woman Log (AvvaStyle)"  ~ "Woman Log & Ovulation Tracker", TRUE ~ App),
         App = case_when(App == "Period Tracker (Sevenlogics)"  ~ "Period Tracker", TRUE ~ App),
         App = case_when(App == "Period Tracker (Wachanga)"  ~ "Period tracker for women", TRUE ~ App),
         App = case_when(App == "Period Tracker (Living Better)"  ~ "Period Tracker & Diary", TRUE ~ App),
         App = case_when(App == "Evs"  ~ "Eve Period Tracker", TRUE ~ App),
         App = case_when(App == "Ovagraph"  ~ "OvaGraph", TRUE ~ App))


reviews_final <- merge(reviews_final, numberofapps, by = "App")
reviews_final$Freq <- format(round(as.numeric(reviews_final$Freq), 1), big.mark=",")
reviews_final$Freq <- gsub("[[:space:]]", "", reviews_final$Freq)




### re-introducing broader group
reviewsstopics_broader <- fread("03_data/02_output/0301_mytopic_lists_lookup.csv", header=TRUE)

test1 <- merge(reviews_final, reviewsstopics_broader, by = "topwords")
test2 <- test1 %>%
  group_by(broadergroup) %>%
  summarise(value = sum(as.numeric(value)*as.numeric(gsub(",","",Freq)))) %>%
  ungroup() %>%
  mutate(overalltotal = sum(value)) %>%
  mutate(value = value/overalltotal) %>%
  mutate(App = "Total") %>%
  select(-overalltotal)
test1 <- test1 %>% select(App, broadergroup, value)
test2 <- test2 %>% select(App, broadergroup, value)

test4 <- rbind(test1, test2)

test4$App <- factor(test4$App, levels=c(rev(apporder), "Total"))

unique(test4$broadergroup)


broadergoruplevels <- c("Period tracker", "Trying to conceive","Natural contraception", "Understanding your body",
                       "Ease of life", "Community")
test4$broadergroup <- factor(test4$broadergroup, levels=c(rev(broadergoruplevels)))

colorsstandard <- c("#ff6961","#ffb340", "#ffd426",  "#30DB5B",# "#66d4cf", "#64d2ff", "#409cff"
                     "#7d7aff", "#da8fff")#, "#ac8e68")

# colorsstandard <- c("#13124e", # periodtrackertopics / Menstruation prediction
#                     "#3f458d", # conceptiontopics / Birth control/Contraception
#                     "#9c9bdb", # understandbodytopics / Understanding body
#                     "#f49600", # naturalcontraception/ Contraception
#                     #"#afa10d", # / Health
#                     #"#20581c", # / Medication
#                     #"#b28573", # / Empower
#                     "#182931", # easeoflifetopics
#                     "#dad3de" # community
#                     )

ggplot(test4,aes(x=App,y=value, fill=broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    values = c(rev(colorsstandard)
                    )) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$App)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$App)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "none",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_reviews.pdf"), width = 14, height = 10)


#### 20.3 legend order ####
# note: for the right legend order, need to undeo reverse
forthelegend <- test4

broadergoruplevels <- c("Period tracker", "Trying to conceive","Natural contraception", "Understanding your body",
                        "Ease of life", "Community")
forthelegend$broadergroup <- factor(forthelegend$broadergroup, levels=c(broadergoruplevels))



ggplot(forthelegend,aes(x=App,y=value, fill=broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    labels=c("Menstruation prediction", "Conception", "Birth control/Contraception", "Understanding your your body",
                    "Ease of life",  "Community"),
                    values = c("#ff6961","#ffb340", "#ffd426",  "#30DB5B",# "#66d4cf", "#64d2ff", "#409cff"
                               "#7d7aff", "#da8fff")) +#, "#ac8e68")) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$App)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$App)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_reviews_legend_only.pdf"), width = 17, height = 10)











# 30: Languages ####

languageorder <- c(
  'Clue',
  'Flo',
  'Period Tracker Lite',
  'Bellabeat',
  'Period Tracker',
  'WomanLog',
  'GLOW',
  'Ovia',
  'Maya',
  'Eve Period Tracker',
  'Ladytimer',
  'Period tracker for women',
  'Period tracker by PinkBird',
  'Kindara',
  'Natural Cycles',
  'Fertility Friend',
  'Period Tracker & Diary',
  'WOOM',
  'Nurx',
  'Ava fertility tracker',
  'OvaGraph',
  'FEMM',
  'Tempdrop',
  'Easy Period',
  'Woman Log & Ovulation Tracker',
  'Premom Ovulation Tracker',
  'Ovulation Calculator',
  'Lunar')

periodtrackertopics <- c('point, spot, accuracy', 'cycle, cycles, menstrual', 'period, know, start', 'period, tracking, using',
                         'best, ever, far')
conceptiontopics <- c('days, ovulation, date', 'tool, excellent, lots', 'fertility, easy, chart', 'pregnant, trying, conceive')
understandbodytopics <- c('wish, add, could', 'symptoms, moods, keep')
naturalconceptiontopics <- c('body, control, natural')
easeoflifetopics <- c('keeps, everything, keeping')
communitytopcis <- c('community, love, questions')
designrecommendationtopics <- c('years, year, using', 'free, premium, pay', 'works, job, good', 'insurance, get, service',
                                'simple, friendly, cute', 'must, recommended, thank', 'live, cant, language', 'update, new, open')

# 15 topics of use

## 30.1. Data setup ####
# I don't want to interfere with 0306,

library(tidyverse)
library(data.table)
summarised_labelsandtopics <- fread("03_data/02_output/0300_topicdocumentmatrix_languages.csv")
mytopics <- fread("03_data/02_output/0301_mytopic_lists_original_shortened.csv", header=TRUE) # USING THE ORIGINAL (only real difference between
# this and 0306)
mytopicslookup <- fread("03_data/02_output/0301_mytopic_lists_lookup.csv", header=TRUE) # USING THE ORIGINAL (only real difference between


## Transpose for ease
summarised_labelsandtopics_transposed <- summarised_labelsandtopics %>%
  tibble::rownames_to_column() %>%
  pivot_longer(-rowname,
               values_to = "value", values_transform = list(value = as.character)) %>%
  pivot_wider(names_from=rowname, values_from=value) %>%
  select(-name) # as it starts at 0

names(summarised_labelsandtopics_transposed) <- summarised_labelsandtopics_transposed[1,]
summarised_labelsandtopics_transposed <- summarised_labelsandtopics_transposed[-1,]
summarised_labelsandtopics_transposed <- rowid_to_column(summarised_labelsandtopics_transposed, "ID")


## Merging the two (broad topics, and topic proportions)
topicnames_reviews <- merge(summarised_labelsandtopics_transposed, mytopics, by = "ID")
names(topicnames_reviews)
topicnames_reviews <- topicnames_reviews %>%
  select(-ID) %>%
  select(topwords, everything())

## Create new dataset for ease
reviews_toplot <-  reshape2::melt(topicnames_reviews, id.vars="topwords")
reviews_toplot$value <- as.numeric(reviews_toplot$value)


reviews_toplot %>% filter(!topwords %in% designrecommendationtopics)

## Group averages
reviews_toplot <- reviews_toplot %>%
  group_by(topwords, variable) %>%
  mutate(value = sum(value)) %>%
  distinct(value, topwords, variable) %>%
  group_by(variable) %>%
  mutate(tomultiplyby = case_when(topwords == designrecommendationtopics ~ value, TRUE ~ 0)) %>%
  filter(!topwords %in% designrecommendationtopics) %>%
  group_by(variable) %>%
  mutate(tomultiplyby = sum(value)) %>%
  ungroup() %>%
  mutate(value = value/tomultiplyby) %>%
  select(-tomultiplyby)

reviews_toplot <- reviews_toplot %>%
  rename(language = variable) %>%
  rename(topwords = topwords)

reviews_toplot_withtext <- reviews_toplot %>%
  mutate(completevaluetext = value * 100)
reviews_toplot_withtext$completevaluetext <-  format(round(reviews_toplot_withtext$completevaluetext, 1), nsmall = 1)
reviews_toplot_withtext$completevaluetext <-  paste0(reviews_toplot_withtext$completevaluetext, "%")

reviews_toplot_withtext$value <- as.numeric(reviews_toplot_withtext$value)
heatmap_withtext_reviews <- reviews_toplot_withtext %>%
  dplyr::group_by(topwords) %>%
  dplyr::mutate(comparativevalueaverage = mean(value)) %>%
  ungroup() %>%
  mutate(comparativevalues = value / comparativevalueaverage)

reviews_final <- heatmap_withtext_reviews %>%
  arrange(value,topwords) %>%
  mutate(topwords=as.character(topwords)) %>%
  mutate(topwords=factor(topwords,levels = unique(topwords),
                         ordered = T))


## A little bit of extra info
top20languages <- readRDS("03_data/02_output/0305_top_20_languages_character.csv")

reviews_final$language <- factor(reviews_final$language, levels = c(rev(top20languages)))

## 30.2 Bar chart ####
test <- reviews_final
test$intention <- factor(test$topwords, levels=c(
  periodtrackertopics,
  conceptiontopics,
  understandbodytopics,
  easeoflifetopics,
  communitytopcis,
  naturalconceptiontopics,
  designrecommendationtopics))
test$Broadergroup <- ""
test <- test %>%
  mutate(Broadergroup = case_when(topwords %in% periodtrackertopics ~ "periodtrackertopics", TRUE ~ Broadergroup)) %>%
  mutate(Broadergroup = case_when(topwords %in% conceptiontopics ~ "conceptiontopics", TRUE ~ Broadergroup)) %>%
  mutate(Broadergroup = case_when(topwords %in% understandbodytopics ~ "understandbodytopics", TRUE ~ Broadergroup)) %>%
  mutate(Broadergroup = case_when(topwords %in% easeoflifetopics ~ "easeoflifetopics", TRUE ~ Broadergroup)) %>%
  mutate(Broadergroup = case_when(topwords %in% communitytopcis ~ "communitytopcis", TRUE ~ Broadergroup)) %>%
  mutate(Broadergroup = case_when(topwords %in% naturalconceptiontopics ~ "naturalconceptiontopics", TRUE ~ Broadergroup))




broadergoruplevels=c(
  "periodtrackertopics",
  "conceptiontopics",
  "naturalconceptiontopics",
  "understandbodytopics",
  "easeoflifetopics",
  "communitytopcis")

test$Broadergroup <- factor(test$Broadergroup, levels=c(rev(broadergoruplevels)))

# colorsstandard <- c("#13124e", # periodtrackertopics / Menstruation prediction
#                     "#3f458d", # conceptiontopics / Birth control/Contraception
#                     "#9c9bdb", # understandbodytopics / Understanding body
#                     "#f49600", # naturalcontraception/ Contraception
#                     #"#afa10d", # / Health
#                     #"#20581c", # / Medication
#                     #"#b28573", # / Empower
#                     "#182931", # easeoflifetopics
#                     "#dad3de" # community
# )

colorsstandard <- c("#ff6961","#ffb340", "#ffd426",  "#30DB5B",# "#66d4cf", "#64d2ff", "#409cff"
                    "#7d7aff", "#da8fff")#, "#ac8e68")



ggplot(test,aes(x=language,y=value, fill=Broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    values = c(rev(colorsstandard))
                    ) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$language)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$language)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_languages.pdf"), width = 14, height = 10)




#### 30.3. Legend ####
forthelegend <- test

broadergoruplevels <- c(
  "periodtrackertopics",
  "conceptiontopics",
  "naturalconceptiontopics",
  "understandbodytopics",
  "easeoflifetopics",
  "communitytopcis")

forthelegend$broadergroup <- factor(forthelegend$Broadergroup, levels=c(broadergoruplevels))



ggplot(forthelegend,aes(x=language,y=value, fill=broadergroup)) +
  geom_bar(stat="identity",  color = "white") +
  scale_fill_manual(name = "Broader Group",
                    labels=c("Menstruation prediction", "Conception", "Birth control/Contraception", "Understanding your your body",
                             "Ease of life",  "Community"),
                    values = c(colorsstandard )) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  #scale_x_discrete(limits = paste0(rev(levels(test$App)), unique(test$Freq),")"), sep="\n"))) +
  #scale_x_discrete(labels=paste(rev(levels(test4$App)))) + #,paste0("(",unique(test$Freq),")"),sep="\n")) +
  labs(x = "", y= "Proportion of topic") +
  theme(title = element_text(family = "sans"),
        legend.position = "top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank())
ggsave(paste0("04_figs/01_Figures_Paper/0302_languages_legend_only.pdf"), width = 17, height = 10)


library(tidyverse)
library(zoo) # for working with dates
library(stringi) # for changing strings
library(readr) # this always writes in UTF-8

# 1. Load language detection with all web scraped information ####
langdetection <-read.csv("03_data/01_raw/fertility-16012022-lang-detection.csv", encoding = "UTF-8")
scriptfile <-  "01_language_filtering" # for saving data later on
datafolder <- "03_data/02_output/"
figuresfolder <- "04_figs/"
datasource <- "reviews"


# 2. Rename and remove apps we do not study ####
langdetection <- langdetection %>%
  rename(rating = score) %>%
  rename(review = text) %>%
  filter(app != "com.facebook.katana") %>%
  filter(app != "fb") %>%
  filter(app != "com.instagram.android") %>%
  filter(app != "instagram") %>%
  filter(!app %in% c("com.period.calendar.ovulation.tracker.lunar", "ovulationcalculator.com.ovulationcalculator", "premom.eh.com.ehpremomapp")) %>% # these are google codes
  filter(!app %in% c("lunar-period-tracker", "ovulation-calculator-fertile-tracker-calendar-oc", "ovulation-tracker-by-premom")) %>% # google codes
  filter(is_reliable == "True") %>%
  filter(probability >= 0.85) # keep only text with more than 85% likelihood of belonging to a language

length(langdetection$app) # 1,640,764 reviews

## 2.1. quick cleanup ####
langdetection <- langdetection %>%
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


# 3. Remove reviews with less than 4 words ####
langdetection$wordcount <- str_count(langdetection$review, "\\S+") # calculate word count
langdetection <- langdetection %>%
  filter(wordcount > 6) %>% # filter by wordcount
  select(-wordcount)
length(langdetection$app)

# 4. Remove duplicates, triplicates etc.####
langdetection <- langdetection %>%
  distinct(review, date, .keep_all = T) %>% # this is hour, minute second - so we are confident they are not unique reviews
  select(app, review, language, rating, date) # these are the only 3 pieces of information we are interested in here
length(langdetection$review)


## 4.1. Clean up dates ####
langdetection$date <- substr(langdetection$date,1,10-3)
langdetection$date <- as.Date(paste(langdetection$date,"-01",sep=""))


# 5. Find languages to keep ####
langdetection$language <- substr(langdetection$language,start=1,stop=2) # simplifies categorisation slightly
languagespresent <- as.data.frame(table(langdetection$language))
languagespresent <- languagespresent %>%
  mutate(totalsum = sum(Freq)) %>%
  mutate(proportion = (Freq/totalsum)*100) # find the proportions of languages unfiltered




### languages with less than 100 posts
# languages available in languagal training model - https://www.sbert.net/docs/pretrained_models.html
traininglangs <- c("ar", "bg", "ca", "cs", "da", "de", "el", "es", "et", "en",
                             "fa", "fi", "fr", "fr-ca", "gl", "gu", "he", "hi", "hr",
                             "hu", "hy", "id", "it", "ja", "ka", "ko", "ku", "lt",
                             "lv", "mk", "mn", "mr", "ms", "my", "nb", "nl", "pl",
                             "pt", "pt", "pt-br", "ro", "ru", "sk", "sl", "sq",
                             "sr", "sv", "th", "tr", "uk", "ur", "vi", "zh")

languagespresent <- languagespresent %>%
  filter(Var1 %in% c(traininglangs)) %>%
  mutate(totalsum = sum(Freq)) %>%
  mutate(proportion = (Freq/totalsum)*100) # find the proportions of languages filtered


languagestokeep <- as.character(unique(languagespresent$Var1)) # 45 languages remain
langdetection <- langdetection %>%
  filter(language %in% c(languagestokeep))
length(langdetection$app)
unique(langdetection$language)

## 5.1. Renaming languages ####
langdetection <- langdetection %>%
  mutate(language = case_when(language == "en" ~ "English", TRUE ~ language),
         language = case_when(language == "pt" ~ "Portuguese", TRUE ~ language),
         language = case_when(language == "es" ~ "Spanish", TRUE ~ language),
         language = case_when(language == "ru" ~ "Russian", TRUE ~ language),
         language = case_when(language == "fr" ~ "French", TRUE ~ language),
         language = case_when(language == "de" ~ "German", TRUE ~ language),
         language = case_when(language == "it" ~ "Italian", TRUE ~ language),
         language = case_when(language == "ko" ~ "Korean", TRUE ~ language),
         language = case_when(language == "pl" ~ "Polish", TRUE ~ language),
         language = case_when(language == "sv" ~ "Swedish", TRUE ~ language),
         language = case_when(language == "tr" ~ "Turkish", TRUE ~ language),
         language = case_when(language == "vi" ~ "Vietnamese", TRUE ~ language),
         language = case_when(language == "ar" ~ "Arabic", TRUE ~ language),
         language = case_when(language == "nl" ~ "Dutch", TRUE ~ language),
         language = case_when(language == "id" ~ "Indonesian", TRUE ~ language),
         language = case_when(language == "iw" ~ "Hebrew", TRUE ~ language),
         language = case_when(language == "da" ~ "Danish", TRUE ~ language))


# save these 3 columns
write_excel_csv(langdetection, paste0(datafolder, scriptfile, "_reliable_langdetection.csv")) # This is what we will use in the analysis

test <- read_csv(paste0(datafolder, scriptfile, "_reliable_langdetection.csv"))
length(test$app)


# 6. Figures ####
## 6.1. Apps date ####
datasource <- "reviews"
appdistributions <- langdetection
appdistributions %>%
  ggplot() +
  geom_histogram(aes(date), bins = length(unique(appdistributions$date))) +
  facet_wrap(~app, ncol = 4) +
  labs(x = "Date (by month)", y = "Count of reviews per month") +
  theme(title = element_text(family = "sans"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"))
ggsave(paste0(figuresfolder, scriptfile, datasource, "_apps_dates.pdf"), width = 10, height = 11)


## 6.2. Languages date ####
langdistributions <- langdetection
langdistributions %>%
  # filter(language %in% c("Portuguese", "Spanish", "Russian", "French",
  #                        "German", "Italian", "Korean", "Polish", "Swedish",
  #                        "Turkish", "Vietnamese", "Arabic", "Dutch", "Indonesian")) %>%
  filter(!language %in% c("English")) %>% # because english is too dominant for comparative graphs like this
  ggplot() +
  geom_histogram(aes(date), bins = length(unique(langdistributions$date))) + # because we have 146 months
  facet_wrap(~language, ncol = 5) +
  labs(x = "Date (by month)", y = "Count of reviews per month") +
  theme(title = element_text(family = "sans"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"))
ggsave(paste0(figuresfolder, scriptfile,datasource,"_apps_languages.pdf"), width = 10, height = 11)


## 6.3. Saving number of reviews per month ####
monthlyappreviews <- appdistributions
monthlyappreviews <- monthlyappreviews %>% count(date) %>%
  rename(reviewspermonth = n)

scriptfile <-  "0100_"
write_csv(monthlyappreviews, paste0("03_data/02_output/", scriptfile, datasource, "_monthlyappreviews.csv"))

## 6.4 Top languages ####
mytoplanguages <- table(langdetection$language)
mytoplanguages <- as.data.frame(mytoplanguages)
write_csv(mytoplanguages, "toplanguages.csv")

## 6.5. Exploring ratings ####
library(ggrepel)
colorsstandard <- c("#13124e", #
                    "#3f458d", #
                    "#9c9bdb", #
                    "#f49600", #
                    "#afa10d", #
                    "#20581c", #
                    "#b28573", #
                    "#182931", #
                    "#210506")

langdetection %>%
  filter(app %in% c("Tinder",
                    "Plenty of Fish",
                    "Her",
                    "Grindr",
                    "Happn",
                    "OkCupid")) %>%
  group_by(app, date) %>%
  summarise(rating = mean(rating)) %>%
  mutate(label = case_when(date == "2021-08-01" ~ app, TRUE ~ NA_character_)) %>%
  ggplot(aes(x = date, y = rating)) +
  geom_smooth(aes(color = app), se = FALSE) +
  geom_label_repel(aes(label = label, fill = app), color = 'white', segment.colour="black",
                   na.rm = TRUE,
                   box.padding = 0,
                   label.size = 0,
                   size = 3) +
  scale_color_manual(values = colorsstandard) +
  scale_fill_manual(values = colorsstandard) +
  labs(x = "Date (by month)", y = "Average monthly rating") +
  theme(title = element_text(family = "sans"),
        legend.position = "none",
        legend.title=element_text(size=16),
        legend.text=element_text(size=15),
        axis.text.x = element_text(size = 15, family = "sans"), # angle = 45, vjust = 0, hjust = 0, family = "sans"),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"))
ggsave(paste0(figuresfolder, scriptfile, datasource, "_ratings_averages.pdf"), width = 10, height = 11)



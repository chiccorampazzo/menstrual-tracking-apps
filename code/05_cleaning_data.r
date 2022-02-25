library(tidyverse)
library(zoo) # for working with dates
library(stringi) # for changing strings
library(readr) # this always writes in UTF-8

#### 1. Load language detection with all web scraped information ####
langdetection <-read.csv("03_data/01_raw/fertility-16012022-lang-detection.csv", encoding = "UTF-8")

#### 2. Rename and remove apps we do not study ####
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

#### 3. Remove reviews with less than 4 words ####
langdetection$wordcount <- str_count(langdetection$review, "\\S+") # calculate word count
langdetection <- langdetection %>%
  filter(wordcount > 3) %>% # filter by wordcount
  select(-wordcount)
length(langdetection$app) # 1,325,549 reviews

#### 4. Remove duplicates, triplicates etc.####
langdetection <- langdetection %>%
  distinct(review, date, .keep_all = T) %>% # this is hour, minute second - so we are confident they are not unique reviews
  select(app, review, language) # these are the only 3 pieces of information we are interested in here
length(langdetection$review) # 580,233

#### 5. Find languages to keep ####
langdetection_app_descriptions$language <- substr(langdetection_app_descriptions$language,start=1,stop=2) # simplifies categorisation slightly
languagespresent <- as.data.frame(table(langdetection_app_descriptions$language))
languagespresent <- languagespresent %>%
  mutate(totalsum = sum(Freq)) %>%
  mutate(proportion = (Freq/totalsum)*100) # find the proportions of languages unfiltered

### languages with less than 100 posts
languagespresent <- languagespresent %>%
  filter(Freq > 100) %>%
  filter(!Var1 %in% c("gl", "la", "fy", "eo")) %>%
  mutate(totalsum = sum(Freq)) %>%
  mutate(proportion = (Freq/totalsum)*100) # find the proportions of languages filtered

### languages with more than 100 words but not correctly identified
# gl # galician
# la # latin
# fy # frisian
# eo # esperanto

languagestokeep <- as.character(unique(languagespresent$Var1)) # 45 languages remain
langdetection <- langdetection %>%
  filter(language %in% c(languagestokeep))
length(langdetection$app) # 574,947 final reviews - non-duplicates, reliable language identification

# save these 3 columns
write_excel_csv(langdetection, "03_data/02_output/0100_reliable_langdetection.csv")


#### 6. App descriptions
## loading the app descriptions previously scraped and cleaned
app_descriptions <- read.csv("03_data/02_output/01_app_descriptions.csv", encoding = "UTF-8")

app_descriptions$review <- sapply(app_descriptions$review, function(row) iconv(row, "latin1", "ASCII", sub=""))

# need to remove some descriptions I don't want anymore as of 27/01/2022
app_descriptions <- app_descriptions %>%
  filter(!app %in% c("Description_Lunar", "Description_Premom", "Description_Ovulation calculator"))

write_excel_csv(app_descriptions, "03_data/02_output/0100_app_descriptions.csv")

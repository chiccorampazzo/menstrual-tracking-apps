# Spanish ####
library(tidyverse)
library(data.table)
library(gdata)

numoftopics <- 19
numoftopicsstarting0 <- as.character(numoftopics-1)

sourcescript <- "02_Spanish"
language <- "Spanish"
script <-  "03_import_and_clean"
datafolder <- "03_data/02_output/"
figuresfolder <- "04_figs/"
datasource <- "reviews"



# 0. Load topics ####
# Based on the top 10 words of each topic, group into broader topics for figures
mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')#, mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")
mytopics$broadergroup <- mytopics$topwords # We need an empty column

write_csv(mytopics, paste0("03_data/02_output/", script, "_", numoftopics, "_", datasource, "_", "mytopic_lists_3_words.csv"))

## 0.1. Load output ####
english <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_", language,  ".csv"), header=TRUE)
nonenglish <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_non_", language, ".csv"), header=TRUE)
mytopicdocumentmatrix <- rbind(english, nonenglish)

write_csv(mytopicdocumentmatrix, paste0("03_data/02_output/", script, "_", numoftopics, "_", datasource, "_", "English_and_NonEnglish.csv"))

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, newcomparableid, date, language, rating, text, `0`:numoftopicsstarting0) %>%
  rename(app = id) %>%
  rename(dates = rating) %>%
  rename(rating = date) %>%
  rename(date = dates)


## 0.2. Check the frequency of reviews per app and language ####
numberofapps <- as.data.frame(table(mytopicdocumentmatrix$app)) # checking the frequency of reviews - also useful when removing

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  mutate(language = case_when(language == "af" ~ "Afrikaans", TRUE ~ language),
         language = case_when(language == "bg" ~ "Bulgarian", TRUE ~ language),
         language = case_when(language == "ca" ~ "Catalan", TRUE ~ language),
         language = case_when(language == "cs" ~ "Czech", TRUE ~ language),
         language = case_when(language == "el" ~ "Greek", TRUE ~ language),
         language = case_when(language == "fa" ~ "Farsi", TRUE ~ language),
         language = case_when(language == "fi" ~ "Finnish", TRUE ~ language),
         language = case_when(language == "hu" ~ "Hungarian", TRUE ~ language),
         language = case_when(language == "lt" ~ "Lithuanian", TRUE ~ language),
         language = case_when(language == "af" ~ "Afrikaans", TRUE ~ language),
         language = case_when(language == "ml" ~ "Malayalam", TRUE ~ language),
         language = case_when(language == "ms" ~ "Malay", TRUE ~ language),
         language = case_when(language == "no" ~ "Norwegian", TRUE ~ language),
         language = case_when(language == "ro" ~ "Romanian", TRUE ~ language),
         language = case_when(language == "sk" ~ "Slovak", TRUE ~ language),
         language = case_when(language == "lt" ~ "Lithuanian", TRUE ~ language),
         language = case_when(language == "th" ~ "Thai", TRUE ~ language),
         language = case_when(language == "uk" ~ "Ukrainian", TRUE ~ language))

## 0.3. Rename the columns ####
basecolumns <- names(mytopicdocumentmatrix)
basecolumns <- basecolumns[0:6] # we call it top words as this is easier for binding
topics_char <- mytopics$topwords

correctnames <- rbindlist(list(list(basecolumns), list(topics_char)))
names(mytopicdocumentmatrix) <- correctnames$V1

alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`codigo, espanol, publicidad`:`deja, entrar, actualizacion`, names_to = "topic", values_to = "topicvalue") %>% # would need to change
  select(topic, topicvalue, text, everything(), -newcomparableid)

# get examples
topexamples <- alllanguages %>%
  group_by(topic) %>%
  filter(topicvalue == max(topicvalue))

top_3_examplesexamples <- alllanguages %>%
  arrange(desc(topicvalue)) %>%
  group_by(topic) %>%
  slice(1:3)


write_csv(topexamples, paste0(datafolder, script, "_", language, "_",numoftopics,  "_top_example.csv"))
write_csv(top_3_examplesexamples, paste0(datafolder, script, "_", language,"_",numoftopics,  "_top_3_examples.csv"))



## 0.4. Importing chosen broad groups ####
completedata <- alllanguages
head(completedata, 2)

length(unique(completedata$text))


## Individual topics ####
mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE) # faster than read.csv#
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2', ", ", mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5', ", ",
                            mytopics$'6', ", ", mytopics$'7', ", ", mytopics$'8', ", ", mytopics$'9')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")
mytopics$broadergroup <- mytopics$topwords # we need an empty column



mytopicdocumentmatrix <- rbind(english, nonenglish)

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, newcomparableid, date, language, rating, text, `0`:numoftopicsstarting0) %>%
  rename(app = id) %>%
  rename(dates = rating) %>%
  rename(rating = date) %>%
  rename(date = dates)



basecolumns <- names(mytopicdocumentmatrix)
basecolumns <- basecolumns[0:6]
topics_char <- mytopics$topwords

correctnames <- rbindlist(list(list(basecolumns), list(topics_char)))
names(mytopicdocumentmatrix) <- correctnames$V1

mynames <- names(mytopicdocumentmatrix)
head(mynames, 7)
alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`codigo, espanol, publicidad, ingles, referencia, entiendo, puntos, idioma, alguien, doy`:`deja, entrar, actualizacion, abrir, ultima, cierra, error, puedo, abre, sale`, names_to = "topic", values_to = "topicvalue") %>%
  select(topic, topicvalue, text, everything(), -newcomparableid)


write_csv(mytopics, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_top_10_words.csv"))

head(alllanguages, 2)

topicsums <- alllanguages %>%
  group_by(topic) %>%
  summarise(prevalence = sum(topicvalue)) %>%
  distinct()

topicsums <- topicsums %>%
  arrange(topic) %>%
  ungroup() %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal, -prevalence)

write_csv(topicsums, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_topicsums_sup.csv"))




# Portuguese ####
library(tidyverse)
library(data.table)
library(gdata)

numoftopics <- 19
numoftopicsstarting0 <- as.character(numoftopics-1)

sourcescript <- "02_Portuguese"
language <- "Portuguese"
script <-  "03_import_and_clean"
datafolder <- "03_data/02_output/"
figuresfolder <- "04_figs/"
datasource <- "reviews"



# 0. Load topics ####
# Based on the top 10 words of each topic, group into broader topics for figures
mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE) # faster than read.csv#
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')#, mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")
mytopics$broadergroup <- mytopics$topwords

write_csv(mytopics, paste0("03_data/02_output/", script, "_", numoftopics, "_", datasource, "_", "mytopic_lists_3_words.csv"))

## 0.1. Load output ####
english <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_", language,  ".csv"), header=TRUE)
nonenglish <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_non_", language, ".csv"), header=TRUE)
mytopicdocumentmatrix <- rbind(english, nonenglish)

write_csv(mytopicdocumentmatrix, paste0("03_data/02_output/", script, "_", numoftopics, "_", datasource, "_", "English_and_NonEnglish.csv"))

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, newcomparableid, date, language, rating, text, `0`:numoftopicsstarting0) %>%
  rename(app = id) %>%
  rename(dates = rating) %>%
  rename(rating = date) %>%
  rename(date = dates)


## 0.2. Check the frequency of reviews per app and language ####
numberofapps <- as.data.frame(table(mytopicdocumentmatrix$app)) # checking the frequency of reviews

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  mutate(language = case_when(language == "af" ~ "Afrikaans", TRUE ~ language),
         language = case_when(language == "bg" ~ "Bulgarian", TRUE ~ language),
         language = case_when(language == "ca" ~ "Catalan", TRUE ~ language),
         language = case_when(language == "cs" ~ "Czech", TRUE ~ language),
         language = case_when(language == "el" ~ "Greek", TRUE ~ language),
         language = case_when(language == "fa" ~ "Farsi", TRUE ~ language),
         language = case_when(language == "fi" ~ "Finnish", TRUE ~ language),
         language = case_when(language == "hu" ~ "Hungarian", TRUE ~ language),
         language = case_when(language == "lt" ~ "Lithuanian", TRUE ~ language),
         language = case_when(language == "af" ~ "Afrikaans", TRUE ~ language),
         language = case_when(language == "ml" ~ "Malayalam", TRUE ~ language),
         language = case_when(language == "ms" ~ "Malay", TRUE ~ language),
         language = case_when(language == "no" ~ "Norwegian", TRUE ~ language),
         language = case_when(language == "ro" ~ "Romanian", TRUE ~ language),
         language = case_when(language == "sk" ~ "Slovak", TRUE ~ language),
         language = case_when(language == "lt" ~ "Lithuanian", TRUE ~ language),
         language = case_when(language == "th" ~ "Thai", TRUE ~ language),
         language = case_when(language == "uk" ~ "Ukrainian", TRUE ~ language))

numberoflangs <- as.data.frame(table(mytopicdocumentmatrix$language)) # checking the frequency of reviews


## 0.3. Rename the columns ####
basecolumns <- names(mytopicdocumentmatrix)
basecolumns <- basecolumns[0:6]
topics_char <- mytopics$topwords

correctnames <- rbindlist(list(list(basecolumns), list(topics_char)))
names(mytopicdocumentmatrix) <- correctnames$V1

alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`portugues, codigo, faco`:`ajudando, ajudado, ajudou`, names_to = "topic", values_to = "topicvalue") %>%
  select(topic, topicvalue, text, everything(), -newcomparableid)

# get examples
topexamples <- alllanguages %>%
  group_by(topic) %>%
  filter(topicvalue == max(topicvalue))

top_3_examplesexamples <- alllanguages %>%
  arrange(desc(topicvalue)) %>%
  group_by(topic) %>%
  slice(1:3)


write_csv(topexamples, paste0(datafolder, script, "_", language, "_",numoftopics,  "_top_example.csv"))
write_csv(top_3_examplesexamples, paste0(datafolder, script, "_", language,"_",numoftopics,  "_top_3_examples.csv"))


completedata <- alllanguages
head(completedata, 2)

length(unique(completedata$text))





## Individual topics ####
mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2', ", ", mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5', ", ",
                            mytopics$'6', ", ", mytopics$'7', ", ", mytopics$'8', ", ", mytopics$'9')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")
mytopics$broadergroup <- mytopics$topwords

mytopicdocumentmatrix <- rbind(english, nonenglish)

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, newcomparableid, date, language, rating, text, `0`:numoftopicsstarting0) %>%
  rename(app = id) %>%
  rename(dates = rating) %>%
  rename(rating = date) %>%
  rename(date = dates)



basecolumns <- names(mytopicdocumentmatrix)
basecolumns <- basecolumns[0:6]
topics_char <- mytopics$topwords

correctnames <- rbindlist(list(list(basecolumns), list(topics_char)))
names(mytopicdocumentmatrix) <- correctnames$V1

mynames <- names(mytopicdocumentmatrix)
mynames
alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`portugues, codigo, faco, ingles, quero, alguem, assinatura, mudar, apos, janela`:
                 `ajudando, ajudado, ajudou, organizar, auxilia, esquecida, ajudar, bastante, principalmente, organizacao`, names_to = "topic", values_to = "topicvalue") %>%
  select(topic, topicvalue, text, everything(), -newcomparableid)


write_csv(mytopics, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_top_10_words.csv"))


head(alllanguages, 2)

topicsums <- alllanguages %>%
  group_by(topic) %>%
  summarise(prevalence = sum(topicvalue)) %>%
  distinct()

topicsums <- topicsums %>%
  arrange(topic) %>%
  ungroup() %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal, -prevalence)

write_csv(topicsums, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_topicsums_sup.csv"))

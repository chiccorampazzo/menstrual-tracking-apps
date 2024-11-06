library(tidyverse)
library(data.table)
library(gdata)

numoftopics <- 19
numoftopicsstarting0 <- as.character(numoftopics-1)

sourcescript <- "02_English"
language <- "English"
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
mytopics$broadergroup <- mytopics$topwords
mytopics <- mytopics %>% mutate(`5` = case_when(`5` = 'eve' ~ 'one', TRUE ~ `5`))

## 0.1. Load output ####
english <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_",  "English.csv"), header=TRUE)
nonenglish <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_non_English.csv"), header=TRUE)
mytopicdocumentmatrix <- rbind(english, nonenglish)

mytopicdocumentmatrix <- mytopicdocumentmatrix %>%
  select(id, newcomparableid, date, language, rating, text, `0`:numoftopicsstarting0) %>%
  rename(app = id) %>%
  rename(dates = rating) %>%
  rename(rating = date) %>%
  rename(date = dates)


## 0.2. Check the frequency of reviews per app and language ####
numberofapps <- as.data.frame(table(mytopicdocumentmatrix$app))

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

numberoflangs <- as.data.frame(table(mytopicdocumentmatrix$language))




## 0.3. Rename the columns ####
basecolumns <- names(mytopicdocumentmatrix)
basecolumns <- basecolumns[0:6]
topics_char <- mytopics$topwords

correctnames <- rbindlist(list(list(basecolumns), list(topics_char)))
names(mytopicdocumentmatrix) <- correctnames$V1

alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`control, birth, body`:`start, date, late`, names_to = "topic", values_to = "topicvalue") %>%
  select(topic, topicvalue, text, everything(), -newcomparableid)


## 0.4. Importing chosen broad groups ####
# Identify broader groups by reading random samples of the reviews and their calculated topic probability, do this separately in a csv file
broadertopics <- read_csv(paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_top_example_edited.csv"))
broadertopics <- broadertopics %>% select(broadergroup, topic)

alllanguages <- full_join(alllanguages, broadertopics, by = "topic")
head(alllanguages, 2)


alllanguages <- alllanguages %>%
  mutate(broadergroup = case_when(
    str_detect(topic, 'add, would, symptoms') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'keeping, keeps, stay') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'period, recommend, periods') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'point, spot, accuracy') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'start, date, late') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'symptoms, cycle, cycles') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'years, using, year') ~ 'Menstruation tracker (symptoms & track & prediction)',
    TRUE ~ broadergroup
  ))

## 0.5. Identify best matches ####
unique(alllanguages$broadergroup)
alllanguages <- alllanguages %>%
  mutate(broadergroup = case_when(broadergroup == "Birth control/Contraception" ~
                                    'Avoid pregnancy', TRUE ~ broadergroup),
         broadergroup = case_when(broadergroup == "Conception" ~
                                    'Achieve pregnancy', TRUE ~ broadergroup))
birthcontrol <- alllanguages %>%
  filter(broadergroup == "Avoid pregnancy") %>%
  group_by(topic) %>%
  arrange(desc(topicvalue)) %>%
  slice_max(order_by = topicvalue, n = 10) %>%
  ungroup()
unique(birthcontrol$topic)

community <- alllanguages %>%
  filter(broadergroup == "Community/Education") %>%
  group_by(topic) %>%
  arrange(desc(topicvalue)) %>%
  slice_max(order_by = topicvalue, n = 10) %>%
  ungroup()
unique(community$topic)

conception <- alllanguages %>%
  filter(broadergroup == "Achieve pregnancy") %>%
  group_by(topic) %>%
  arrange(desc(topicvalue)) %>%
  slice_max(order_by = topicvalue, n = 10) %>%
  ungroup()
unique(conception$topic)

birthcontrol <- rbind(birthcontrol, menstruation)
birthcontrol <- rbind(birthcontrol, community)
birthcontrol <- rbind(birthcontrol, conception)


## Check groupings ####
unique(alllanguages$broadergroup)
alllanguages_reviews <- alllanguages %>%
  mutate(broadergroup = case_when(broadergroup == "Track menstruation (track & prediction)" ~
                                    'Menstruation tracker (track & prediction)', TRUE ~ broadergroup)) %>%

  group_by(broadergroup) %>%
  summarise(prevalence = sum(topicvalue))

alllanguages_reviews_byapp <- alllanguages %>%
  group_by(broadergroup, app) %>%
  summarise(prevalence = sum(topicvalue))

alllanguages_reviews_bylangauge <- alllanguages %>%
  group_by(broadergroup, language) %>%
  summarise(prevalence = sum(topicvalue))



# 1. Individual topics ####
alllanguages <- full_join(alllanguages, broadertopics, by = "topic")
alllanguages <- alllanguages %>%
  mutate(broadergroup = case_when(broadergroup == "Birth control/Contraception" ~
                                    'Avoid pregnancy', TRUE ~ broadergroup),
         broadergroup = case_when(broadergroup == "Conception" ~
                                    'Achieve pregnancy', TRUE ~ broadergroup)) %>%
  mutate(broadergroup = case_when(
    str_detect(topic, 'add, would, symptoms') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'keeping, keeps, stay') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'period, recommend, periods') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'point, spot, accuracy') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'start, date, late') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'symptoms, cycle, cycles') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'years, using, year') ~ 'Menstruation tracker (symptoms & track & prediction)',
    TRUE ~ broadergroup
  ))
unique(alllanguages$topic)

alllanguages_reviews_topics <- alllanguages %>%
  group_by(topic, broadergroup) %>%
  summarise(prevalence = sum(topicvalue))

# 2. Figures ####
## 2.1. Total ####
mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE)
mytopics <- mytopics %>% mutate(`5` = case_when(`5` == 'eve' ~ 'one', TRUE ~ `5`))

mytopics$topwords_new <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2',", ", mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5',", ",
                            mytopics$'6', ", ", mytopics$'7', ", ", mytopics$'8',", ", mytopics$'9')
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2')

mytopics <- mytopics %>% select(topwords, topwords_new)
mytopics <- rowid_to_column(mytopics, "ID")

mytopics <- mytopics %>% rename(topic = topwords)
alllanguages_reviews_topics <- full_join(alllanguages_reviews_topics, mytopics, by = 'topic')

alllanguages_reviews_topics <- alllanguages_reviews_topics %>% ungroup()
alllanguages_reviews_topics <- alllanguages_reviews_topics %>% select(broadergroup, prevalence, topwords_new) %>%
  rename(topic = topwords_new)
alllanguages_reviews_topics

tot_des <- alllanguages_reviews_topics %>% mutate(source="Individual topics \n with the ten most common words within that topic")
tot_res <- alllanguages_reviews %>% mutate(source="Topics grouped into \n reasons for use")
tot_res <- tot_res %>% mutate(topic = '')
tot <- rbind(tot_des, tot_res)
tot <- tot %>% filter(broadergroup != "User experience") %>%
  group_by(source) %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  ungroup() %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal)
sum(tot$value)

broadergoruplevels <- c("Menstruation tracker", "Achieve pregnancy", "Community/Education", "Avoid pregnancy",
                        "Health", "Reliability/Security", "Empowerment")

broadergoruplevels <- c("Menstruation tracker (symptoms & track & prediction)",
  'Menstruation tracker (track & prediction)',
  "Health", "Achieve pregnancy",
  "Reliability/Security", "Community/Education", "Empowerment",
  "Avoid pregnancy")
broadergoruplevels <- c("Menstruation tracker (symptoms & track & prediction)",
                        'Menstruation tracker (track & prediction)',"Achieve pregnancy", "Community/Education",
                        "Avoid pregnancy")
tot$broadergroup <- ordered(tot$broadergroup, levels=c((broadergoruplevels)))
levels(tot$broadergroup)
legend_ord <- levels(tot$broadergroup)
legend_ord <- c("Menstruation tracker (symptoms & track & prediction)",
                'Menstruation tracker (track & prediction)',
                "Health", "Achieve pregnancy",
                "Reliability/Security", "Community/Education",
                "Avoid pregnancy")
legend_ord <- c("Menstruation tracker (symptoms & track & prediction)",
                        'Menstruation tracker (track & prediction)',"Achieve pregnancy", "Community/Education",
                        "Avoid pregnancy")

levels(tot$source)
levels(tot$broadergroup)

tot$source <- ordered(tot$source, levels=c('Individual topics \n with the ten most common words within that topic', 'Topics grouped into \n reasons for use'))

library(viridis)

values <- viridis(10)
values <- c('#fde724', '#5cc862', '#A3C1E1', '#20908c')

values <- c('#fde724', '#fde724', '#5cc862', '#31688d', '#20908c')

print(tot)

legend_ord <- c("Menstruation tracker (symptoms & track & prediction)",
                'Menstruation tracker (track & prediction)',
                "Achieve pregnancy",  "Community/Education", "Avoid pregnancy")

# Function to reformat legend text
reformat_legend_text <- function(text) {
  str_replace(text, "\\((.*?)\\)", "\n(\\1)")
}

# Reformat the legend_ord text
legend_ord <- c("Menstruation tracker (symptoms & track & prediction)",
                'Menstruation tracker (track & prediction)',
                "Achieve pregnancy",  "Community/Education", "Avoid pregnancy") %>%
  sapply(reformat_legend_text)


# 3. Cleaner approach ####
library(dplyr)
library(stringr)
library(ggplot2)

# Update broadergroup factor levels to include newline characters
tot_original <- tot
tot <- tot_original %>%
  mutate(broadergroup = case_when(
    str_detect(topic, 'add, would, symptoms') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'keeping, keeps, stay') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'period, recommend, periods') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'point, spot, accuracy') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'start, date, late') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'symptoms, cycle, cycles') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'years, using, year') ~ 'Menstruation tracker (symptoms & track & prediction)',
    TRUE ~ broadergroup
  ))


tot$broadergroup <- as.character(tot$broadergroup)
unqique()
tot_2 <- tot %>%
  mutate(broadergroup = factor(broadergroup, levels = c(
    "Menstruation tracker (symptoms & track & prediction)",
    "Menstruation tracker (track & prediction)",
    "Achieve pregnancy",
    "Community/Education",
    "Avoid pregnancy"
  )))

# Custom function to split text after the fifth word
split_text <- function(text) {
  words <- unlist(str_split(text, ", "))
  if (length(words) > 5) {
    first_part <- paste(words[1:5], collapse = ", ")
    second_part <- paste(words[6:length(words)], collapse = ", ")
    paste(first_part, "\n", second_part, sep = "")
  } else {
    text
  }
}

tot <- tot %>%
  mutate(topic_split = sapply(topic, split_text))
sum(tot_x$value)

values <- c('#FDE724',  '#FFEA66', '#5cc862', '#31688d', '#20908c')


legend_ord <- c(
  "Menstruation tracker \n(symptoms & track & prediction)",
  "Menstruation tracker \n(track & prediction)",
  "Achieve pregnancy",
  "Community/Education",
  "Avoid pregnancy"
)

legend_ord <- c(
  "Menstruation tracker (symptoms & track & prediction)",
  "Menstruation tracker (track & prediction)",
  "Achieve pregnancy",
  "Community/Education",
  "Avoid pregnancy"
)


x <- read_csv('all_reviews.csv')
summary(x$datetime)

tot <- tot %>%
  mutate(broadergroup = case_when(broadergroup == 'Menstruation tracker (symptoms & track & prediction)' ~
                                    'Menstruation tracker \n(symptoms & track & prediction)', T ~ broadergroup),
         broadergroup = case_when(broadergroup == 'Menstruation tracker (track & prediction)' ~
                                    'Menstruation tracker \n(track & prediction)', T ~ broadergroup)
  )

tot <- tot %>%
  group_by(source) %>%
  arrange(prevalence) %>%
  mutate(broadergroup = factor(broadergroup, levels = c(
    "Menstruation tracker \n(symptoms & track & prediction)",
    "Menstruation tracker \n(track & prediction)",
    "Achieve pregnancy",
    "Community/Education",
    "Avoid pregnancy"
  )))
legend_ord <- c("Menstruation tracker \n(symptoms & track & prediction)",
                "Menstruation tracker \n(track & prediction)",
                "Achieve pregnancy",
                "Community/Education",
                "Avoid pregnancy")

values <- c('#FDE724',  '#FFC107', '#5cc862', '#6699b3', '#D4A5A5')


tot_x <- tot %>%
  mutate(value = case_when(topic_split == '' ~ round(value, 3), TRUE ~ value)) %>%
  mutate(percent = case_when(topic == '' ~ ceiling(value * 100), TRUE ~ as.numeric(NA)))

total <- sum(tot_x$percent, na.rm = TRUE)

tot_x <- tot_x %>%
  mutate(percent = ifelse(!is.na(percent), percent * 100 / total, NA))

sum(tot_x$percent, na.rm = TRUE)

tot_x <- tot_x %>%
  mutate(percent = case_when(topic == '' ~ value * 100, TRUE ~ as.numeric(NA)))
unique(tot_x$percent)

tot_final <- tot_x %>%
  mutate(percent = case_when(percent == 7.8 ~ 8, TRUE ~ percent),
         percent = case_when(percent == 9.4 ~ 9, TRUE ~ percent),
         percent = case_when(percent == 22.3 ~ 22, TRUE ~ percent),
         percent = case_when(percent == 27.1 ~ 27, TRUE ~ percent),
         percent = case_when(percent == 33.4 ~ 34, TRUE ~ percent))

tot_final %>%
  mutate(percent = case_when(topic == '' ~ paste0(percent, '%'), TRUE ~ as.character(NA))) %>%
  ggplot(aes(x = source, y = value, fill = broadergroup)) +
  geom_bar(stat = "identity", color = "white", position = "fill") +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Reason for using the app",
                    values = values,
                    guide = guide_legend(
                      title.position = 'top',
                      title.hjust = 0.5,
                      label.hjust = 0.5
                    ), breaks = legend_ord) +
  geom_text(aes(x = source, y = value, label = percent),
            colour = "black", size = 15, position = position_stack(vjust = 0.6)) +
  geom_text(aes(x = source, y = value, label = topic_split),
            colour = "black", size = 11.5, position = position_stack(vjust = 0.6), lineheight = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_text(size = 55),
        legend.text = element_text(size = 36),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 42),
        axis.text.y = element_text(size = 42),
        axis.title.y = element_text(size = 42),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 42)
  ) +
  facet_wrap(~source, scales = "free_x")

ggsave("04_figs/text_tot_figure.pdf", width = 80, height = 50, units = "cm")


# 4. Final approach ####
# Prepare data for plotting
tot_x <- tot_x %>%
  mutate(percent = case_when(topic == '' ~ trunc(value * 1000) / 10, TRUE ~ as.numeric(NA)))

tot_x <- tot_x %>%
  mutate(percent = ifelse(!is.na(percent), percent * 100 / total, NA))

tot_x <- tot_x %>%
  mutate(percent = trunc(percent * 10) / 10)

tot_x <- tot_x %>%
  mutate(adjusted_percent = trunc(adjusted_percent * 10) / 10)

test <- tot_x %>% filter(!is.na(percent))
sum(test$percent)
tot_x %>%
  mutate(percent = case_when(topic == '' ~ paste0(percent, '%'), TRUE ~ as.character(NA))) %>%
  ggplot(aes(x = source, y = value, fill = broadergroup)) +
  geom_bar(stat = "identity", color = "white", position = "fill") +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Reason for using the app",
                    values = values,
                    guide = guide_legend(
                      title.position = 'top',
                      title.hjust = 0.5,
                      label.hjust = 0.5
                    ), breaks = legend_ord) +
  geom_text(aes(x = source, y = value, label = percent),
            colour = "black", size = 15, position = position_stack(vjust = 0.6)) +
  geom_text(aes(x = source, y = value, label = topic_split),
            colour = "black", size = 11.5, position = position_stack(vjust = 0.6), lineheight = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_text(size = 55),
        legend.text = element_text(size = 36),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 42),
        axis.text.y = element_text(size = 42),
        axis.title.y = element_text(size = 42),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 42)
  ) +
  facet_wrap(~source, scales = "free_x")

ggsave("04_figs/text_tot_figure.pdf", width = 80, height = 50, units = "cm")


tot <- tot %>%
  mutate(broadergroup = case_when(
    str_detect(topic, 'add, would, symptoms') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'keeping, keeps, stay') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'period, recommend, periods') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'point, spot, accuracy') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'start, date, late') ~ 'Menstruation tracker (track & prediction)',
    str_detect(topic, 'symptoms, cycle, cycles') ~ 'Menstruation tracker (symptoms & track & prediction)',
    str_detect(topic, 'years, using, year') ~ 'Menstruation tracker (symptoms & track & prediction)',
    TRUE ~ broadergroup
  ))

tot <- tot %>%
  mutate(broadergroup = factor(broadergroup, levels = c("Menstruation tracker (symptoms & track & prediction)",
                                                        'Menstruation tracker (track & prediction)',
                                                        "Achieve pregnancy","Community/Education",  "Avoid pregnancy")))


# Plotting
# Custom function to split text after the fifth word
split_text <- function(text) {
  words <- unlist(str_split(text, ", "))
  if (length(words) > 5) {
    first_part <- paste(words[1:5], collapse = ", ")
    second_part <- paste(words[6:length(words)], collapse = ", ")
    paste(first_part, "\n", second_part, sep = "")
  } else {
    text
  }
}

tot_x <- tot %>%
  mutate(topic_split = sapply(topic, split_text))
values <- c('#FDE724', '#FDE724', '#5cc862', '#31688d', '#20908c')
unique(tot_x$broadergroup)


tot_x %>%
  mutate(percent = case_when(topic == '' ~ trunc(value * 100), TRUE ~ as.numeric(NA))) %>%
  mutate(percent = case_when(topic == '' ~ paste0(percent, '%'), TRUE ~ as.character(NA))) %>%
  ggplot(aes(x = source, y = value, fill = broadergroup)) +
  geom_bar(stat = "identity", color = "white", position = "fill") +
  xlab("") + ylab("") +
  scale_fill_manual(name = "Reason for using the app",
                    values = values,
                    guide = guide_legend(
                      title.position = 'top',
                      title.hjust = 0.5,
                      label.hjust = 0.5
                    ), breaks = legend_ord) +
  geom_text(aes(x = source, y = value, label = percent),
            colour = "black", size = 15, position = position_stack(vjust = 0.6)) +
  geom_text(aes(x = source, y = value, label = topic_split),
            colour = "black", size = 11.5, position = position_stack(vjust = 0.6), lineheight = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "top",
        legend.title = element_text(size = 55),
        legend.text = element_text(size = 45),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 45),
        axis.text.y = element_text(size = 45),
        axis.title.y = element_text(size = 45),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 45)
  ) +
  facet_wrap(~source, scales = "free_x")

ggsave("04_figs/text_tot_figure.pdf", width = 80, height = 50, units = "cm")

# 5 Supplementary ####
## 5.1 Languages - reviews ####
alllanguages_reviews_bylangauge
alllanguages_reviews_byapp
alllanguages_descriptions_byapp


tot_res <- alllanguages_reviews_bylangauge %>% mutate(source="Reviews")

tot <- tot_res
tot <- tot %>% filter(broadergroup != "User experience") %>%
  group_by(source) %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  ungroup() %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal)

# broadergoruplevels <- c("Menstruation tracker", "Achieve pregnancy", "Community/Education", "Avoid pregnancy")

broadergoruplevels <- c("Menstruation tracker", "Achieve pregnancy", "Community/Education", "Avoid pregnancy",
                        "Health", "Reliability/Security", "Empowerment")

tot$broadergroup <- ordered(tot$broadergroup, levels=c((broadergoruplevels)))
levels(tot$broadergroup)

numberoflangs <- numberoflangs %>%
  rename(language = Var1)

numberoflangsfactor <- numberoflangs %>%
  arrange(Freq)

tot <- full_join(tot, numberoflangs, by = "language")

langorder <- numberoflangsfactor$language

tot <- tot %>%
  mutate(language=factor(language,levels =langorder,
                         ordered = T))
tot %>%
  mutate(uniquelanguage = case_when(broadergroup == "Menstruation tracker"
                                    ~ as.character(language), TRUE ~ NA_character_)) %>%
  ggplot(aes(x=language, y=value, fill=(broadergroup)))+
  geom_bar(stat="identity",  color = "white", position="fill")+
  xlab("") + ylab("")+
  scale_fill_manual(name = "Reason for using the app",
                    values = rev(values),
                    guide = guide_legend(
                      title.position = 'top',
                      title.hjust = 0.5,
                      label.hjust = 0.5,
                    ), breaks=legend_ord) +
  scale_y_continuous(labels = scales::percent) +
  theme(title = element_text(family = "sans"),
        legend.position = "top",
        legend.title=element_text(size=20),
        legend.text=element_text(size=20),
        axis.text.x = element_blank(),
        axis.title.x = element_text(size = 16, family = "sans"),
        axis.text.y = element_text(size = 15, family = "sans"),
        axis.title.y = element_text(size = 16, family = "sans"),
        panel.background = element_rect(fill = "white", colour = "white"),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        strip.text.x = element_text(size = 20)) +
  coord_flip()
ggsave("04_figs/languages.pdf", width = 40, height = 28, units = "cm")


## 5.2. apps - reviews ####
tot_res <- alllanguages_reviews_byapp %>% mutate(source="Reviews")



tot <- tot_res
tot <- tot%>% filter(broadergroup != "User experience") %>%
  group_by(source) %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  ungroup() %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal)

broadergoruplevels <- c("Menstruation tracker", "Achieve pregnancy", "Community/Education", "Avoid pregnancy",
                        "Health", "Reliability/Security", "", "Empowerment")

tot$broadergroup <- ordered(tot$broadergroup, levels=c((broadergoruplevels)))
levels(tot$broadergroup)

numberofapps <- numberofapps %>%
  arrange(Freq) %>%
  rename(app = Var1)
sum(numberofapps$Freq)

tot <- full_join(tot, numberofapps, by = "app")

apporder <- numberofapps$app
tot <- tot %>%
  mutate(app=factor(app,levels =apporder,
                    ordered = T))


tot %>%
  mutate(percent=trunc(value*100)) %>%
  ggplot(aes(x=app, y=value, fill=(broadergroup)))+
  geom_bar(stat="identity",  color = "white", position="fill")+
  xlab("") + ylab("")+
  scale_fill_manual(name = "Reason for using the app",
                    values = rev(c("#7d7aff", "#66d4cf", "#30db5b", "#da8fff", "#ffd426", "#da8fff", "#da8fff")),
                    guide = guide_legend(
                      title.position = 'top',
                      title.hjust = 0.5,
                      label.hjust = 0.5,
                    ), breaks=legend_ord) +
  scale_y_continuous(labels = scales::percent) +
  theme(title = element_text(family = "sans"),
    legend.position = "top",
    legend.title=element_text(size=20),
    legend.text=element_text(size=20),
     axis.text.x = element_blank(),
     axis.title.x = element_text(size = 16, family = "sans"),
     axis.text.y = element_text(size = 15, family = "sans"),
   axis.title.y = element_text(size = 16, family = "sans"),
    panel.background = element_rect(fill = "white", colour = "white"),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    strip.text.x = element_text(size = 20)) +
  coord_flip()
ggsave("04_figs/apps_reviews.pdf", width = 40, height = 28, units = "cm")



## 5.3. Individual topic ####
library(tidyverse)
library(data.table)
library(gdata)

numoftopics <- 19
numoftopicsstarting0 <- as.character(numoftopics-1)

sourcescript <- "02_English"
language <- "English"
script <-  "03_import_and_clean"
datafolder <- "03_data/02_output/"
figuresfolder <- "04_figs/"
datasource <- "reviews"

mytopics <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource, "_", "mytopic_lists.csv"), header=TRUE)
mytopics$topwords <- paste0(mytopics$'0', ", ", mytopics$'1', ", ", mytopics$'2', ", ", mytopics$'3', ", ", mytopics$'4', ", ", mytopics$'5', ", ",
                            mytopics$'6', ", ", mytopics$'7', ", ", mytopics$'8', ", ", mytopics$'9')
mytopics <- mytopics %>% select(topwords)
mytopics <- rowid_to_column(mytopics, "ID")
mytopics$broadergroup <- mytopics$topwords


english <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_",  "English.csv"), header=TRUE)
nonenglish <- fread(paste0("03_data/02_output/", sourcescript, "_", numoftopics, "_", datasource,"_non_English.csv"), header=TRUE)

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

alllanguages <- mytopicdocumentmatrix %>% # change depending on topic
  pivot_longer(`control, birth, body, natural, pill, know, hormonal, get, contraception, cycles`:`start, date, late, next, days, always, coming, know, dates, starts`, names_to = "topic", values_to = "topicvalue") %>%
  select(topic, topicvalue, text, everything(), -newcomparableid)


write_csv(mytopics, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_top_10_words.csv"))
broadertopics <- read_csv(paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_top_10_words_edited.csv"))
broadertopics <- broadertopics %>% select(broadergroup, topic)

alllanguages <- full_join(alllanguages, broadertopics, by = "topic")
head(alllanguages, 2)

topicsums <- alllanguages %>%
  group_by(topic) %>%
  summarise(prevalence = sum(topicvalue),
            broadergroup = broadergroup) %>%
  distinct()

topicsums <- topicsums %>%
  arrange(broadergroup) %>%
  ungroup() %>%
  mutate(sourcetotal = sum(prevalence)) %>%
  mutate(value = (prevalence/sourcetotal)) %>%
  select(-sourcetotal, -prevalence)

write_csv(topicsums, paste0("03_data/03_edited_output/", script, "_", language, "_",numoftopics,  "_topicsums_sup.csv"))


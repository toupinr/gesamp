library(readxl)
library(tidyverse)
library(openxlsx)
library(ggplot2)

data<-read.xlsx("data/gesamp_news.xlsx", sheet = "Sheet 1 - gesamp_news")

data["eureka_doctype"][is.na(data["eureka_doctype"])] <- 0

data_title <- data %>%
  distinct(title, publication, .keep_all = T)

n_distinct(data_title$publication)

data_filtered <- data %>%
  distinct(title, publication, .keep_all = T) %>%
  distinct(url, .keep_all = T) %>%
  filter(grepl("GESAMP\\d", report)) %>%
  filter(eureka_doctype != "journalArticle", .preserve = T) %>%
#  filter(eureka_doctype != "magazineArticle", .preserve = T) %>%
  filter(eureka_doctype != "dataset", .preserve = T) %>%
  filter(!str_detect(publication, "Nature.com")) %>%
  filter(!str_detect(publication, "Proceedings")) %>%
  filter(!str_detect(publication, "Advances")) %>%
  filter(!str_detect(publication, "Journal of"))

data_filtered <- data_filtered %>%
  mutate(report_split = str_split(report, ";")) %>%
  unnest %>%
  mutate(report = str_trim(report_split))

data_filtered %>%
  count(report) %>%
  arrange(desc(n)) %>%
  print(n = 20)
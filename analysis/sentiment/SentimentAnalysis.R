library(tidyverse)
library(tidytext)
library(readxl)
library(here)


# load data
News <- read_excel(paste0(here(), "/data/raw_data/artiklar nytt dataset.xlsx")) %>% select(-`...6`, -`...7`) %>% 
  mutate(Date=Date %>% as.numeric() %>% as.Date(origin="1900-01-01"), Length = str_count(Content)) 

SentimentLexikon <- read.csv(paste0(here(), "/data/raw_data/sentimentlex.csv"))


# Analysis following Julia Silge https://www.tidytextmining.com/sentiment
TidyNews <- News %>% 
  unnest_tokens(word, Content)

TidyNewsSentiments <- TidyNews %>% inner_join(SentimentLexikon, by = c("word" = "word")) %>% mutate(strength = as.numeric(strength))

# TidyNewSentimentSummary_byNewspaper <- TidyNewsSentiments %>% group_by(Policy, Newspaper) %>% mutate(PolaritySum = sum(strength), Ratio = n()/Length, PolarityRatio = PolaritySum * Ratio) %>% select(Policy, Date, Newspaper, PolaritySum, PolarityRatio ) %>% unique() 

TidyNewSentimentSummary_byPolicy <- TidyNewsSentiments %>% group_by(Policy) %>% mutate(PolaritySum = sum(strength), Ratio = n()/sum(Length), PolarityRatio = PolaritySum * Ratio, Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y")) %>% select(Policy, Year, PolaritySum, PolarityRatio ) %>% group_by(Policy, PolaritySum, PolarityRatio) %>% drop_na() %>% filter (! duplicated(Policy)) # %>% drop_na() 

xlsx::write.xlsx(TidyNewSentimentSummary_byPolicy %>% as.data.frame(), paste0(here(),"/analysis/sentiment/SentimentScores.xlsx"))

library(tidyverse)
library(tidytext)
library(readxl)
library(here)


# load data
News <- read_excel(paste0(here(), "/data/raw_data/artiklar nytt dataset.xlsx")) %>% select(-`...6`, -`...7`) %>% 
  mutate(Date=Date %>% as.numeric() %>% as.Date(origin="1900-01-01"), Length = str_count(Content)) 

SentimentLexikon <- read.csv(paste0(here(), "/data/raw_data/sentimentlex.csv"))


# Analysis following Julia Silge https://www.tidytextmining.com/sentiment
TidyNews <- News %>% 
  unnest_tokens(word, Content)

TidyNewsSentiments <- TidyNews %>% inner_join(SentimentLexikon, by = c("word" = "word")) %>% mutate(strength = as.numeric(strength))

# TidyNewSentimentSummary_byNewspaper <- TidyNewsSentiments %>% group_by(Policy, Newspaper) %>% mutate(PolaritySum = sum(strength), Ratio = n()/Length, PolarityRatio = PolaritySum * Ratio) %>% select(Policy, Date, Newspaper, PolaritySum, PolarityRatio ) %>% unique() 

TidyNewSentimentSummary_byPolicy <- TidyNewsSentiments %>% group_by(Policy) %>% mutate(PolaritySum = sum(strength), Ratio = n()/sum(Length), PolarityRatio = PolaritySum * Ratio, Year = format(as.Date(Date, format="%d/%m/%Y"),"%Y")) %>% select(Policy, Year, PolaritySum, PolarityRatio ) %>% group_by(Policy, PolaritySum, PolarityRatio) %>% drop_na() %>% filter (! duplicated(Policy)) # %>% drop_na() 

xlsx::write.xlsx(TidyNewSentimentSummary_byPolicy %>% as.data.frame(), paste0(here(),"/analysis/sentiment/SentimentScores.xlsx"))

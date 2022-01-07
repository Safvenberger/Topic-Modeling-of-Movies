# Load extra functions
source("Visualizations/help_functions.R")

library(tidyverse)
library(RColorBrewer)
library(grid)
library(pBrackets) 

# BERTOpic topics per class
topic10class_unsup <- read_csv("Output/BERTopic/topic10class_unsup.csv") %>% 
  select(!X1) 
topic10class_sup <- read_csv("Output/BERTopic/topic10class_sup.csv") %>% 
  select(!X1) 
topic20class_unsup <- read_csv("Output/BERTopic/topic20class_unsup.csv") %>% 
  select(!X1) 
topic20class_sup <- read_csv("Output/BERTopic/topic20class_sup.csv") %>% 
  select(!X1) 
topic30class_unsup <- read_csv("Output/BERTopic/topic30class_unsup.csv") %>% 
  select(!X1) 
topic30class_sup <- read_csv("Output/BERTopic/topic30class_sup.csv") %>%
  select(!X1) 

# BERTopic 
topic10preds_unsup <- read_csv("Output/BERTopic/topic10preds_unsup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)
topic10preds_sup <- read_csv("Output/BERTopic/topic10preds_sup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)
topic20preds_unsup <- read_csv("Output/BERTopic/topic20preds_unsup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)
topic20preds_sup <- read_csv("Output/BERTopic/topic20preds_sup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)
topic30preds_unsup <- read_csv("Output/BERTopic/topic30preds_unsup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)
topic30preds_sup <- read_csv("Output/BERTopic/topic30preds_sup.csv") %>% 
  select(!X1) %>% rename(Topic = topics)

# Probability distributions of each documents
prob_df <- read_csv("Output/BERTopic/probability_distributions.csv") 
  
# Cleaned version of the data
df_clean <- read_csv("Data/df_clean.csv")

# List of all classes
class_list <- list(topic10class_unsup, topic10class_sup, 
                   topic20class_unsup, topic20class_sup,
                   topic30class_unsup, topic30class_sup)

# List of all predicted classes
pred_list <- list(topic10preds_unsup, topic10preds_sup, 
                  topic20preds_unsup, topic20preds_sup,
                  topic30preds_unsup, topic30preds_sup)

#prob_distribution <- top_pred_test(class_list, pred_list)
#write_csv(prob_distribution, "probability_distributions.csv")

# Mean probabilities across all models
prob_df %>% 
  mutate(rowname = as.integer(rowname)) %>% 
  # Group by each test observation
  group_by(rowname) %>% 
  # Summarise the probabilities in each class and divide by the number of models (i.e. the mean)
  summarise(across(starts_with("Class"), ~ mean(.x, na.rm = TRUE))) -> pred_dist


# Class distribution for models
class_dist(prob_df, df_clean)
ggsave("Figures/genre_distribution.svg", width = 8, height = 7)


# Heatmap of probabilities for each genre & document
pred_heatmap(pred_dist)
ggsave("Figures/genre_heatmap.png", width = 14, height = 9)


# Topic distribution within genres
facet_class(topic10class_unsup)
ggsave("Figures/topic10class_unsup.svg", width = 8, height = 7)
facet_class(topic10class_sup)
ggsave("Figures/topic10class_sup.svg", width = 8, height = 7)
facet_class(topic20class_unsup)
ggsave("Figures/topic20class_unsup.svg", width = 8, height = 7)
facet_class(topic20class_sup)
ggsave("Figures/topic20class_sup.svg", width = 8, height = 7)
facet_class(topic30class_unsup)
ggsave("Figures/topic30class_unsup.svg", width = 8, height = 7)
facet_class(topic30class_sup)
ggsave("Figures/topic30class_sup.svg", width = 8, height = 7)



# Genre distribution within topics
facet_topics(topic10class_unsup)
ggsave("Figures/topic10topics_unsup.svg", width = 8, height = 7)
facet_topics(topic10class_sup)
ggsave("Figures/topic10topics_sup.svg", width = 8, height = 7)
facet_topics(topic20class_unsup)
ggsave("Figures/topic20topics_unsup.svg", width = 8, height = 7)
facet_topics(topic20class_sup)
ggsave("Figures/topic20topics_sup.svg", width = 8, height = 7)
facet_topics(topic30class_unsup)
ggsave("Figures/topic30topics_unsup.svg", width = 8, height = 7)
facet_topics(topic30class_sup)
ggsave("Figures/topic30topics_sup.svg", width = 8, height = 7)




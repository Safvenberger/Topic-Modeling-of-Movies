# Load extra functions
source("Visualizations/help_functions.R")

library(tidyverse)
library(RColorBrewer)
library(grid)
library(pBrackets) 

df_clean <- read_csv("Data/df_clean.csv")


# Probability distributions for each model and train documents
prob_dist_train20 <- read_csv("Output/gensim/model20_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1))
prob_dist_train40 <- read_csv("Output/gensim/model40_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1))
prob_dist_train60 <- read_csv("Output/gensim/model60_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1))

# Predicted classes (argmax) for each train document and their counts 
train_topics20 <- read_csv("Output/gensim/model20_labels.csv")
train_topics40 <- read_csv("Output/gensim/model40_labels.csv")
train_topics60 <- read_csv("Output/gensim/model60_labels.csv")

# Probability distributions for each model and test documents
prob_dist_test20 <- read_csv("Output/gensim/lda20_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1)) %>% 
  replace(is.na(.), 0)
prob_dist_test40 <- read_csv("Output/gensim/lda40_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1)) %>% 
  replace(is.na(.), 0)
prob_dist_test60 <- read_csv("Output/gensim/lda60_df.csv") %>% 
  mutate(Topic = apply(., 1, function(x) which.max(x) - 1)) %>% 
  replace(is.na(.), 0)


# List of all classes
class_list <- list(train_topics20, train_topics40, train_topics60)

# List of all predicted classes
pred_list <- list(prob_dist_test20, prob_dist_test40, prob_dist_test60)

#prob_distribution <- top_pred_test(class_list, pred_list)
#write_csv(prob_distribution, "Output/gensim/lda_prob_distr.csv")
prob_distribution <- read_csv("Output/gensim/lda_prob_distr.csv")


# Mean probabilies across all models
prob_distribution %>% 
  mutate(rowname = as.integer(rowname)) %>% 
  # Group by each test observation
  group_by(rowname) %>% 
  # Summarise the probabilities in each class and divide by the number of models (i.e. the mean)
  summarise(across(starts_with("Class"), ~ mean(.x, na.rm = TRUE))) -> pred_dist

pred_heatmap(pred_dist)
ggsave("Figures/genre_heatmap_lda.png", width = 14, height = 9)

class_dist(prob_distribution, df_clean)
ggsave("Figures/genre_distribution_lda.svg", width = 8, height = 7)

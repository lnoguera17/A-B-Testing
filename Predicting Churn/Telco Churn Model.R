library(tidyverse)
library(tidymodels)
library(skimr)


telco <- read_csv("Telco-Customer-Churn.csv")

# Quick Exploration

telco %>% skim()

# Look into that missing data 

telco %>% 
  filter(is.na(TotalCharges)) %>%  View()

# I assume the data missing is from recent customers, none of them are tenured, not churn and it's the only data missing.
# I'll assume 0 charge instead of a missing value

telco <-  telco %>% 
  mutate(TotalCharges =  replace_na(TotalCharges, 0),
         SeniorCitizen = as.factor(SeniorCitizen))

# Exploratory Analysis

telco %>% 
  ggplot(aes(tenure)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() 

# Look for Class Imbalance 

telco %>% 
  count(Churn, name = "Churn Count") %>% 
  ggplot(aes(Churn, `Churn Count`, fill = Churn)) +
  geom_col() +
  geom_text(aes(label = `Churn Count`), vjust = -0.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = NULL) +
  theme_minimal()


  





  





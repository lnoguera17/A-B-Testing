library(tidyverse)
library(scales)


results <- read.csv("Website Results.csv")
results_clean <- results %>% 
  filter(revenue < 2e+14) %>% 
  mutate(binary = case_when(converted == T ~ 1,
                            TRUE ~ 0),
         bounce = case_when(length_of_stay > 0 ~ 0,
                            TRUE ~ 1)) 

# Summary Statistics
# Removed an outlier from variant B that seemed quite to high in the data set
summary_stats <- results_clean %>% 
  group_by(variant) %>% 
  summarise(visits = n(),
            conversion = sum(binary),
            conversion_rate = conversion/visits,
            revenue_sum = sum(revenue),
            rev_per_conversion = (revenue_sum/conversion),
            revenue_sum = revenue_sum,
            avg_length_visit = mean(length_of_stay),
            bounce_rate = (sum(bounce)/n()) * 100)
            
# Test Statistics
test_stats <- summary_stats %>%
  pivot_wider(variant, 
              names_from = 'variant',
              values_from = c(conversion, visits, conversion_rate)) %>% 
  mutate(sample_proportion = (conversion_A + conversion_B)/(visits_A + visits_B),
         SE_pool = sqrt(sample_proportion * (1 - sample_proportion) * ((1 / visits_A) +  
                                                   (1 / visits_B))),
         MOE = SE_pool * qnorm(0.975),
         d_hat = conversion_rate_B - conversion_rate_A,
         z_score = d_hat / SE_pool,
         p_value = pnorm(q = -z_score,  
                         mean = 0,  
                         sd = 1) * 2) %>% 
  select(7:12) %>% 
  pivot_longer(1:6, values_to = "Value", names_to = "Metric")


#  Exploratory Visualizations
results %>% 
  filter(revenue < 2e+14) %>% 
  ggplot(aes(variant, revenue, color = variant)) +
  geom_boxplot() +
  scale_y_log10()


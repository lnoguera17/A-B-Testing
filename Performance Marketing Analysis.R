library(tidyverse)
library(scales)


results <- read.csv("Website Results.csv")


#  Exploratory Visualizations
revenue_plot <- results %>% 
  filter(revenue < 2e+14) %>% 
  ggplot(aes(variant, revenue, fill = variant, alpha = 0.07)) +
  geom_boxplot(show.legend = F) +
  scale_y_log10() +
  theme_minimal() 

revenue_plot

# Summary Statistics


results_clean <- results %>% 
  filter(revenue < 2e+14) %>% 
  mutate(binary = case_when(converted == T ~ 1,
                            TRUE ~ 0),
         bounce = case_when(length_of_stay > 0 ~ 0,
                            TRUE ~ 1)) 


# Removed an outlier from variant B that seemed quite to high in the data set. 
# Possible bug?
performance_metrics <- results_clean %>% 
  group_by(variant) %>% 
  summarise(visits = n(),
            conversion = sum(binary),
            conversion_rate = conversion/visits,
            revenue_sum = sum(revenue),
            rev_per_conversion = (revenue_sum/conversion),
            revenue_sum = revenue_sum,
            avg_length_visit = mean(length_of_stay),
            bounce_rate = (sum(bounce)/n()) * 100)
            
performance_metrics

# Test Statistics
test_stats <- performance_metrics %>%
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
                         sd = 1) * 2,
         conf_int_high = d_hat + MOE,
         conf_int_low = d_hat - MOE) %>% 
  select(7:14) %>% 
  pivot_longer(1:8, values_to = "Value", names_to = "Metric")

test_stats

confidence_interval_A <- performance_metrics %>%
  pivot_wider(variant, 
              names_from = 'variant',
              values_from = c(visits, conversion, conversion_rate)) %>% 
  mutate(se_hat_A =  sqrt(conversion_rate_A * (1 - conversion_rate_A) / visits_A),
         low_ci_A = conversion_rate_A - qnorm(0.975) * se_hat_A,
         high_ci_A = conversion_rate_A + qnorm(0.975) * se_hat_A) %>% 
  select(contains("_A")) %>% 
  pivot_longer(1:6, values_to = "Value", names_to = "Metric")


confidence_interval_B <- performance_metrics %>%
  pivot_wider(variant, 
              names_from = 'variant',
              values_from = c(visits, conversion, conversion_rate)) %>% 
  mutate(se_hat_B =  sqrt(conversion_rate_B * (1 - conversion_rate_B) / visits_B),
         low_ci_B = conversion_rate_B - qnorm(0.975) * se_hat_B,
         high_ci_B = conversion_rate_B + qnorm(0.975) * se_hat_B) %>% 
  select(contains("_B")) %>% 
  pivot_longer(1:6, values_to = "Value", names_to = "Metric")


Variant_comparison_table <- confidence_interval_B %>% 
  bind_rows(confidence_interval_A) %>% 
  mutate(Variant = case_when(str_detect(Metric, "_A") ~ "Variant_A",
                             TRUE ~ "Variant_B")) %>% 
  mutate(Metric = str_remove(Metric, "_B"),
         Metric = str_remove(Metric, "_A")) %>% 
  pivot_wider(names_from = "Variant", values_from = "Value")
  
Variant_comparison_table
  
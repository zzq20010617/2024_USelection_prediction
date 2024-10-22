#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Ziqi Zhu, Yuanchen Miao, Claire Chang
# Date: 22 October 2024
# Contact: ziqi.zhu@mail.utoronto.ca, vincent.miao@mail.utoronto.ca, claire.chang@mail.utoronto.ca
# License: None
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
#library(rstanarm)

#### Read data ####
poll_data <- read_csv("data/02-analysis_data/cleaned_data.csv")
candidate_avg_pct <- poll_data %>%
  group_by(candidate_name) %>%
  summarize(average_pct = mean(pct, na.rm = TRUE))

# View the result
print(candidate_avg_pct, n=60)
top_10_candidates <- candidate_avg_pct %>%
  arrange(desc(average_pct)) %>%  # Sort by average_pct in descending order
  head(10)

### Model data ####
first_model <-
  stan_glm(
    formula = flying_time ~ length + width,
    data = analysis_data,
    family = gaussian(),
    prior = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),
    prior_aux = exponential(rate = 1, autoscale = TRUE),
    seed = 853
  )


#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)



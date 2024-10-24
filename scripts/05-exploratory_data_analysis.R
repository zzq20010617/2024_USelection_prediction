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
  group_by(answer) %>%
  summarize(average_pct = mean(pct, na.rm = TRUE))

# View the result
print(candidate_avg_pct, n=60)
top_10_candidates <- candidate_avg_pct %>%
  arrange(desc(average_pct)) %>%  # Sort by average_pct in descending order
  head(10)

#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)



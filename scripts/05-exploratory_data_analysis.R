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

# View the candidate
print(candidate_avg_pct, n=60)
top_10_candidates <- candidate_avg_pct %>%
  arrange(desc(average_pct)) %>%  # Sort by average_pct in descending order
  head(10)

# View parties
nationwide_data <- poll_data %>%  filter(state == "National")
boxplot(pct ~ party, data = nationwide_data)

# Separate by state 
poll_counts <- poll_data %>%
  group_by(state) %>%  # Group by 'state'
  summarize(poll_count = n()) %>%  # Count the number of polls per state
  arrange(desc(poll_count))

# Check support pct for Biden and Harris
average_biden <- nationwide_data %>%
  filter(answer == "Biden") %>%
  summarize(avg_pct_biden = mean(pct, na.rm = TRUE))

# Calculate average support for Harris
average_harris <- nationwide_data %>%
  filter(answer == "Harris") %>%
  summarize(avg_pct_harris = mean(pct, na.rm = TRUE))


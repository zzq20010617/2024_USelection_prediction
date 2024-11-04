#### Preamble ####
# Purpose: Cleans the raw poll predicts data of United States president election 2024 from Five Thirty Eight 
# Author: Ziqi Zhu, Yuanchen Miao, Claire Chang
# Date: 2  November 2024
# Contact: ziqi.zhu@mail.utoronto.ca, vincent.miao@mail.utoronto.ca, claire.chang@mail.utoronto.ca
# License: None
# Pre-requisites: raw data must be obtained from poll-of-polls

#### Workspace setup ####
library(tidyverse)
library(lubridate)
library(dplyr)

#### Clean data ####
#Pollster: Different pollsters may have varying methodologies and biases.
#State: Election outcomes vary by state, especially in battleground states.
#Sample Size: Larger samples tend to produce more reliable estimates.
#Poll Methodology: Online, phone, in-person, etc., can influence poll results.
#Date: More recent polls might better reflect current voter sentiment.
#Party Affiliation: Voters' support tends to correlate with their party.
#Pollster Rating: Reliability and bias rating of the pollster.

# Read and load raw data
raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# change the type of end date in raw data from double to date
raw_data$end_date <- as.character(raw_data$end_date)
raw_data$end_date <- mdy(raw_data$end_date)

# select useful columns and filter by date to keep polls ends in 2024
cleaned_data <-
  raw_data |>
  janitor::clean_names() %>%
  select(pollster,sample_size,numeric_grade,pollscore,state,transparency_score,end_date,party,answer,pct) %>%
  filter(end_date >= as.Date("2024-01-01")) %>%
  mutate(
    state = if_else(is.na(state), "National", state))

#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/cleaned_data.csv")

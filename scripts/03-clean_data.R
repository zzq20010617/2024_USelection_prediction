#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

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

raw_data <- read_csv("data/01-raw_data/raw_data.csv")

# select useful columns and filter by date to keep polls ends in 2024
cleaned_data <-
  raw_data |>
  janitor::clean_names() %>%
  mutate(end_date = mdy(end_date)) %>%
  mutate(
    state = if_else(is.na(state), "National", state)) %>%
  filter(year(end_date) == 2024)|>
  select(pollster,sample_size,numeric_grade,pollscore,state,transparency_score,end_date,party,answer,pct)


#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/cleaned_data.csv")

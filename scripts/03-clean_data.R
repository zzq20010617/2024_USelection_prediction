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
  filter(end_date > as.Date("2024-01-01")) %>%
  mutate(
    state = if_else(is.na(state), "National", state))


#### Prepare dataset for Harris only ####
# Read in the data and clean variable names
data <- read_csv("data/01-raw_data/raw_data.csv") |>
  janitor::clean_names()

# Filter data to Harris estimates based on high-quality polls after she declared
just_harris_high_quality <- data |>
  filter(
    candidate_name == "Kamala Harris",
    numeric_grade >= mean(!is.na(numeric_grade))
  ) |>
  mutate(
    state = if_else(is.na(state), "National", state),
    end_date = mdy(end_date)
  ) |>
  filter(end_date >= as.Date("2024-07-21")) |> 
  mutate(
    num_harris = round((pct / 100) * sample_size, 0) 
  )

#### Save data ####
write_csv(cleaned_data, "data/02-analysis_data/cleaned_data.csv")
write_csv(just_harris_high_quality, "data/02-analysis_data/just_harris_high_quality.csv")

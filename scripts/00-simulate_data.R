#### Preamble ####
# Purpose: Simulates a dataset of United states president elections 2024 with different parties and states.
# Author: Ziqi Zhu, Yuanchen Miao, Claire Chang
# Date: 22 October 2024
# Contact: ziqi.zhu@mail.utoronto.ca, vincent.miao@mail.utoronto.ca, claire.chang@mail.utoronto.ca
# License: None
# Pre-requisites: The `tidyverse` package must be installed


#### Workspace setup ####
library(tidyverse)
set.seed(853)


#### Simulate data ####
# State names
states <- c(
  "Alabama",
  "Alaska",
  "Arizona",
  "Arkansas",
  "California",
  "Colorado",
  "Connecticut",
  "Delaware",
  "Florida",
  "Georgia",
  "Hawaii",
  "Idaho",
  "Illinois",
  "Indiana",
  "Lowa",
  "Kansas",
  "Kentucky",
  "Louisiana",
  "Maine",
  "Maryland",
  "Massachusetts",
  "Michigan",
  "Minnesota",
  "Mississippi",
  "Missouri",
  "Montana",
  "Nebraska",
  "Nevada",
  "New Hampshire",
  "New Jersey",
  "New Mexico",
  "New York",
  "North Carolina",
  "North Dakota",
  "Ohio",
  "Oklahoma",
  "Oregon",
  "Pennsylvania",
  "Rhode Island",
  "South Carolina",
  "South Dakota",
  "Tennessee",
  "Texas",
  "Utah",
  "Vermont",
  "Virginia",
  "Washington",
  "West Virginia",
  "Wisconsin",
  "Wyoming"
)

# Political parties
parties <- c("Republican", "Democratic", "Greens", "Libertarians", "Constitution", "Natural Law")

# Initialize a vector of length 50
extended_prob <- numeric(50)

# Assign the original probabilities to the first 8 elements
prob = c(0.25, 0.25, 0.15, 0.1, 0.1, 0.1, 0.025, 0.025)
extended_prob[1:8] <- prob

# Distribute the remaining probability mass (which is 0 in this case) equally among the rest of the elements
extended_prob[9:50] <- (1 - sum(extended_prob)) / 42

# Create a dataset by randomly assigning states and parties to divisions
analysis_data <- tibble(
  division = paste("Division", 1:151),  # Add "Division" to make it a character
  state = sample(
    states,
    size = 151,
    replace = TRUE,
    prob = extended_prob # Rough state population distribution
  ),
  party = sample(
    parties,
    size = 151,
    replace = TRUE,
    prob = c(0.40, 0.40, 0.05, 0.1, 0.05, 0.1) # Rough party distribution
  ),
  support = runif(151, min = 0, max = 100)
)


#### Save data ####
write_csv(analysis_data, "data/00-simulated_data/simulated_data.csv")

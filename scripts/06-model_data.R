#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
#library(rstanarm)

#### Read data ####
nationwide_data <- read_csv("data/02-analysis_data/nationwide_data.csv")

# only two entries for Conservative Party
unique_parties <- unique(nationwide_data$party)

# drop columns that have NA in numeric_grade or pollscore, and 2 entries for CON party
nationwide_data <- nationwide_data %>%
  drop_na(numeric_grade, pollscore) %>%
  filter(party != "CON") %>%
  mutate(days_since_start = as.numeric(end_date - min(end_date)))

unique_parties <- unique(nationwide_data$party)
nationwide_data$party <- as.factor(nationwide_data$party)
#model that include party as a predictor
model <- lm(pct ~ log(sample_size) + pollscore + numeric_grade + transparency_score+days_since_start+party, data = nationwide_data)
plot(model, which = 1)
new_data <- data.frame(
  sample_size = 1500,
  pollscore = -1.5,
  numeric_grade = 3,
  transparency_score = 8,
  days_since_start = 310,
  party = factor("DEM", levels = levels(nationwide_data$party))
)


### Model data ####
boxplot(pct ~ party, data = nationwide_data)

# Create an empty list to store models
models <- list()

for (party in unique_parties) {
  # Subset the data for the current party
  party_data <- filter(nationwide_data, party == !!party)
  
  # Fit the linear model for the current party
  model <- lm(pct ~ log(sample_size) + pollscore + numeric_grade + transparency_score+days_since_start, data = party_data)
  
  # Store the model in the list, using the party name as the key
  models[[party]] <- model
}

for (party in names(models)) {
  # Extract the model for the current party
  current_model <- models[[party]]

  # Create a Residuals vs Fitted plot for the current model
  plot(current_model, which = 1, main = paste("Residuals vs Fitted for", party, "Party"))

  # Optionally, pause between plots to view them
  readline(prompt = "Press [Enter] to continue to the next plot...")
}

# choose a party to check summary
model_summary <- summary(models[['DEM']])

# Print the model summary
print(model_summary)

# Access the R-squared and Adjusted R-squared values
r_squared <- model_summary$r.squared
adjusted_r_squared <- model_summary$adj.r.squared

#### Save model ####
saveRDS(
  model,
  file = ""
)



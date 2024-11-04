#### Preamble ####
# Purpose: SLM of specific candidate are fitted and MLM of all candidates from United States president election 2024 are fitted
# Author: Ziqi Zhu, Yuanchen Miao, Claire Chang
# Date: 22 October 2024
# Contact: ziqi.zhu@mail.utoronto.ca, vincent.miao@mail.utoronto.ca, claire.chang@mail.utoronto.ca
# License: None
# Pre-requisites: 03-clean-data.R must be run to obtained the data used in this script,04-test_cleaned_data.R must be run to verify that cleaned data could be used to fit a model
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(modelsummary)
library(rstanarm)
library(splines)

#### Read data ####
cleaned_data <- read_csv("data/02-analysis_data/cleaned_data.csv")
nationwide_data <- cleaned_data %>%  filter(state == "National")

# only two entries for Conservative Party
unique_parties <- unique(nationwide_data$party)

# drop columns that have NA in numeric_grade or pollscore, and 2 entries for CON party
nationwide_data <- nationwide_data %>%
  drop_na(numeric_grade, pollscore) %>%
  filter(party != "CON") %>%
  mutate(days_since_start = as.numeric(end_date - min(end_date)))

unique_parties <- unique(nationwide_data$party)
nationwide_data$party <- as.factor(nationwide_data$party)

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


# Need number not percent for Bayesian models
nationwide_data <- nationwide_data %>% mutate(
  num_party = round((pct / 100) * sample_size, 0)
)
# filter nationwide_data by parties
dem_data <- nationwide_data %>%
  filter(party == "DEM")
dem_data <- na.omit(dem_data)

dem_data <- dem_data |>
  mutate(
    pollster = factor(pollster)
  )

rep_data <- nationwide_data %>%
  filter(party == "REP")
rep_data <- na.omit(rep_data)

rep_data <- rep_data |>
  mutate(
    pollster = factor(pollster)
  )


# Augment the datasets with predictions
dem_data <- dem_data |>
  mutate(fitted_pct_dem = predict(models[['DEM']]))
rep_data <- rep_data |>
  mutate(fitted_pct_rep = predict(models[['REP']]))

# Bayesian model section 
model_formula_dem <- cbind(num_party, sample_size - num_party) ~ log(sample_size) + days_since_start + 
   pollscore + numeric_grade + transparency_score + (1 | pollster)

# Set normal priors for the coefficients
priors <- normal(0, 2.5, autoscale = TRUE)

bayesian_model_dem <- stan_glmer(
  formula = model_formula_dem,
  data = dem_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  cores = 4,
  adapt_delta = 0.95,
)

bayesian_model_rep <- stan_glmer(
  formula = model_formula_dem,
  data = rep_data,
  family = binomial(link = "logit"),
  prior = priors,
  prior_intercept = priors,
  seed = 123,
  adapt_delta = 0.95
)

plot(bayesian_model_dem, pars = "(Intercept)", prob = 0.95)

spline_model_dem <- stan_glm(
  pct ~ ns(days_since_start, df = 7) + pollscore + numeric_grade + transparency_score + log(sample_size) + pollster,  # Spline with 7 degrees of freedom for end_date_num
  data = dem_data,                            # DEM national dataset
  family = gaussian(),                        # Use a Gaussian family for continuous outcome
  prior = normal(0, 5),                       # Weakly informative prior for coefficients
  prior_intercept = normal(50, 10),           # Weakly informative prior for intercept
  seed = 1234,                                # Set seed for reproducibility
  iter = 2000,                                # Number of iterations
  chains = 4,                                 # Number of MCMC chains
  refresh = 0                                 # Suppress unnecessary output
)

spline_model_rep <- stan_glm(
  pct ~ ns(days_since_start, df = 7) + pollscore + numeric_grade + transparency_score + log(sample_size) + pollster,
  data = rep_data,
  family = gaussian(),
  prior = normal(0, 5),
  prior_intercept = normal(50, 10),
  seed = 1234,
  iter = 2000,
  chains = 4,
  refresh = 0
)

#### Save model ####
saveRDS(
  bayesian_model_dem,
  file = "models/bayesian_model_dem.rds"
)
saveRDS(
  bayesian_model_rep,
  file = "models/bayesian_model_rep.rds"
)
saveRDS(models, file = "models/MLRmodels.rds")
saveRDS(spline_model_dem, file = "models/spline_dem.rds")
saveRDS(spline_model_rep, file = "models/spline_rep.rds")



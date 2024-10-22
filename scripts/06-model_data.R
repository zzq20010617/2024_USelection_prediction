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

for (party in names(models)) {
  # Extract the model for the current party
  current_model <- models[[party]]

  # Create a Residuals vs Fitted plot for the current model
  plot(current_model, which = 1, main = paste("Residuals vs Fitted for", party, "Party"))

  # Optionally, pause between plots to view them
  readline(prompt = "Press [Enter] to continue to the next plot...")
}

# choose a party to check summary
model_summary <- summary(models[['REP']])
# Access the R-squared and Adjusted R-squared values
r_squared <- model_summary$r.squared
adjusted_r_squared <- model_summary$adj.r.squared

# Print the model summary
print(model_summary)

# Need number not percent for some models
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

combined_data <- bind_rows(dem_data, rep_data)
ggplot(combined_data, aes(x = days_since_start)) +
#  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_pct_dem), data = subset(combined_data, party == "DEM"), color = "blue") +
  geom_line(aes(y = fitted_pct_rep), data = subset(combined_data, party == "REP"), color = "red") +
  theme_classic() +
  labs(y = "Percent", x = "Date", title = "Linear Model: 
       pct ~ log(sample_size) + pollscore + numeric_grade + transparency_score + 
       days_since_start")

modelsummary(models = models)

# Bayesian model section 
model_formula_dem <- cbind(num_party, sample_size - num_party) ~ log(sample_size) +
  pollscore + numeric_grade + (1 | pollster)

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
  adapt_delta = 0.95
)
priors |>
  ggplot(aes(x = marathon_time)) +
  geom_histogram(binwidth = 10) +
  theme_classic()

# Posterior predictive checks
pp_check(bayesian_model_dem)

plot(bayesian_model_dem, pars = "(Intercept)", prob = 0.95)

spline_model_dem <- stan_glm(
  pct ~ ns(days_since_start, df = 7) + pollscore + log(sample_size) + pollster,  # Spline with 5 degrees of freedom for end_date_num
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
  pct ~ ns(days_since_start, df = 7) + pollscore + log(sample_size) + pollster,  # Spline with 5 degrees of freedom for end_date_num
  data = rep_data,                            # DEM national dataset
  family = gaussian(),                        # Use a Gaussian family for continuous outcome
  prior = normal(0, 5),                       # Weakly informative prior for coefficients
  prior_intercept = normal(50, 10),           # Weakly informative prior for intercept
  seed = 1234,                                # Set seed for reproducibility
  iter = 2000,                                # Number of iterations
  chains = 4,                                 # Number of MCMC chains
  refresh = 0                                 # Suppress unnecessary output
)

pp_check(spline_model_dem)
pp_check(spline_model_rep)
# Create new data for prediction
new_data <- data.frame(
  pollscore = -1.5,
  sample_size = 1200,
  numeric_grade = 3,
  days_since_start = seq(
    min(dem_data$days_since_start),
    max(dem_data$days_since_start),
    length.out = 100
  ),
  pollster = factor("TIPP", levels = levels(dem_data$pollster))
)

posterior_preds_dem <- posterior_predict(spline_model_dem, newdata = new_data)
posterior_preds_rep <- posterior_predict(spline_model_rep, newdata = new_data)

# Summarize predictions
pred_summary_dem <- new_data |>
  mutate(
    pred_mean_dem = colMeans(posterior_preds_dem),
    pred_lower_dem = apply(posterior_preds_dem, 2, quantile, probs = 0.025),
    pred_upper_dem = apply(posterior_preds_dem, 2, quantile, probs = 0.975),
    party = "DEM"
  )

# Summarize predictions for REP
pred_summary_rep <- new_data |>
  mutate(
    pred_mean_rep = colMeans(posterior_preds_rep),
    pred_lower_rep = apply(posterior_preds_rep, 2, quantile, probs = 0.025),
    pred_upper_rep = apply(posterior_preds_rep, 2, quantile, probs = 0.975),
    party = "REP"
  )

# Combine both summaries into a single dataframe
pred_summary <- bind_rows(pred_summary_dem, pred_summary_rep)

combined_data <- bind_rows(dem_data, rep_data)

# Plot the spline fit
ggplot(combined_data, aes(x = days_since_start, y = pct, color = pollster)) +
  geom_line(
    data = pred_summary %>% filter(party == "DEM"),
    aes(x = days_since_start, y = pred_mean_dem),
    color = "blue",
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = pred_summary %>% filter(party == "DEM"),
    aes(x = days_since_start, ymin = pred_lower_dem, ymax = pred_upper_dem),
    alpha = 0.2,
    fill = "blue",
    inherit.aes = FALSE
  ) +
  # Add REP predictions
  geom_line(
    data = pred_summary %>% filter(party == "REP"),
    aes(x = days_since_start, y = pred_mean_rep),
    color = "red",
    inherit.aes = FALSE
  ) +
  geom_ribbon(
    data = pred_summary %>% filter(party == "REP"),
    aes(x = days_since_start, ymin = pred_lower_rep, ymax = pred_upper_rep),
    alpha = 0.2,
    fill = "red",
    inherit.aes = FALSE
  ) +
  labs(
    x = "Days since 2024",
    y = "Percentage",
    title = "Poll Percentage over Time with Spline Fit"
  ) +
  theme_minimal()

#### Just Harris model ####

# Model 1: pct as a function of end_date
model_date <- lm(pct ~ end_date, data = just_harris_high_quality)

# Augment data with model predictions
just_harris_high_quality <- just_harris_high_quality |>
  mutate(
    fitted_date = predict(model_date),
  )

# Plot model predictions
# Model 1
ggplot(just_harris_high_quality, aes(x = end_date)) +
  geom_point(aes(y = pct), color = "black") +
  geom_line(aes(y = fitted_date), color = "blue", linetype = "dotted") +
  theme_classic() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(y = "Harris percent", x = "Date", title = "Linear Model: pct ~ end_date")



#### Save model ####
saveRDS(
  bayesian_model_dem,
  file = "models/bayesian_model_dem.rds"
)



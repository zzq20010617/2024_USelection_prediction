#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned United States president election 2024 data
# Author: Ziqi Zhu, Yuanchen Miao, Claire Chang
# Date: 22 October 2024
# Contact: ziqi.zhu@mail.utoronto.ca, vincent.miao@mail.utoronto.ca, claire.chang@mail.utoronto.ca
# License: None
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 03-clean_data.R must be run


#### Workspace setup ####
library(tidyverse)
library(testthat)

analysis_data <- read_csv("data/02-analysis_data/cleaned_data.csv")

# Test if the data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####

# Test that the dataset has 10 columns
test_that("dataset has 10 columns", {
  expect_equal(ncol(analysis_data), 10)
})

# Test that the 'state' column is character type
test_that("'state' is character", {
  expect_type(analysis_data$state, "character")
})

# Test that 'state' column contains only valid United state names
valid_states <- c(
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
  "Iowa",
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
  "Wyoming",
  "National",
  "Maine CD-2",
  "Maine CD-1",
  "Nebraska CD-1",
  "Nebraska CD-2",
  "Nebraska CD-3"
)
test_that("'state' contains valid United State state names", {
  expect_true(all(analysis_data$state %in% valid_states))
})

# Test that the 'pollster' column is character type
test_that("'pollster' is character", {
  expect_type(analysis_data$pollster, "character")
})

# Test that the 'pollster' column is character type
test_that("'pollster' is character", {
  expect_type(analysis_data$pollster, "character")
})


# Test that the 'party' column is character type
test_that("'party' is character", {
  expect_type(analysis_data$party, "character")
})

# Test that there are no empty strings in 'party', or 'state' columns
test_that("no empty strings in 'party', or 'state' columns", {
  expect_false(any(analysis_data$party == "" | analysis_data$state == ""))
})

# Test that the 'party' column contains at least 2 unique values
test_that("'party' column contains at least 2 unique values", {
  expect_true(length(unique(analysis_data$party)) >= 2)
})
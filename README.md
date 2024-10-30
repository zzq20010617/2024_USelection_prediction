# The US Presidential Election Forecasting

## Overview

This project applies a statistical approach to forecast the winner of the upcoming US presidential election through "poll-of-polls" approach, and discuss the methodology and survey of certain pollster in the election.


## File Structure

The repo is structured as:

- **data/**
  - `00-simulated_data/`: Contains the simulated data generated for model testing.
    - `simulated_data.csv`
  - `01-raw_data/`: Contains the raw polling data used for analysis.
    - `raw_data.csv`
  - `02-analysis_data/`: Contains cleaned and analyzed data that was constructed from the raw data.
    - `cleaned_data.csv`
    - `just_harris_high_quality.csv`
    - `nationwide_data.csv`
    
- **models/**: Contains fitted models for both Harris and Trump, saved in RDS format.
  - `bayesian_model_dem.rds`
  
- **other/**:
  - `llm_usage/`: Includes documentation about interactions with large language models (LLMs).
    - `usage.txt`
    
- **paper/**: Contains files used to generate the final paper.
  - `paper.pdf`: The final report in PDF format.
  - `paper.qmd`: The Quarto document used to generate the report.
  - `references.bib`: Bibliography file with references for the paper.

- **scripts/**: Contains R scripts for simulating, downloading, cleaning, and analyzing the data.
  - `00-simulate_data.R`
  - `01-test_simulated_data.R`
  - `03-clean_data.R`
  - `04-test_cleaned_data.R`
  - `05-exploratory_data_analysis.R`
  - `06-model_data.R`
  - `example.R` 
  
- `.gitignore`: Specifies files to ignore in version control.
- `2024_USelection_prediction.Rproj`: The R project file.
- `README.md`: This file.
  

## Statement on LLM usage

Aspects of the code were written with the assistance of ChatGPT-4o. The entire chat history is saved in other/llm_usage/usage.txt.

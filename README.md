# Doggy DNA Analyzer

## Overview

This R script demonstrates advanced statistical analysis applied to a fun real-world problem: determining which family member best guessed our mixed-breed puppy's DNA test results. The code showcases robust data manipulation, statistical comparison of probability distributions, and data visualization techniques to analyze and rank family members' predictions against actual DNA test results.

## Key Features

- **Statistical Distribution Analysis**: Implements multiple comparison metrics including Kullback-Leibler divergence, Jensen-Shannon divergence, and Euclidean distance to quantitatively evaluate prediction accuracy
  
- **Custom Distribution Comparison Function**: Creates a reusable function for comparing probability distributions with proper statistical testing and error handling

- **Elegant Data Visualization**: Generates clear visual representations of breed guess distributions across family members versus actual DNA results

- **Data Transformation**: Demonstrates advanced data manipulation using tidyverse libraries to reshape and prepare data for analysis

- **Reproducible Research**: Thoroughly commented code making the analysis transparent and reproducible

## Technical Capabilities Demonstrated

- **Advanced R Programming**: Leverages modern R packages including dplyr, purrr, ggplot2, and janitor
  
- **Functional Programming**: Uses map functions for efficient and elegant code structure
  
- **Statistical Knowledge**: Applies information theory concepts and statistical tests to quantify differences between distributions
  
- **Data Wrangling**: Shows proficiency in reshaping data between wide and long formats
  
- **Visualization Design**: Creates intuitive visualizations that effectively communicate comparative results

## Real-World Application

This project showcases the ability to:

1. Take an everyday question ("Who guessed our dog's breed best?")
2. Apply sophisticated statistical techniques to analyze it rigorously
3. Present results in a clear, visual format accessible to non-technical audiences
4. Build reusable functions that could be applied to similar distribution comparison problems

By combining technical excellence with creative problem-solving, this code exemplifies how data science can transform even casual family competitions into opportunities for insight.

## Usage

The code works with a simple data table of breed percentages (from DNA testing) and family members' guesses. It can be easily adapted to similar comparison problems by modifying the input data structure.

# empirical-power-shiny-app
An R Shiny app for computing empirical power of a t-test

Overview

This project is an interactive R Shiny application designed to simulate and evaluate the empirical power of t-tests. It allows users to explore how sample size, variance, and effect size influence statistical significance and study design.

Key Features
Supports one-sample, paired, and two-sample t-tests
Estimates empirical power through simulation
Calculates required sample size to achieve a target power
Allows customization of:
Means and standard deviations
Sample sizes
Significance level (alpha)
Alternative hypotheses
Provides interactive visualizations for interpreting results

Methods
Simulation-based approach using repeated sampling
Iterative power estimation across 100 simulated datasets per scenario
Statistical testing using t-test frameworks

Tools & Technologies
R
Shiny
Statistical simulation

Use Case

This tool supports understanding of statistical power and sample size planning, with applications in public health research and study design.

How to Run
Clone the repository
Open app.R in RStudio
Run the application
shiny::runApp()

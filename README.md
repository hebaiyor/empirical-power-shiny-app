# empirical-power-shiny-app
An R Shiny app for computing empirical power of a t-test

**Overview**

This project is an interactive R Shiny application designed to simulate and evaluate the empirical power of t-tests under different statistical scenarios. The tool allows users to explore how sample size, effect size, and variability influence statistical significance and study design decisions.

By translating statistical theory into an interactive simulation tool, this project supports intuitive understanding of power analysis and sample size planning, particularly in research and public health contexts.

- Supports one-sample, paired, and two-sample t-tests
- Simulates empirical power using repeated random sampling
- Estimates required sample size to achieve a target power level
- Customizable inputs including:
    - Means and standard deviations
    - Sample sizes
    - Significance level (alpha)
    - Alternative hypothesis type
- Interactive outputs for exploring how assumptions affect results, including polished visualizations


**Methods**

- Simulation-based approach using repeated sampling (Monte Carlo simulation)
- Iterative power estimation across 100 simulated datasets per scenario
- Statistical testing using t-test frameworks
- Dynamic user input-driven modeling via Shiny
- Customizable output options

**Tools & Technologies**

- R
- Shiny
- Statistical simulation
- Hypothesis testing

**Use Case**

This tool can support study design in public health and clinical research by helping analysts estimate required sample sizes and understand the impact of assumptions on statistical power.
This application is useful for:
  - Study design in public health and clinical research
  - Understanding the relationship between sample size and statistical power
  - Teaching core statistical concepts interactively

**How to Run**

1. Clone this repository
2. Open app.R in RStudio
3. Install required package if needed:
   
        install.packages("shiny")
5. Run the app:
   
        shiny::runApp()

## 📷 App Preview

### Example Output - Empirical Power for Two-Sample Test
![Power Plot](images/TwoSamplePower.png)

### Example Output - Sample Size for Paired T-Test
![Dashboard](images/PairedTestSampleSize.png)



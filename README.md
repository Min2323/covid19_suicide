# COVID-19 and Suicide/Self-Harm in Korea: Time-Series and Causal Inference Analysis

This repository contains the full R code used for the study:

> **"The Effect of the COVID-19 Pandemic on Suicide and Self-Harm in Korea: Interrupted Time Series and Causal Impact Analyses"**  
> Published in *Psychiatry Investigation* (2024)  
> 🔗 [Read the full article here](https://www.psychiatryinvestigation.org/m/journal/view.php?number=1798)

---

## 📁 Repository Structure

| File | Description |
|------|-------------|
| **`preprocessing.R`** | Loads and preprocesses the NEDIS dataset: cleans columns, recodes factors, generates suicide/self-harm flags, and labels pre/post-COVID-19 era. |
| **`interrupted time series analysis.R`** | Performs segmented Poisson and quasi-Poisson regression to evaluate changes in monthly suicide/self-harm rates before and after the COVID-19 outbreak, with seasonal and trend adjustment. |
| **`forestplot.position.R`** | Estimates subgroup-specific rate ratios (RRs) comparing pre- vs. post-COVID periods (stratified by age and sex), and visualizes them using forest plots. |
| **`causal inference nocovariate.R`** | Uses Google’s `CausalImpact` package to estimate the causal effect of COVID-19 on suicide/self-harm without covariate adjustment. |
| **`causal inference addcovariate.R`** | Same as above, but includes adjustment for covariates such as age group, sex, injury mechanism, and insurance type. |

---

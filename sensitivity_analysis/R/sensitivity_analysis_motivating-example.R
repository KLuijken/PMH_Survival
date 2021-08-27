# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Add-on: sensitivity analysis in motivating example, incident diabetes
# -----------------------------------------------------------------------------#
library(data.table)
library(survival)
library(riskRegression)
library(timeROC)
library(ggplot2)
library(cowplot)
library(rms)
library(glmnet)

# load and clean data
source("./R/motivating_example/clean_data.R")

# derive prediction model in derivation sample (takes around 1 minute)
source("./R/motivating_example/model_derivation.R")

# model validation is performed in script R/motivating_example/model_validation.R

# source helper functions for sensitivity analysis
source("./R/helper_functions.R")


# Perform sensisitivity analysis ----

### 1. State the prediction target (see main text).

### 2. Report discrepancies between derivation study, validation study, and 
###    prediction target (see main text).

### 3. Identify one predictor that is expected to be measured in the target 
###    clinical setting using a different procedure than used to collect the 
###    validation data. 
#      Identified predictor: BMI
#      Validation setting: measured during medical examination
#      Clinical setting: measured using self-report

### 4. Define the anticipated predictor measurement heterogeneity between the 
###    validation study and target clinical setting in terms of a measurement 
###    heterogeneity model.
#      Self-reported BMI = psi + theta * measured BMI + epsilon,
#      where epsilon ~ N(0, sd_epsilon^2)

### 5. Perform a literature search to establish the size of measurement error 
###    parameters for the anticipated predictor measurement heterogeneity.
#      See main text. Ranges:
#      Psi: -1 to 0
#      Theta: 0.8 to 1
#      Sd_Epsilon: 0 to 2

### 6. Simulate the scenarios of anticipated measurement heterogeneity to assess
###    the possible impact on predictive performance.


# Input information from derivation study
# -----------------------------------------------------------------------------#
# Specify a matrix with the predictor names and coefficients of predictors in 
# obtained from the derivation study 
# Typically, this is external information, but in this example we derived the 
# model in the same script (see R/motivating_example/model_derivation.R).
pred_names <- row.names( coefficients( ridge_mod) )
pred_coefs <- as.numeric( coefficients( ridge_mod) )
baseline_surv_tval <- baseline_surv_tval

# Specify the time point at which the model is validated
t_val <- floor( 6 * 365.25)

# Define scenarios of predictor measurement heterogeneity (pmh)
# -----------------------------------------------------------------------------#
psi        <- c( -1, -0.5, 0)
theta      <- c( 0.9, 1)
sd_epsilon <- c( 0, 1, 1.5)

scenarios  <- expand.grid( psi = psi,              # 18 scenarios
                           theta = theta,
                           sd_epsilon = sd_epsilon)

# Perform validation across pmh scenarios
# -----------------------------------------------------------------------------#
# it takes around 19 minutes to run this code. Alternatively, results
# can be found in ./data/output_sensitivity.rds
#
# note: prepare validation data such that variables time and event have  
# standardized names
results_listed <- purrr::pmap( list( psi = scenarios$psi,
                                     theta = scenarios$theta,
                                     sd_epsilon = scenarios$sd_epsilon),
                  pmh_sensitivity_analysis,# see R/helperfunctions.R
                  pmh_predictor = "bmi",   # the predictor for which measurement
                                           # heterogeneity is expected
                  validation_data = validation_data,
                  pred_names = pred_names,
                  pred_coefs = pred_coefs,
                  t_val = t_val,
                  baseline_surv = baseline_surv_tval,
                  bootstrap_rep = 500
             )

results <- do.call( rbind, results_listed)

# saveRDS(results, file = "./data/output_sensitivity.rds")


# Plot outcomes
# -----------------------------------------------------------------------------#

# prepare data
plot_data <- cbind( scenarios,
                    results)

homogeneity_coordinate <- plot_data[ plot_data$psi == 0 &
                                       plot_data$theta == 1 &
                                       plot_data$sd_epsilon == 0,]

# plot results sensitivity analyses
plotting_data( plot_data)

### 7. Report the anticipated impact of predictor measurement heterogeneity on 
###    predictive performance in clinical implementation (see main text).

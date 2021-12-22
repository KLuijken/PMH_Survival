# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Add-on: sensitivity analysis in motivating example, incident diabetes
# -----------------------------------------------------------------------------#
library(survival)
library(riskRegression)
library(timeROC)
library(ggplot2)
library(cowplot)
library(rms)
library(pec)

# load and clean data
source("./R/clean_data.R")

# source helper functions for sensitivity analysis
source("./R/helper_functions.R")


# Perform model validation ----

# Input information from derivation study
# -----------------------------------------------------------------------------#
# Specify a matrix with the predictor names and coefficients of predictors in 
# obtained from the derivation study https://doi.org/10.1371/journal.pone.0152054
pred_names <- c( "age", "bmi", "fpg", "tg")
pred_coefs <- c( 0.027, 0.124, 0.76, 0.239)
baseline_surv_tval <- 0.999998

# Specify the time point at which the model is validated
t_val <- floor( 6 * 365.25)

# It takes around 1 minute to run line 40 to 79. Alternatively, results
# can be found in ./data/external_val.rds

# Create storage for results
performance_measures <- c( "cal_large",
                           "AUC_t",
                           "IPA_t")
bootstrap_rep <- 500
bootstrapped_estimates <- matrix( NA, 
                                  nrow = bootstrap_rep, 
                                  ncol = length( performance_measures))
colnames( bootstrapped_estimates) <- performance_measures

# Draw bootstrap samples and validate model in each sample
for(i in 1:bootstrap_rep){
  bootstrap_data <- diabetes_data[ sample( 1:nrow( diabetes_data), 
                                            size = nrow( diabetes_data), 
                                            replace = T), ]
  bootstrapped_estimates[i, ] <- unlist( validate_model( pred_names = pred_names,
                                                         pred_coefs = pred_coefs,
                                                         dataset = bootstrap_data,
                                                         t_val = t_val,
                                                         baseline_surv = baseline_surv_tval))
}

# Derive predictive performance point estimates and percentile CIs
point_estimates <- apply( bootstrapped_estimates,
                          2,
                          median )
lower_bounds <- apply( bootstrapped_estimates,
                       2,
                       function( x) quantile( x, 0.025) )
upper_bounds <- apply( bootstrapped_estimates,
                       2,
                       function( x) quantile( x, 0.975) )

external_val_performance <- cbind( t( point_estimates[ c("cal_large", "AUC_t", "IPA_t")]),
                                   t( lower_bounds[ c("cal_large", "AUC_t", "IPA_t")]),
                                   t( upper_bounds[ c("cal_large", "AUC_t", "IPA_t")]) )
colnames( external_val_performance) <- c( performance_measures,
                                          paste0( "ci_low_", performance_measures),
                                          paste0( "ci_up_", performance_measures))

external_val_performance
# round( external_val_performance, digits = 2)
# saveRDS( external_val_performance, "./data/external_val.rds")

# Based on miscalibration, update baseline hazard
lp <- as.matrix( diabetes_data[ , pred_names]) %*% pred_coefs
f <- coxph( Surv( time, event) ~ offset(lp), 
            data = diabetes_data,
            x = T)
updated_baseline_surv_tval <- mean( predictSurvProb( f,
                                             newdata = diabetes_data,
                                             times = t_val))
diabetes_data <- data.frame( diabetes_data) # predictSurvProb altered object class

# Evaluate performance with updated baseline hazard
for(i in 1:bootstrap_rep){
  bootstrap_data <- diabetes_data[ sample( 1:nrow( diabetes_data), 
                                           size = nrow( diabetes_data), 
                                           replace = T), ]
  bootstrapped_estimates[i, ] <- unlist( validate_model( pred_names = pred_names,
                                                         pred_coefs = pred_coefs,
                                                         dataset = bootstrap_data,
                                                         t_val = t_val,
                                                         baseline_surv = updated_baseline_surv_tval))
}

# Derive predictive performance point estimates and percentile CIs
point_estimates <- apply( bootstrapped_estimates,
                          2,
                          median )
lower_bounds <- apply( bootstrapped_estimates,
                       2,
                       function( x) quantile( x, 0.025) )
upper_bounds <- apply( bootstrapped_estimates,
                       2,
                       function( x) quantile( x, 0.975) )

updated_external_val_performance <- cbind( t( point_estimates[ c("cal_large", "AUC_t", "IPA_t")]),
                                           t( lower_bounds[ c("cal_large", "AUC_t", "IPA_t")]),
                                           t( upper_bounds[ c("cal_large", "AUC_t", "IPA_t")]) )
colnames( updated_external_val_performance) <- c( performance_measures,
                                                  paste0( "ci_low_", performance_measures),
                                                  paste0( "ci_up_", performance_measures))

updated_external_val_performance
# round( updated_external_val_performance, digits = 2)
# saveRDS( updated_external_val_performance, "./data/external_val_updated.rds")


# Perform sensisitivity analysis ----

### 1. State the prediction target (see main text).

### 2. Report whether predictor measurement procedures in the validation setting
###    correspond to those at implementation (see main text).

### 3. Identify one predictor that is expected to be measured in the target 
###    clinical setting using a different procedure than used to collect the 
###    validation data. 
#      Identified predictor: BMI
#      Validation setting: measured during medical examination
#      Clinical setting: measured using self-report

### 4. Define a model for the relation between the measurement in the validation
###    study and its equivalent in the implementation setting.
#      Self-reported BMI = psi + theta * measured BMI + epsilon,
#      where epsilon ~ N(0, sd_epsilon^2)

### 5. Perform a literature search to establish the size of the possible 
###    parameters of predictor measurement heterogeneity.
#      See main text. Ranges:
#      Psi: -1 to 0
#      Theta: 0.9 to 1
#      Sd_Epsilon: 0 to 1.5

### 6. Simulate the scenarios of anticipated measurement heterogeneity to assess
###    the possible impact on predictive performance.


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
                  validation_data = diabetes_data,
                  pred_names = pred_names,
                  pred_coefs = pred_coefs,
                  t_val = t_val,
                  baseline_surv = updated_baseline_surv_tval,
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

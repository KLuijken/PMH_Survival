# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Add-on: model derivation for motivating example
# -----------------------------------------------------------------------------#
library(data.table)
library(survival)
library(riskRegression)
library(timeROC)
library(rms)
library(glmnet)

# source helper functions ----
source("./R/helper_functions.R")

# load and clean data ----
source("./R/motivating_example/clean_data.R")


# derive penalized coxph model ----
# tune lambda using cross-validation, package glmnet
init       <- glmnet( x = as.matrix( derivation_data[, c("age", "bmi", "fpg", "tg")]),
                      y = Surv( derivation_data$time, derivation_data$event),
                      family="cox",
                      alpha = 0) # ridge penalty
sequence   <- exp( seq( from = min( init$lambda), 
                        by = log( init$lambda[2]) - log( init$lambda[1]),
                        length.out = 200))
# optimize deviance in 10-fold cross-validations
MRidgeCV   <- cv.glmnet( x = as.matrix( derivation_data[, c("age", "bmi", "fpg", "tg")]),
                         y = Surv( derivation_data$time, derivation_data$event),
                         nfolds = 10,
                         type.measure = "deviance",
                         family = "cox",
                         alpha = 0, # ridge penalty
                         lambda = sequence)

ridge_mod <- glmnet( x = as.matrix( derivation_data[, c("age", "bmi", "fpg", "tg")]),
                     y = Surv( derivation_data$time, derivation_data$event),
                     family="cox",
                     alpha = 0,
                     lambda = MRidgeCV$lambda.min  # optimal deviance cross-validated lambda
)

# inspect model
coefficients( ridge_mod)

# store cumulative baseline hazard at t_val
# for average individual at time = t_val (i.e., for a single “pseudo” subject 
# with covariate values equal to the means of the data set)
obj_survfit <- survfit( ridge_mod, 
                        x = as.matrix( derivation_data[, c("age", "bmi", "fpg", "tg")]),
                        y = Surv( derivation_data$time, derivation_data$event))
baseline_surv_tval <- summary( obj_survfit, times = t_val)$surv


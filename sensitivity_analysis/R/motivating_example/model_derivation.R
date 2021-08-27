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

# store cumulative baseline survival at t_val
# for covariate values equal to zero at time = t_val 
# covariate values are set to zero to avoid the need for centering of the linear 
# predictor at validation
obj_survfit <- survfit( ridge_mod,
                        x = as.matrix( derivation_data[, c("age", "bmi", "fpg", "tg")]),
                        y = Surv( derivation_data$time, derivation_data$event),
                        newx = as.matrix( data.frame( "age" = 0,
                                                      "bmi" = 0,
                                                      "fpg" = 0,
                                                      "tg" = 0)))
baseline_surv_tval <- summary( obj_survfit, times = t_val)$surv

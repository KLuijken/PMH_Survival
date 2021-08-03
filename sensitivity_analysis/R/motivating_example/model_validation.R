# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Add-on: model validation for motivating example
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
source("./R/model_derivation.R")

# apparent performance ----
apparent_performance <- unlist( validate_model( pred_names = row.names( coefficients( ridge_mod)),
                                                pred_coefs = as.numeric( coefficients( ridge_mod)),
                                                dataset = derivation_data, # internal validation
                                                t_val = t_val,
                                                baseline_surv = baseline_surv_tval)
)

# internally validate ----
# it takes around 1 hour and 13 minutes to run this code. Alternatively, results
# can be found in ./data/internal_val.rds
bootstrap_rep <- 500
bootstrapped_estimates <- matrix( NA, nrow = bootstrap_rep, ncol = length( apparent_performance))
optimism <- matrix( NA, nrow = bootstrap_rep, ncol = length( apparent_performance))

for(i in 1:bootstrap_rep){
  # generate bootstrap data
  bootstrap_data <- derivation_data[ sample(1:nrow( derivation_data), 
                                            size = nrow( derivation_data), 
                                            replace = T), ]
  
  # derive model
  # tune lambda using cross-validation, package glmnet
  init       <- glmnet(x = as.matrix( bootstrap_data[, c("age", "bmi", "fpg", "tg")]),
                       y = Surv( bootstrap_data$time, bootstrap_data$event),
                       family="cox",
                       alpha = 0) # ridge penalty
  sequence   <- exp( seq( from = min( init$lambda), 
                          by = log( init$lambda[2]) - log( init$lambda[1]),
                          length.out = 200))
  # optimize deviance in 10-fold cross-validations
  MRidgeCV   <- cv.glmnet(x = as.matrix( bootstrap_data[, c("age", "bmi", "fpg", "tg")]),
                          y = Surv( bootstrap_data$time, bootstrap_data$event),
                          nfolds = 10,
                          type.measure = "deviance",
                          family = "cox",
                          alpha = 0, # ridge penalty
                          lambda = sequence)
  
  bootstrap_ridge_mod <- glmnet(x = as.matrix( bootstrap_data[, c("age", "bmi", "fpg", "tg")]),
                                y = Surv( bootstrap_data$time, bootstrap_data$event),
                                family="cox",
                                alpha = 0,
                                lambda = MRidgeCV$lambda.min # optimal cross-validated lambda
                                
  )
  
  bootstrapped_estimates[i, ] <- unlist( validate_model( pred_names = row.names( coefficients( bootstrap_ridge_mod)),
                                                         pred_coefs = as.numeric( coefficients( bootstrap_ridge_mod)),
                                                         dataset = bootstrap_data, 
                                                         t_val = t_val,
                                                         baseline_surv = baseline_surv_tval))
  optimism[i, ] <- bootstrapped_estimates[i, ] - unlist( apparent_performance)
  
}

colnames( bootstrapped_estimates) <- names( apparent_performance)
colnames( optimism) <- names( apparent_performance)

# calibration
point_estimates <- median( bootstrapped_estimates[, "cal_large"])

lower_bounds <- quantile( bootstrapped_estimates[, "cal_large"], 0.025) 

upper_bounds <- quantile( bootstrapped_estimates[, "cal_large"], 0.975) 

# optimism-corrected AUC(t) and IPA(t)
point_estimates <- c( point_estimates,
                      apparent_performance[ c( "AUC_t", "IPA_t")] - 
                        apply( optimism[, c( "AUC_t", "IPA_t")],
                               2,
                               mean)
)

lower_bounds <- c( lower_bounds,
                   apply( bootstrapped_estimates[, c( "AUC_t", "IPA_t")],
                          2,
                          function(x) quantile (x, 0.025)) - 
                     apply( optimism[, c( "AUC_t", "IPA_t")],
                            2,
                            mean)
)

upper_bounds <- c( upper_bounds,
                   apply( bootstrapped_estimates[, c( "AUC_t", "IPA_t")],
                          2,
                          function(x) quantile (x, 0.975)) - 
                     apply( optimism[, c( "AUC_t", "IPA_t")],
                            2,
                            mean)
)

internal_val_performance <- c( point_estimates,
                               lower_bounds,
                               upper_bounds )
names( internal_val_performance) <- c( names( apparent_performance),
                                       paste0( "ci_low_", names( apparent_performance)),
                                       paste0( "ci_up_", names( apparent_performance)))

internal_val_performance
# round( internal_val_performance, digits =2)
# saveRDS( internal_val_performance, "./data/internal_val.rds")


# 'externally' validate ----
# it takes around 1 minute to run this code. Alternatively, results
# can be found in ./data/external_val.rds
bootstrap_rep <- 500
bootstrapped_estimates <- matrix(NA, nrow = bootstrap_rep, 
                                 ncol = length( apparent_performance))
colnames( bootstrapped_estimates) <- names( apparent_performance)

for(i in 1:bootstrap_rep){
  bootstrap_data <- validation_data[ sample(1:nrow( validation_data), 
                                            size = nrow( validation_data), 
                                            replace = T), ]
  bootstrapped_estimates[i, ] <- unlist( validate_model( pred_names = row.names( coefficients( ridge_mod)),
                                                         pred_coefs = as.numeric( coefficients( ridge_mod)),
                                                         dataset = bootstrap_data,
                                                         t_val = t_val,
                                                         baseline_surv = baseline_surv_tval))
}

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
colnames( external_val_performance) <- c( names( apparent_performance),
                                          paste0( "ci_low_", names( apparent_performance)),
                                          paste0( "ci_up_", names( apparent_performance)))

external_val_performance
# round( external_val_performance, digits = 2)
# saveRDS( external_val_performance, "./data/external_val.rds")

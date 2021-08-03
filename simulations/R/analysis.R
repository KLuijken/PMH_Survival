# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Analyse data
# -----------------------------------------------------------------------------#

# Helper function for model validation
validate_model <- function( mod,
                            sim_data,
                            t_val
                            ){
  # change predictor name W to X
  validation_data <- data.frame( time_event = sim_data$time_event,
                                 event = sim_data$event,
                                 X = sim_data$W)
  
  # linear predictor
  # lp     <- if( class( mod)[1] == "coxph"){
  #   sim_data$W * mod$coefficients[1]} else{
  #     lambda_hat + sim_data$W * beta_hat }
  lp <- if( class( mod)[1] == "coxph"){
        predict( mod, 
                 newdata = validation_data,
                 type = "lp")}else{
                   -predict( mod, 
                            newdata = validation_data,
                            type = "lp")  
                 }
                   

  # evaluate IPA using riskRegression package
  IPA <- IPA( mod,
              formula = Surv( time_event, event) ~ 1,
              newdata = validation_data,
              times = t_val)$IPA[2]
  
  # evaluate time-dependent cumulative c-statistic using timeROC package
  c_stat <- unname( timeROC( T = sim_data$time_event,
                             delta = sim_data$event,
                             cause = 1,
                             marker = lp,
                             times = t_val)$AUC[2])
  
  # take marginal predicted risk
  pred_risk <- 1 - mean( pec::predictSurvProb( mod, newdata = validation_data, times = t_val))
  obs_risk <- 1 - summary( survfit( Surv( time_event, event) ~ 1, data = sim_data), times= t_val)$surv
  
  cal_large <- obs_risk / pred_risk 
  
  results <- data.frame( matrix( c( IPA,
                                    c_stat,                       
                                    cal_large),
                                 nrow = 1)
  )
  colnames(results) <- c("IPA",
                         "c_stat",
                         "cal_large")
  results$model<- ifelse( class( mod)[1] == "coxph", "mod_cox", "mod_exp")
  
  return(results)
}


# Workhorse for validation analysis

one_sim_scenario <- function( n_obs, 
                              psi, 
                              theta,
                              sigma_epsilon,
                              mod,
                              t_val){
  # generate validation/implementation data containing predictor W
  data_sets_val <- generate_data( n_obs = n_obs, 
                                  psi = psi, 
                                  theta = theta, 
                                  sigma_epsilon = sigma_epsilon)
  # store descriptives
  val_mean <- purrr:::map_df( data_sets_val, function(x) apply( x, 2, mean))
  colnames( val_mean) <- paste0( colnames( val_mean), "_mean")
  val_median <- purrr:::map_df( data_sets_val, function(x) apply( x, 2, median))
  colnames( val_median) <- paste0( colnames( val_median), "_median")
  val_sd <- purrr:::map_df( data_sets_val, function(x) apply( x, 2, sd))
  colnames( val_sd) <- paste0( colnames( val_sd), "_sd")
  
  descriptives <- cbind( val_mean, val_median, val_sd)

  # perform model validation
  results   <- purrr::pmap( list( mod, data_sets_val, t_val), validate_model)
  results   <- bind_rows( results, .id = "censoring_mechanism")
  results   <- cbind( psi, theta, sigma_epsilon, results, descriptives)
  
  return( results)
}


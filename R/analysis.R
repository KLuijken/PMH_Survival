# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Analyse data
# -----------------------------------------------------------------------------#

# Helper function: perform model validation

model_validation <- function( predictor,
                              mod,
                              mod_name,
                              sim_data,
                              t_val
                              ){
  
  # store parameters for parametric survival model
  if( class( mod)[1] != "coxph"){
    # use minus sign for aft models fitted by rms::psm()
    lambda_hat <- unname( -mod$coefficients[1])
    beta_hat   <- unname( -mod$coefficients[2]) 
  }
  
  # linear predictor at t_val
  lp     <- if( class( mod)[1] == "coxph"){
    sim_data[ , predictor] * mod$coefficients[1] - mean( sim_data[ , predictor]) * mod$coefficients[1]} else{
      lambda_hat + sim_data[ , predictor] * beta_hat - mean( sim_data[ , predictor]) * beta_hat}

  # evaluate IPA using riskRegression package
  IPA <- IPA( mod,
              formula = Surv( time_event, event) ~ 1,
              newdata = sim_data,
              times = t_val)$IPA[2]
  
  # evaluate time-dependent cumulative c-statistic using timeROC package
  c_stat <- unname( timeROC( T = sim_data$time_event,
                             delta = sim_data$event,
                             cause = 1,
                             marker = lp,
                             times = t_val)$AUC[2])
  
  # evaluate calibration using Poisson calibration model as in Crowson et al. (2016, SMMR)
  # number of events before time = t_val
  
  # create data.frame with event time trimmed after t_val (all event times > t_val
  # are substituted by t_val) and with events censored after t_val.
  data_new <- data.frame( time_event = ifelse( sim_data$time_event <= t_val,
                                               sim_data$time_event,
                                               t_val),
                          X = sim_data[ , predictor],
                          event = ifelse( sim_data$event == 1 & 
                                            sim_data$time_event <= t_val,
                                          1,
                                          0))
  
  # expected number of events at t_val based on martingale residuals
  expect <- if( class( mod)[1] == "coxph"){
    predict( mod, type = "expected", newdata = data_new)} else{
      data_new$time_event * exp( lp)} 
  expect <- ifelse( expect == 0, .0001, expect) # issues with log(0)
  expect <- log( expect)
  
  # calibration in the large coefficient
  cal_large <- glm( event ~ offset( expect),
                    data = data_new,
                    family = poisson)$coefficients[1]

  # calibration slope
  cal_slope <- glm( event ~ lp + offset( expect - lp),
                    data = data_new,
                    family = poisson)$coefficients[2]
  
  results <- data.frame( matrix( c( IPA,
                                    c_stat,                       
                                    cal_large,
                                    cal_slope),
                                 nrow = 1)
  )
  colnames(results) <- c("IPA",
                         "c_stat",
                         "cal_large",
                         "cal_slope")
  results$pred <- predictor
  results$model<- mod_name
  
  return(results)
}


# Helper function: derive and validate model

analyse_data <- function( sim_data,
                          t_val){
  # derive survreg parametric model based on X
  mod_exp   <- rms::psm( Surv( time_event, event) ~ X,
                         dist = "exponential",
                         data = sim_data,
                         x = T, # return design matrix
                         y = T # return Surv() matrix
  )
  
  results_x_exp <- model_validation( predictor = "X",
                                     mod = mod_exp,
                                     mod_name = "mod_exp",
                                     sim_data = sim_data,
                                     t_val = t_val)
  
  results_w_exp <- model_validation( predictor = "W",
                                     mod = mod_exp,
                                     mod_name = "mod_exp",
                                     sim_data = sim_data,
                                     t_val = t_val)
  
  # derive survreg Cox model based on X
  mod_cox   <- coxph( Surv( time_event, event) ~ X,
                      data = sim_data,
                      x = T,
                      y = T)
  
  results_x_cox <- model_validation( predictor = "X",
                                     mod = mod_cox,
                                     mod_name = "mod_cox",
                                     sim_data = sim_data,
                                     t_val = t_val)
  
  results_w_cox <- model_validation( predictor = "W",
                                     mod = mod_cox,
                                     mod_name = "mod_cox",
                                     sim_data = sim_data,
                                     t_val = t_val)
                         
  results <- rbind( results_x_exp,
                    results_w_exp,
                    results_x_cox,
                    results_w_cox)
  
  return(results)
}

# Workhorse for analysis

one_sim_scenario <- function( n_obs, 
                              psi, 
                              theta,
                              sigma_epsilon,
                              pmh_type = pmh_type,
                              t_val){
  data_sets <- generate_data( n_obs = n_obs, 
                              psi = psi, 
                              theta = theta, 
                              sigma_epsilon = sigma_epsilon)
  results   <- purrr::map_df( data_sets, analyse_data, t_val = t_val, .id = "censoring_mechanism")
  results   <- cbind( psi, theta, sigma_epsilon, pmh_type, results)
  
  return( results)
}


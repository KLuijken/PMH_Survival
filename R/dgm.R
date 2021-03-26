# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Define functions that generate data
# -----------------------------------------------------------------------------#


# No censoring ----
generate_data_no_cens <- function( n_obs  = 100000,
                                   psi,
                                   theta,
                                   sigma_epsilon,
                                   lambda_event = 0.1,
                                   betas_event  = log( 2)){
  psi <- (psi / 10) - 0.3
  theta <- theta / 10
  sigma_epsilon <- sigma_epsilon / 10
  
  # Generate underlying "true" X0 that is measured using procedure X and W
  X0 <- rnorm( n_obs, 0, 1.2)
  
  # Generate predictor measurement procedures X and W
  X  <- X0 + rnorm( n_obs, 0, sd = 1)
  W  <- psi + theta * X0 + rnorm( n_obs, 0, sd = sigma_epsilon)
  
  # Generate survival times from exponential distribution
  U          <- runif( n_obs)
  time_event <- -log( U) / ( lambda_event * exp( X0 * betas_event))
  event      <- rep( 1, times = n_obs)
  
  data <- data.frame( time_event, event, X, W)
  
  return(data)
}


# Administrative censoring ----
generate_data_admin_cens <- function(data_no_censoring,
                                     c_admin){
  # rename dataframe
  data_admin_censoring <- data_no_censoring
  
  # record which individuals are censored
  data_admin_censoring$event <- as.integer( data_admin_censoring$time_event < c_admin)
  
  # censored individuals get time of censoring
  data_admin_censoring$time_event[ data_admin_censoring$event == 0] <- c_admin 
  
  return(data_admin_censoring)
  
}


# Random censoring ----
generate_data_random_cens <- function( data_admin_censoring,
                                       data_no_censoring,
                                       betas_cens = log(3),
                                       lambda_cens = 0.01){
  n_obs <- nrow( data_no_censoring)
  
  # Generate censoring times based on uniform distribution
  U_cens      <- runif( n_obs)
  X_cens      <- rnorm( n_obs)
  time_cens   <- -log( U_cens) / ( lambda_cens * exp( betas_cens * X_cens))
  
  # rename dataframe
  data_random_censoring <- data_admin_censoring
  
  # record which individuals are censored randomly
  data_random_censoring$time_event <- pmin( data_admin_censoring$time_event,
                                            time_cens)
  data_random_censoring$event      <- as.integer( data_random_censoring$time_event == 
                                                    data_no_censoring$time_event)
  
  return(data_random_censoring)
}

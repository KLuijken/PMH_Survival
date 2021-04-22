# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
# April 2021
#
# Define function for data generation
# -----------------------------------------------------------------------------#


# No censoring ----
generate_data_no_cens <- function( n_obs  = 1000000,
                                   psi,
                                   theta,
                                   sigma_epsilon,
                                   lambda_event = 0.1,
                                   betas_event  = log( 2)){
  # Generate underlying "true" X0 that is measured using procedure X and W
  X0 <- rnorm( n_obs, mean = 0, sd = 1.2)
  
  # Generate predictor measurement procedures X and W
  X  <- X0 + rnorm( n_obs, mean = 0, sd = 1)
  W  <- psi + theta * X0 + rnorm( n_obs, mean = 0, sd = sigma_epsilon)
  
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
                                       lambda_cens = 0.01,
                                       betas_cens = log(3)
                                       ){
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

# Generate data depending on censoring mechanism ----

generate_data <- function( n_obs,
                           psi,
                           theta,
                           sigma_epsilon){
  no_censoring   <- generate_data_no_cens( n_obs       = n_obs,
                                         psi           = psi,
                                         theta         = theta,
                                         sigma_epsilon = sigma_epsilon)
  administrative <- generate_data_admin_cens( data_no_censoring = no_censoring,
                                              c_admin = 15)
  random         <- generate_data_random_cens( data_admin_censoring = administrative,
                                               data_no_censoring = no_censoring)
  
  data <- list( no_censoring   = no_censoring,
                administrative = administrative,
                random         = random)
  
  return( data)
}

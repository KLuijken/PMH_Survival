library(targets)
library(tarchetypes)

# Load support functions
#source( "./R/scenarios_pmh.R")
source( "./R/dgm.R")
source( "./R/analysis.R")
#source( "./R/plotting.R")

# Set target-specific options such as packages.
tar_option_set( packages = c( "tidyverse",
                              "timeROC",        # cumulative-incidence time-dependent AUC
                              "rms",
                              "scales",
                              "survival",
                              "riskRegression", # index of prediction accuracy
                              "ggplot2",
                              "gridExtra"))

# Set number of observations
n_obs <- 1000000

# Create scenarios grid using scenarios_pmh.R
### Note: the created scenarios are those as described in the main manuscript.
### To adapt simulation parameters, modify the script in scenarios_pmh.R.

scenario_grid <- expand.grid( psi = c( -0.3, 0, 0.3),
                              theta = c( 0.5, 1, 2),
                              sigma_epsilon = c( 0, 0.5*sqrt(2), sqrt(2)))

additive <- expand.grid( psi = c( -0.2, -0.1, 0.1, 0.2),
                         theta = 1,
                         sigma_epsilon = 0)
multiplicative <- expand.grid( psi = 0,
                               theta = c( 0.7, 0.85, 1.3, 1.7),
                               sigma_epsilon = 0)
random <- expand.grid( psi = 0,
                               theta = 1,
                               sigma_epsilon = c( (1/6)*sqrt(2),
                                                  (2/6)*sqrt(2),
                                                  (4/6)*sqrt(2),
                                                  (5/6)*sqrt(2)))

scenario_grid <- rbind( scenario_grid,
                        additive,
                        multiplicative,
                        random)

# Create 'static' branches for exponential and for cox model
simulation_exp <- tar_map(
  unlist = FALSE, # Important: it means object simulation_body will be a list
  values = scenario_grid, # Iterate over rows of scenario_grid
  tar_target( results_exp,
              one_sim_scenario( n_obs = n_obs,
                                psi = psi,
                                theta = theta,
                                sigma_epsilon = sigma_epsilon,
                                mod = derivation_mod_exp,
                                t_val = 6.5))
)

simulation_cox <- tar_map(
  unlist = FALSE, 
  values = scenario_grid,
  tar_target( results_cox,
              one_sim_scenario( n_obs = n_obs,
                                psi = psi,
                                theta = theta,
                                sigma_epsilon = sigma_epsilon,
                                mod = derivation_mod_cox,
                                t_val = 6.5))
)

# Build pipeline
list(
  tar_target(
    derivation_data,
    generate_data( n_obs = n_obs, 
                   psi = 0, # this argument is not actually used here
                   theta = 1, # this argument is not actually used here 
                   sigma_epsilon = 0) # this argument is not actually used here
  ),
  
  tar_target(
    derivation_mod_exp,
    purrr::map( derivation_data, .f= function(x) rms::psm( Surv( time_event, event) ~ X,
                                                         dist = "exponential",
                                                         data = x,
                                                         x = T, # return design matrix
                                                         y = T # return Surv() matrix
    )
    )
  ),
  
  simulation_exp,
 
  # Combine all scenarios
  tar_combine(
    all_scenarios_exp,
    simulation_exp,
    command = dplyr::bind_rows(!!!.x)
  ),
  
  tar_target(
    derivation_mod_cox,
    purrr::map( derivation_data, .f= function(x) coxph( Surv( time_event, event) ~ X,
                                                        data = x,
                                                        x = T,
                                                        y = T)
    )
  ),
  
  simulation_cox,
  
  # Combine all scenarios
  tar_combine(
    all_scenarios_cox,
    simulation_cox,
    command = dplyr::bind_rows(!!!.x)
  )
)
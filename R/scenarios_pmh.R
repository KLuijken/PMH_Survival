# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
# April 2021
#
# Define functions that generate scenarios of predictor measurement 
# heterogeneity (pmh)
### additive systematic pmh
### multiplicative systematic pmh
### random pmh
# Generate one scenario grid containing all types of pmh
# -----------------------------------------------------------------------------#

# Additive systematic pmh ----
generate_sys_add_scenarios <- function( psi           = seq( -0.3, 0.3, by = 0.1),
                                        theta         = c( 0.5, 1, 2),
                                        sigma_epsilon = c( 0, 1, 2)
                                       ){
  # create scenario grid
  sys_add_scenarios <- expand.grid( psi           = psi,
                                    theta         = theta,
                                    sigma_epsilon = sigma_epsilon)
  
  # remove redundant scenarios
  sys_add_scenarios <- subset( sys_add_scenarios, (!( theta == max( theta) & 
                                                        sigma_epsilon == min( sigma_epsilon)) &
                                                   !( theta == min( theta) &
                                                        sigma_epsilon == max( sigma_epsilon))))
  
  # add label describing type of predictor measurement heterogeneity
  sys_add_scenarios$pmh_type <- "additive"
  
  return( sys_add_scenarios)
}

# Multiplicative systematic pmh ----
generate_sys_mult_scenarios <- function(psi           = c( -0.3, 0, 0.3),
                                        theta         = c( 0.5, 0.7, 0.8, 1, 1.3, 1.7, 2),
                                        sigma_epsilon = c( 0, 1, 2)
                                        ){
  # create scenario grid
  sys_mult_scenarios <- expand.grid( psi          = psi,
                                    theta         = theta,
                                    sigma_epsilon = sigma_epsilon)
  
  # remove redundant scenarios
  sys_mult_scenarios <- subset( sys_mult_scenarios, (!( psi == max( psi) & 
                                                        sigma_epsilon == min( sigma_epsilon)) &
                                                     !( psi == min( psi) &
                                                        sigma_epsilon == max( sigma_epsilon))))
  
  # add label describing type of predictor measurement heterogeneity
  sys_mult_scenarios$pmh_type <- "multiplicative"
  
  return( sys_mult_scenarios)
}

# Random pmh ----
generate_random_scenarios <- function(psi           = c( -0.3, 0, 0.3),
                                      theta         = c( 0.5, 1, 2),
                                      sigma_epsilon = c( 0.1, 0.4, 0.8, 1, 1.2, 1.7, 2)
                                      ){
  # create scenario grid
  random_scenarios <- expand.grid( psi           = psi,
                                   theta         = theta,
                                   sigma_epsilon = sigma_epsilon)
  
  # remove redundant scenarios
  random_scenarios <- subset( random_scenarios, (!( psi == max( psi) & 
                                                    theta == min( theta)) &
                                                 !( psi == min( psi) &
                                                    theta == max( theta))))
  
  # add label describing type of predictor measurement heterogeneity
 random_scenarios$pmh_type <- "random"
  
  return( random_scenarios)
}


# Combined scenario grid ----

generate_sim_scenarios <- function(){
  scenario_grid <- rbind( generate_sys_add_scenarios(),
                          generate_sys_mult_scenarios(),
                          generate_random_scenarios())
  
  return( scenario_grid)
}



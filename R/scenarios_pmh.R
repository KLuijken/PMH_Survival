# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Define functions that generate scenarios of predictor measurement 
# heterogeneity (pmh)
# -----------------------------------------------------------------------------#

# Additive systematic pmh ----
generate_sys_add_scenarios <- function( psi           = seq( 0, 0.6, by = 0.1),
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
  # correction for targets naming
  sys_add_scenarios <- sys_add_scenarios *10
  
  return(sys_add_scenarios)
}

# Multiplicative systematic pmh ----
generate_sys_mult_scenarios <- function(psi           = c( 0, 0.3, 0.6),
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
  # correction for targets naming
  sys_mult_scenarios <- sys_mult_scenarios * 10
  
  return(sys_mult_scenarios)
}

# Random pmh ----
generate_random_scenarios <- function(psi           = c( 0, 0.3, 0.6),
                                      theta         = c( 0.5, 1, 2),
                                      sigma_epsilon = c(0.1, 0.4, 0.8, 1, 1.2, 1.7, 2)
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
  # correction for targets naming
  random_scenarios <- random_scenarios * 10
  
  return(random_scenarios)
}
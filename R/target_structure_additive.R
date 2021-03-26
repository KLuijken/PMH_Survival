# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Helper file structure targets additive predictor measurement heterogeneity (pmh)
# -----------------------------------------------------------------------------#

# Define simulation scenarios of additive pmh structures ----

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
  # correction for targets naming --> strange, look into way to change this
  sys_add_scenarios <- sys_add_scenarios *10
  
  return(sys_add_scenarios)
}

sim_scenarios_sys_add <- generate_sys_add_scenarios()


# Define static branches for targets ----

additive <- tar_map(
  unlist = FALSE, # Return a nested list from tar_map() for combining later
  values = tibble( generate_sys_add_scenarios()), # map over simulation scenarios
  
  # simulate data without censoring
  tar_target( add_data_no_cens, 
              generate_data_no_cens( psi = psi,
                                     theta = theta,
                                     sigma_epsilon = sigma_epsilon)),
  # analyze uncensored data
  tar_target( add_results_no_cens, 
              analyse_data( data = add_data_no_cens),
              format = "rds"),
  
  # introduce administrative censoring at t = 15
  tar_target( add_data_admin_cens, 
              generate_data_admin_cens( data_no_censoring = add_data_no_cens,
                                        c_admin = 15)),
  
  # analyze administratively censored data
  tar_target( add_results_admin_cens, 
              analyse_data( data = add_data_admin_cens),
              format = "rds"),
  
  # introduce random censoring
  tar_target( add_data_random_cens, 
              generate_data_random_cens( data_admin_censoring = add_data_admin_cens,
                                         data_no_censoring = add_data_no_cens)),
  
  # analyze randomly censored data
  tar_target( add_results_random_cens, 
              analyse_data( data = add_data_random_cens),
              format = "rds")
)

# Define combined targets (rbind all results) ----

combined_add_nocens <- tar_combine(
  combined_add_no_cens,
  additive[[2]],
  command = dplyr::bind_rows(!!!.x)
)

combined_add_censadmin <- tar_combine(
  combined_add_admin_cens,
  additive[[4]],
  command = dplyr::bind_rows(!!!.x)
)

combined_add_censrandom <- tar_combine(
  combined_add_random_cens,
  additive[[6]],
  command = dplyr::bind_rows(!!!.x)
)

# Save output ----

saved_summary_add_x_no_cens <- tar_target( analysis_summary_add_x_no_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_no_cens)[tar_read(combined_add_no_cens)$pred == "X",]
            ),
            file = "./data/analysis_summary_add_no_cens_x.rds")
)
saved_summary_add_w_no_cens <- tar_target( analysis_summary_add_w_no_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_no_cens)[tar_read(combined_add_no_cens)$pred == "W",]
            ),
            file = "./data/analysis_summary_add_no_cens_w.rds")
)

saved_summary_add_x_admin_cens <- tar_target( analysis_summary_add_x_admin_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_admin_cens)[tar_read(combined_add_admin_cens)$pred == "X",]
            ),
            file = "./data/analysis_summary_add_admin_cens_x.rds")
)
saved_summary_add_w_admin_cens <- tar_target( analysis_summary_add_w_admin_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_admin_cens)[tar_read(combined_add_admin_cens)$pred == "W",]
            ),
            file = "./data/analysis_summary_add_admin_cens_w.rds")
)
saved_summary_add_x_random_cens <- tar_target( analysis_summary_add_x_random_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_random_cens)[tar_read(combined_add_random_cens)$pred == "X",]
            ),
            file = "./data/analysis_summary_add_random_cens_x.rds")
)

saved_summary_add_w_random_cens <- tar_target( analysis_summary_add_w_random_cens, 
            saveRDS( cbind( sim_scenarios_sys_add,
                            tar_read(combined_add_random_cens)[tar_read(combined_add_random_cens)$pred == "W",]
            ),
            file = "./data/analysis_summary_add_random_cens_w.rds")
)

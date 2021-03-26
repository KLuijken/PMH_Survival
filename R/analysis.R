# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Analyse data
# -----------------------------------------------------------------------------#

# Helper function

model_validation <- function( predictor,
                              mod,
                              data,
                              t_val){
  # evaluate IPA using riskRegression package
  IPA <- IPA( mod,
              formula = Surv( time_event, event) ~ 1,
              newdata = data,
              times = t_val)$IPA[2]
  
  # evaluate time-dependent cumulative c-statistic using timeROC package
  c_stat <- unname( timeROC( T = data$time_event,
                             delta = data$event,
                             cause = 1,
                             marker = data[ , predictor],
                             times = t_val)$AUC[2])
  
  # evaluate calibration using Poisson calibration model as in Crowson et al. (2016, SMMR)
  # Number of events before time = t_val
  # Observed
  event_t <- as.numeric( data$time_event <= t_val)
  
  # Expected
  ### create data.frame with event time trimmed after t_val (all event times > t_val
  ### are substituted by t_val) and with events censored after t_val.
  data_new <- data.frame( time_event = ifelse( data$time_event <= t_val,
                                               data$time_event,
                                               t_val),
                          X = data[, predictor],
                          event = as.numeric( data$time_event <= t_val))
  
  # # expected number of events at t_val based on martingale residuals
  # martingale_expect <- lambda_hat * data_new$time_event * exp( data_new$X * beta_hat)
  # lp <- data_new$X * beta_hat
  expect <- predict( mod, type = "expected", newdata = data_new)
  expect <- ifelse( expect == 0, .0001, expect) # issues with log(0)
  lp     <- predict( mod, type = "lp", newdata = data_new)
  
  # calibration in the large coefficient
  cal_large <- glm( event_t ~ offset( log( expect)),
                    family = poisson,
                    data = data_new)$coefficients[1]
  
  # calibration slope
  cal_slope <- glm( event_t ~ lp + offset( log( expect) - lp),
                    family = poisson,
                    data = data_new)$coefficients[2]
  
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
  
  return(results)
}


# Workhorse for analysis

analyse_data <- function( data){
  t_val <- median( data$time_event)
  
  # # derive survreg model based on X
  # mod <- rms::psm( Surv( time_event, event) ~ X,
  #                  dist = "exponential",
  #                  data = data,
  #                  x = T, # return design matrix
  #                  y = T # return Surv() matrix
  # )
  # 
  # # Use minus sign for aft models
  # lambda_hat <- unname( exp( -mod$coefficients[1]))
  # beta_hat   <- unname( -mod$coefficients[2])
  
  # derive survreg model based on X
  mod <- coxph( Surv( time_event, event) ~ X,
                data = data,
                x = T,
                y = T)
  
  results_x <- model_validation( predictor = "X",
                                 mod = mod,
                                 data = data,
                                 t_val = t_val)
  
  results_w <- model_validation( predictor = "W",
                                 mod = mod,
                                 data = data,
                                 t_val = t_val)
                         
  results <- rbind( results_x,
                    results_w)
  
  return(results)
}

# -----------------------------------------------------------------------------#
# Predictor measurement heterogeneity in survival regression prediction models
# Author: K Luijken
#
# Add-on: clean data for motivating example on incident diabetes
# -----------------------------------------------------------------------------#
# Load data ----
diabetes_data_raw <- readxl::read_excel( 
  here::here( "data/Dataset-Int-J-Obesity.xlsx")) # used here() to source in .Rmd
#### Data set available from: https://doi.org/10.5061/dryad.8q0p192

# Select relevant variables from data set
diabetes_data <- data.frame( 
  cbind(diabetes_data_raw$`Baseline Age, yrs`,
        diabetes_data_raw$`Baseline BMI, kg/m2`,
        diabetes_data_raw$`Baseline Triglycerides, mmol/L`,
        diabetes_data_raw$`Baseline Fasting plasma glucose, mmol/L`,
        diabetes_data_raw$`Follow up duration, days`,
        diabetes_data_raw$`Incident DM 0/1`)
)
colnames(diabetes_data) <- c("age",
                             "bmi",
                             "tg",
                             "fpg",
                             "time",
                             "event") # change to event (incident diabetes in comment)
# clean data ----
table( diabetes_data$event) # 373 events

# censor data after 6 years
t_val <- floor( 6 * 365.25)
diabetes_data$event[ diabetes_data$time > (t_val + 1)] <- 0
diabetes_data$time[ diabetes_data$time > (t_val + 1)] <- (t_val + 1)

# decimals of fpg corresponding to other studies
diabetes_data$fpg <- round( diabetes_data$fpg, digits = 1) 

# tg > 10 implausible value, exclude 1 individual
diabetes_data <- diabetes_data[ diabetes_data$tg < 10, ] 


# Resample data ----
set.seed(721) # set seed based on date (July 2021)

# create derivation data set (70% of observations, n = 10,824)
index_events_der <- sample( 1:nrow(diabetes_data[ diabetes_data$event == 1,]),
                            size = round( 0.7 * nrow(diabetes_data[ diabetes_data$event == 1,])),
                            replace = F)
events_der <- subset( diabetes_data, event == 1)[ index_events_der,]

index_non_events_der <- sample( 1:nrow(diabetes_data[ diabetes_data$event == 0,]),
                                size = round( 0.7 * nrow(diabetes_data[ diabetes_data$event == 0,])),
                                replace = F)
non_events_der <- subset( diabetes_data, event == 0)[ index_non_events_der,]

derivation_data <- rbind( events_der, non_events_der)

# create validation data set (30% of observations, n = 4,639)
index_events_val <- sample( 1:nrow(diabetes_data[ diabetes_data$event == 1,]),
                            size = round( 0.3 * nrow(diabetes_data[ diabetes_data$event == 1,])),
                            replace = F)
events_val <- subset( diabetes_data, event == 1)[ index_events_val,]

index_non_events_val <- sample( 1:nrow(diabetes_data[ diabetes_data$event == 0,]),
                                size = round( 0.3 * nrow(diabetes_data[ diabetes_data$event == 0,])),
                                replace = F)
non_events_val <- subset( diabetes_data, event == 0)[ index_non_events_val,]

validation_data <- rbind( events_val, non_events_val)


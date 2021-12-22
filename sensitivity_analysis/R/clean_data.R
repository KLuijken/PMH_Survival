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

diabetes_data <- data.frame( diabetes_data)

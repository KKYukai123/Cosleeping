# Load the data
setwd("D:/study/Dissertation/data")
FC=read.csv("Feeding&Cosleeping.csv")

## Print the first few rows
head(FC)
dim(FC)

# Initialise feeding_method column to NA
FC$feeding_method <- NA

# Tagged Exclusive_Breastfeeding
FC$feeding_method[FC$what_eat_drink_breast_milk == 1 & 
                    is.na(FC$what_eat_drink_baby_formula) & 
                    is.na(FC$what_eat_drink_cows_milk) &
                    is.na(FC$what_eat_drink_pureed_foods) &
                    is.na(FC$what_eat_drink_solid_foods) &
                    is.na(FC$what_eat_drink_baby_cereals)] <- "Exclusive_Breastfeeding"

# Tagged Partial_Breastfeeding
FC$feeding_method[FC$what_eat_drink_breast_milk == 1 & (
  !is.na(FC$what_eat_drink_baby_formula) | 
    !is.na(FC$what_eat_drink_cows_milk) |
    !is.na(FC$what_eat_drink_pureed_foods) |
    !is.na(FC$what_eat_drink_solid_foods) |
    !is.na(FC$what_eat_drink_baby_cereals))] <- "Partial_Breastfeeding"

# Tagged Non_Breastfeeding
FC$feeding_method[is.na(FC$what_eat_drink_breast_milk)] <- "Non_Breastfeeding"

# Convert time string to total minutes
FC$sleeps_longest_stretch_time <- sapply(strsplit(as.character(FC$sleeps_longest_stretch_time), ":"), function(x) {
  hours <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
})


FC$baby_sleep_time <- sapply(strsplit(as.character(FC$baby_sleep_time), ":"), function(x) {
  hours <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
})

FC$baby_wake_time <- sapply(strsplit(as.character(FC$baby_wake_time), ":"), function(x) {
  hours <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
})

FC$baby_total_sleep_time <- sapply(strsplit(as.character(FC$baby_total_sleep_time), ":"), function(x) {
  hours <- as.numeric(x[1])
  minutes <- as.numeric(x[2])
  total_minutes <- hours * 60 + minutes
  return(total_minutes)
})

# glm modelling
glm_model_longest_stretch <- glm(sleeps_longest_stretch_time ~ feeding_method + where_sleep_longest + Age_in_months_at_reg, 
                 data = FC, 
                 family = poisson)
glm_model_sleep_time <- glm(baby_sleep_time ~ feeding_method + where_sleep_longest + Age_in_months_at_reg, 
                            data = FC, 
                            family = poisson)

glm_model_wake_time <- glm(baby_wake_time ~ feeding_method + where_sleep_longest + Age_in_months_at_reg, 
                           data = FC, 
                           family = poisson)

glm_model_total_sleep_time <- glm(baby_total_sleep_time ~ feeding_method + where_sleep_longest + Age_in_months_at_reg, 
                                  data = FC, 
                                  family = poisson)

glm_model_past_week_woken_up <- glm(past_week_average_woken_up ~ feeding_method + where_sleep_longest + Age_in_months_at_reg, 
                                    data = FC, 
                                    family = poisson)
# Show results
summary(glm_model_longest_stretch)
summary(glm_model_sleep_time)
summary(glm_model_wake_time)
summary(glm_model_total_sleep_time)
summary(glm_model_past_week_woken_up)


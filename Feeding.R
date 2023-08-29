# Load the data
setwd("D:/study/Dissertation/data")
Feeding=read.csv("Feeding.csv")

## Print the first few rows
head(Feeding)
dim(Feeding)

library(dplyr)
# Convert related columns to POSIXct time format
Feeding$sleeps_longest_stretch_time <- as.POSIXct(Feeding$sleeps_longest_stretch_time, format = "%H:%M")
Feeding$baby_sleep_time <- as.POSIXct(Feeding$baby_sleep_time, format = "%H:%M")
Feeding$baby_wake_time <- as.POSIXct(Feeding$baby_wake_time, format = "%H:%M")
Feeding$baby_total_sleep_time <- as.POSIXct(Feeding$baby_total_sleep_time, format = "%H:%M")

# Filtering of breastfed babies
filtered_data <- Feeding %>%
  filter(what_eat_drink_breast_milk == 1 | 
           is.na(what_eat_drink_breast_milk),
         is.na(what_eat_drink_baby_formula),
         is.na(what_eat_drink_cows_milk),
         is.na(what_eat_drink_pureed_foods),
         is.na(what_eat_drink_solid_foods),
         is.na(what_eat_drink_baby_cereals))

# Calculate the average times for the specified columns
average_sleeps_longest_stretch_time <- as.POSIXct(mean(as.numeric(filtered_data$sleeps_longest_stretch_time), na.rm = TRUE), origin = "1970-01-01")
average_baby_sleep_time <- as.POSIXct(mean(as.numeric(filtered_data$baby_sleep_time), na.rm = TRUE), origin = "1970-01-01")
average_baby_wake_time <- as.POSIXct(mean(as.numeric(filtered_data$baby_wake_time), na.rm = TRUE), origin = "1970-01-01")
average_baby_total_sleep_time <- as.POSIXct(mean(as.numeric(filtered_data$baby_total_sleep_time), na.rm = TRUE), origin = "1970-01-01")

# Output the results
print(format(average_sleeps_longest_stretch_time, format = "%H:%M"))
print(format(average_baby_sleep_time, format = "%H:%M"))
print(format(average_baby_wake_time, format = "%H:%M"))
print(format(average_baby_total_sleep_time, format = "%H:%M"))

# Calculate the average for the 'past_week_average_woken_up' column
average_past_week_average_woken_up <- mean(filtered_data$past_week_average_woken_up, na.rm = TRUE)

# Output the results
print(average_past_week_average_woken_up)


# Convert the time format columns to total minutes for 'filtered_data'
filtered_data$sleeps_longest_stretch_time <- as.numeric(format(filtered_data$sleeps_longest_stretch_time, "%H")) * 60 +
  as.numeric(format(filtered_data$sleeps_longest_stretch_time, "%M"))

filtered_data$baby_sleep_time <- as.numeric(format(filtered_data$baby_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data$baby_sleep_time, "%M"))

filtered_data$baby_wake_time <- as.numeric(format(filtered_data$baby_wake_time, "%H")) * 60 +
  as.numeric(format(filtered_data$baby_wake_time, "%M"))

filtered_data$baby_total_sleep_time <- as.numeric(format(filtered_data$baby_total_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data$baby_total_sleep_time, "%M"))

# Shapiro-Wilk test for normality
columns_to_test <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

results <- list()
for (col in columns_to_test) {
  results[[col]] <- shapiro.test(filtered_data[[col]])
}

# Print results
results



# Filtering of non-breastfed infants
filtered_data2 <- Feeding %>%
  filter(is.na(what_eat_drink_breast_milk))

# Calculate the average times for the specified columns
average_sleeps_longest_stretch_time <- mean(as.numeric(filtered_data2$sleeps_longest_stretch_time), na.rm = TRUE)
average_baby_sleep_time <- mean(as.numeric(filtered_data2$baby_sleep_time), na.rm = TRUE)
average_baby_wake_time <- mean(as.numeric(filtered_data2$baby_wake_time), na.rm = TRUE)
average_baby_total_sleep_time <- mean(as.numeric(filtered_data2$baby_total_sleep_time), na.rm = TRUE)

# Convert the average times to time format
average_sleeps_longest_stretch_time <- format(as.POSIXct(average_sleeps_longest_stretch_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_sleep_time <- format(as.POSIXct(average_baby_sleep_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_wake_time <- format(as.POSIXct(average_baby_wake_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_total_sleep_time <- format(as.POSIXct(average_baby_total_sleep_time, origin = "1970-01-01"), format = "%H:%M")

# Output the results
print(average_sleeps_longest_stretch_time)
print(average_baby_sleep_time)
print(average_baby_wake_time)
print(average_baby_total_sleep_time)

# Calculate the average for the 'past_week_average_woken_up' column
average_past_week_average_woken_up <- mean(filtered_data2$past_week_average_woken_up, na.rm = TRUE)

# Output the results
print(average_past_week_average_woken_up)

# Convert the time format columns to total minutes for 'filtered_data2'
filtered_data2$sleeps_longest_stretch_time <- as.numeric(format(filtered_data2$sleeps_longest_stretch_time, "%H")) * 60 +
  as.numeric(format(filtered_data2$sleeps_longest_stretch_time, "%M"))

filtered_data2$baby_sleep_time <- as.numeric(format(filtered_data2$baby_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data2$baby_sleep_time, "%M"))

filtered_data2$baby_wake_time <- as.numeric(format(filtered_data2$baby_wake_time, "%H")) * 60 +
  as.numeric(format(filtered_data2$baby_wake_time, "%M"))

filtered_data2$baby_total_sleep_time <- as.numeric(format(filtered_data2$baby_total_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data2$baby_total_sleep_time, "%M"))

# Shapiro-Wilk test for normality
columns_to_test <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

results <- list()
for (col in columns_to_test) {
  results[[col]] <- shapiro.test(filtered_data2[[col]])
}

# Print results
results

# Filtering of partially breastfed infants
filtered_data3 <- Feeding %>%
  filter(what_eat_drink_breast_milk == 1,
         (!is.na(what_eat_drink_baby_formula) | 
            !is.na(what_eat_drink_cows_milk) | 
            !is.na(what_eat_drink_pureed_foods) | 
            !is.na(what_eat_drink_solid_foods) | 
            !is.na(what_eat_drink_baby_cereals)))

# Convert the time format columns to total minutes for 'filtered_data3'
filtered_data3$sleeps_longest_stretch_time <- as.numeric(format(filtered_data3$sleeps_longest_stretch_time, "%H")) * 60 +
  as.numeric(format(filtered_data3$sleeps_longest_stretch_time, "%M"))

filtered_data3$baby_sleep_time <- as.numeric(format(filtered_data3$baby_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data3$baby_sleep_time, "%M"))

filtered_data3$baby_wake_time <- as.numeric(format(filtered_data3$baby_wake_time, "%H")) * 60 +
  as.numeric(format(filtered_data3$baby_wake_time, "%M"))

filtered_data3$baby_total_sleep_time <- as.numeric(format(filtered_data3$baby_total_sleep_time, "%H")) * 60 +
  as.numeric(format(filtered_data3$baby_total_sleep_time, "%M"))

# Shapiro-Wilk test for normality
columns_to_test <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

results <- list()
for (col in columns_to_test) {
  results[[col]] <- shapiro.test(filtered_data3[[col]])
}

# Print results
results

# Load required libraries
install.packages("ggplot2")
library(ggplot2)

# Combine the three filtered datasets and add a column indicating the feeding methods
filtered_data$feeding_type <- "Exelusively breastfed"
filtered_data2$feeding_type <- "Non-breastfed"
filtered_data3$feeding_type <- "Partially breastfed"

# Combine the datasets
combined_data <- rbind(filtered_data, filtered_data2, filtered_data3)

# Generate boxplots for the four sleep variables

plot_vars <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time")

# Generate boxplots for the four sleep variables
plot_list <- lapply(plot_vars, function(var) {
  ggplot(combined_data, aes(x = feeding_type, y = as.numeric(difftime(get(var), as.POSIXct("00:00:00", format="%H:%M"), units="secs")))) + 
    geom_boxplot() + 
    labs(y = var, title = paste(var)) + 
    theme_minimal() + 
    scale_y_continuous(labels = function(x) sprintf('%02d:%02d', x %/% 3600, (x %% 3600) %/% 60))
})

# To view the plots one by one
print(plot_list[[1]]) 
print(plot_list[[2]])
print(plot_list[[3]])
print(plot_list[[4]])

plot_list2 <- lapply("past_week_average_woken_up", function(var) {
  ggplot(combined_data, aes(x = feeding_type, y = as.numeric(get(var)))) + 
    geom_boxplot() + 
    labs(y = var, title = paste(var)) + 
    theme_minimal()
})

print(plot_list2)

# Calculate the average times for the specified columns
average_sleeps_longest_stretch_time <- mean(as.numeric(filtered_data3$sleeps_longest_stretch_time), na.rm = TRUE)
average_baby_sleep_time <- mean(as.numeric(filtered_data3$baby_sleep_time), na.rm = TRUE)
average_baby_wake_time <- mean(as.numeric(filtered_data3$baby_wake_time), na.rm = TRUE)
average_baby_total_sleep_time <- mean(as.numeric(filtered_data3$baby_total_sleep_time), na.rm = TRUE)

# Convert the average times to time format
average_sleeps_longest_stretch_time <- format(as.POSIXct(average_sleeps_longest_stretch_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_sleep_time <- format(as.POSIXct(average_baby_sleep_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_wake_time <- format(as.POSIXct(average_baby_wake_time, origin = "1970-01-01"), format = "%H:%M")
average_baby_total_sleep_time <- format(as.POSIXct(average_baby_total_sleep_time, origin = "1970-01-01"), format = "%H:%M")

# Calculate the average for the 'past_week_average_woken_up' column
average_past_week_average_woken_up <- mean(filtered_data3$past_week_average_woken_up, na.rm = TRUE)

# Output the results
print(average_sleeps_longest_stretch_time)
print(average_baby_sleep_time)
print(average_baby_wake_time)
print(average_baby_total_sleep_time)
print(average_past_week_average_woken_up)

# Perform Kruskal-Wallis test for sleeps_longest_stretch_time
test1_result <- kruskal.test(list(filtered_data$sleeps_longest_stretch_time,
                                 filtered_data2$sleeps_longest_stretch_time,
                                 filtered_data3$sleeps_longest_stretch_time))
# Perform Kruskal-Wallis test for baby_sleep_time
test2_result <- kruskal.test(list(filtered_data$baby_sleep_time,
                                  filtered_data2$baby_sleep_time,
                                  filtered_data3$baby_sleep_time))
# Perform Kruskal-Wallis test for baby_wake_time
test3_result <- kruskal.test(list(filtered_data$baby_wake_time,
                                  filtered_data2$baby_wake_time,
                                  filtered_data3$baby_wake_time))
# Perform Kruskal-Wallis test for baby_total_sleep_time
test4_result <- kruskal.test(list(filtered_data$baby_total_sleep_time,
                                  filtered_data2$baby_total_sleep_time,
                                  filtered_data3$baby_total_sleep_time))
# Perform Kruskal-Wallis test for past_week_average_woken_up
test5_result <- kruskal.test(list(filtered_data$past_week_average_woken_up,
                                  filtered_data2$past_week_average_woken_up,
                                  filtered_data3$past_week_average_woken_up))

# Print the test result
print(test1_result)
print(test2_result)
print(test3_result)
print(test4_result)
print(test5_result)

# Functions for performing post-hoc
perform_posthoc <- function(combined_data, grouping_variable) {
  posthoc_result <- pairwise.wilcox.test(combined_data, grouping_variable, p.adjust.method = "bonferroni")
  print(posthoc_result)
}

# 1. For sleeps_longest_stretch_time
combined_data1 <- c(filtered_data$sleeps_longest_stretch_time,
                    filtered_data2$sleeps_longest_stretch_time,
                    filtered_data3$sleeps_longest_stretch_time)
grouping_variable1 <- c(rep("Group1", length(filtered_data$sleeps_longest_stretch_time)),
                        rep("Group2", length(filtered_data2$sleeps_longest_stretch_time)),
                        rep("Group3", length(filtered_data3$sleeps_longest_stretch_time)))

print("Post-hoc for test1_result:")
perform_posthoc(combined_data1, grouping_variable1)

# 2. For baby_sleep_time
combined_data2 <- c(filtered_data$baby_sleep_time,
                    filtered_data2$baby_sleep_time,
                    filtered_data3$baby_sleep_time)
grouping_variable2 <- c(rep("Group1", length(filtered_data$baby_sleep_time)),
                        rep("Group2", length(filtered_data2$baby_sleep_time)),
                        rep("Group3", length(filtered_data3$baby_sleep_time)))

print("Post-hoc for test2_result:")
perform_posthoc(combined_data2, grouping_variable2)

# 3. For baby_wake_time
combined_data3 <- c(filtered_data$baby_wake_time,
                    filtered_data2$baby_wake_time,
                    filtered_data3$baby_wake_time)
grouping_variable3 <- c(rep("Group1", length(filtered_data$baby_wake_time)),
                        rep("Group2", length(filtered_data2$baby_wake_time)),
                        rep("Group3", length(filtered_data3$baby_wake_time)))

print("Post-hoc for test3_result:")
perform_posthoc(combined_data3, grouping_variable3)

# 4. For baby_total_sleep_time
combined_data4 <- c(filtered_data$baby_total_sleep_time,
                    filtered_data2$baby_total_sleep_time,
                    filtered_data3$baby_total_sleep_time)
grouping_variable4 <- c(rep("Group1", length(filtered_data$baby_total_sleep_time)),
                        rep("Group2", length(filtered_data2$baby_total_sleep_time)),
                        rep("Group3", length(filtered_data3$baby_total_sleep_time)))

print("Post-hoc for test4_result:")
perform_posthoc(combined_data4, grouping_variable4)

# 5. For past_week_average_woken_up
combined_data5 <- c(filtered_data$past_week_average_woken_up,
                    filtered_data2$past_week_average_woken_up,
                    filtered_data3$past_week_average_woken_up)
grouping_variable5 <- c(rep("Group1", length(filtered_data$past_week_average_woken_up)),
                        rep("Group2", length(filtered_data2$past_week_average_woken_up)),
                        rep("Group3", length(filtered_data3$past_week_average_woken_up)))

print("Post-hoc for test5_result:")
perform_posthoc(combined_data5, grouping_variable5)



# Create a new variable for age groups based on Age_in_months_at_reg column
Feeding$Age_group <- cut(Feeding$Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)

# Filtering of breastfed babies
filtered_data4 <- Feeding %>%
  filter((what_eat_drink_breast_milk == 1 | is.na(what_eat_drink_breast_milk)) &
           is.na(what_eat_drink_baby_formula) &
           is.na(what_eat_drink_cows_milk) &
           is.na(what_eat_drink_pureed_foods) &
           is.na(what_eat_drink_solid_foods) &
           is.na(what_eat_drink_baby_cereals))

# Calculate the count of infants for each age group
infant_count_by_age_group <- filtered_data4 %>%
  group_by(Age_group) %>%
  summarize(Count = n())

# Print the results
print(infant_count_by_age_group)

# Filtering of non-breastfed infants
filtered_data5 <- Feeding %>%
  filter(is.na(what_eat_drink_breast_milk))

# Calculate the count of infants for each age group
infant_count_by_age_group2 <- filtered_data5 %>%
  group_by(Age_group) %>%
  summarize(Count = n())

# Print the results
print(infant_count_by_age_group2)

# Filter the data based on the conditions
filtered_data6 <- Feeding %>%
  filter(what_eat_drink_breast_milk == 1 &
           (!is.na(what_eat_drink_baby_formula) |
              !is.na(what_eat_drink_cows_milk) |
              !is.na(what_eat_drink_pureed_foods) |
              !is.na(what_eat_drink_solid_foods) |
              !is.na(what_eat_drink_baby_cereals)))

# Calculate the count of infants for each age group
infant_count_by_age_group3 <- filtered_data6 %>%
  group_by(Age_group) %>%
  summarize(Count = n())

# Print the results
print(infant_count_by_age_group3)

install.packages("gridExtra")
library(ggplot2)
library(dplyr)


# Add feeding type column to each data frame
infant_count_by_age_group$Feeding_type <- "Exelusively breastfed"
infant_count_by_age_group2$Feeding_type <- "Non-breastfed"
infant_count_by_age_group3$Feeding_type <- "Partially breastfed"

all_data <- rbind(infant_count_by_age_group, infant_count_by_age_group2, infant_count_by_age_group3)
all_data <- all_data %>%
  group_by(Age_group) %>%
  mutate(Percentage = Count / sum(Count))

label_percentage <- function(variable){
  return(scales::percent(variable, accuracy = 1))
}

plot_pie_chart <- function(data, age_group_label) {
  ggplot(data %>% filter(Age_group == age_group_label), 
         aes(x = "", y = Percentage, fill = Feeding_type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label_percentage(Percentage)), position = position_stack(vjust = 0.5)) +
    labs(title = paste("Distribution for", age_group_label), fill = "Feeding Type")
}
age_groups <- unique(all_data$Age_group)

# Plot pie charts for each age group
lapply(age_groups, function(age_group) {
  plot_pie_chart(all_data, age_group)
})

# Create a new variable for age groups based on Age_in_months_at_reg column
Feeding$Age_group <- cut(Feeding$Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)

# Filter the data based on the conditions
filtered_data7 <- Feeding %>%
  filter(what_eat_drink_breast_milk == 1 &
           is.na(what_eat_drink_baby_formula) &
           is.na(what_eat_drink_cows_milk) &
           is.na(what_eat_drink_pureed_foods) &
           is.na(what_eat_drink_solid_foods) &
           is.na(what_eat_drink_baby_cereals))

# Calculate the average values for each age group
average_values_by_age_group <- filtered_data7 %>%
  group_by(Age_group) %>%
  summarize(avg_sleeps_longest_stretch_time = mean(as.numeric(sleeps_longest_stretch_time), na.rm = TRUE),
            avg_baby_sleep_time = mean(as.numeric(baby_sleep_time), na.rm = TRUE),
            avg_baby_wake_time = mean(as.numeric(baby_wake_time), na.rm = TRUE),
            avg_baby_total_sleep_time = mean(as.numeric(baby_total_sleep_time), na.rm = TRUE),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))

# Convert the average times to time format
average_values_by_age_group$avg_sleeps_longest_stretch_time <- format(as.POSIXct(average_values_by_age_group$avg_sleeps_longest_stretch_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_sleep_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_wake_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_wake_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_total_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_total_sleep_time, origin = "1970-01-01"), format = "%H:%M")

# Print the results
print(average_values_by_age_group)

# Exporting data to a file
View(average_values_by_age_group)
write.csv(average_values_by_age_group, file = "average_values_by_age_group1.csv", row.names = FALSE)
file.show("average_values_by_age_group1.csv")

# Create a new variable for age groups based on Age_in_months_at_reg column
Feeding$Age_group <- cut(Feeding$Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)

# Filter the data based on the conditions
filtered_data8 <- Feeding %>%
  filter(is.na(what_eat_drink_breast_milk))

# Calculate the average values for each age group
average_values_by_age_group <- filtered_data8 %>%
  group_by(Age_group) %>%
  summarize(avg_sleeps_longest_stretch_time = mean(as.numeric(sleeps_longest_stretch_time), na.rm = TRUE),
            avg_baby_sleep_time = mean(as.numeric(baby_sleep_time), na.rm = TRUE),
            avg_baby_wake_time = mean(as.numeric(baby_wake_time), na.rm = TRUE),
            avg_baby_total_sleep_time = mean(as.numeric(baby_total_sleep_time), na.rm = TRUE),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))


# Convert the average times to time format
average_values_by_age_group$avg_sleeps_longest_stretch_time <- format(as.POSIXct(average_values_by_age_group$avg_sleeps_longest_stretch_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_sleep_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_wake_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_wake_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_total_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_total_sleep_time, origin = "1970-01-01"), format = "%H:%M")

# Print the results
print(average_values_by_age_group)

# Exporting data to a file
View(average_values_by_age_group)
write.csv(average_values_by_age_group, file = "average_values_by_age_group2.csv", row.names = FALSE)
file.show("average_values_by_age_group2.csv")

# Create a new variable for age groups based on Age_in_months_at_reg column
Feeding$Age_group <- cut(Feeding$Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)

# Filter the data based on the conditions
filtered_data9 <- Feeding %>%
  filter(what_eat_drink_breast_milk == 1 &
           (!is.na(what_eat_drink_baby_formula) |
              !is.na(what_eat_drink_cows_milk) |
              !is.na(what_eat_drink_pureed_foods) |
              !is.na(what_eat_drink_solid_foods) |
              !is.na(what_eat_drink_baby_cereals)))

# Calculate the average values for each age group
average_values_by_age_group <- filtered_data9 %>%
  group_by(Age_group) %>%
  summarize(avg_sleeps_longest_stretch_time = mean(as.numeric(sleeps_longest_stretch_time), na.rm = TRUE),
            avg_baby_sleep_time = mean(as.numeric(baby_sleep_time), na.rm = TRUE),
            avg_baby_wake_time = mean(as.numeric(baby_wake_time), na.rm = TRUE),
            avg_baby_total_sleep_time = mean(as.numeric(baby_total_sleep_time), na.rm = TRUE),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))

# Convert the average times to time format
average_values_by_age_group$avg_sleeps_longest_stretch_time <- format(as.POSIXct(average_values_by_age_group$avg_sleeps_longest_stretch_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_sleep_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_wake_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_wake_time, origin = "1970-01-01"), format = "%H:%M")
average_values_by_age_group$avg_baby_total_sleep_time <- format(as.POSIXct(average_values_by_age_group$avg_baby_total_sleep_time, origin = "1970-01-01"), format = "%H:%M")

# Print the results
print(average_values_by_age_group)

# Exporting data to a file
View(average_values_by_age_group)
write.csv(average_values_by_age_group, file = "average_values_by_age_group3.csv", row.names = FALSE)
file.show("average_values_by_age_group3.csv")

# Filter data for 0-2 months and 3-6 months age groups
filtered_data7_age0_2 <- filtered_data7 %>% filter(Age_group == "0-2 months")
filtered_data8_age0_2 <- filtered_data8 %>% filter(Age_group == "0-2 months")
filtered_data9_age0_2 <- filtered_data9 %>% filter(Age_group == "0-2 months")

filtered_data7_age3_6 <- filtered_data7 %>% filter(Age_group == "3-6 months")
filtered_data8_age3_6 <- filtered_data8 %>% filter(Age_group == "3-6 months")
filtered_data9_age3_6 <- filtered_data9 %>% filter(Age_group == "3-6 months")

# Perform Kruskal-Wallis tests for 0-2 months age group
kruskal_0_2 <- lapply(c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up"),
                      function(var) {
                        kruskal.test(list(as.numeric(filtered_data7_age0_2[[var]]),
                                          as.numeric(filtered_data8_age0_2[[var]]),
                                          as.numeric(filtered_data9_age0_2[[var]])))
                      })

# Perform Kruskal-Wallis tests for 3-6 months age group
kruskal_3_6 <- lapply(c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up"),
                      function(var) {
                        kruskal.test(list(as.numeric(filtered_data7_age3_6[[var]]),
                                          as.numeric(filtered_data8_age3_6[[var]]),
                                          as.numeric(filtered_data9_age3_6[[var]])))
                      })

# Print the results
print("Kruskal-Wallis test results for 0-2 months age group:")
print(kruskal_0_2)

print("Kruskal-Wallis test results for 3-6 months age group:")
print(kruskal_3_6)

# Function to perform pairwise Wilcoxon test for a given variable and data
perform_pairwise_wilcoxon <- function(data1, data2, data3, var_name) {
  data1_filtered <- na.omit(as.numeric(data1[[var_name]]))
  data2_filtered <- na.omit(as.numeric(data2[[var_name]]))
  data3_filtered <- na.omit(as.numeric(data3[[var_name]]))
  
  combined_data <- c(data1_filtered, data2_filtered, data3_filtered)
  group_labels <- c(rep("data1", length(data1_filtered)),
                    rep("data2", length(data2_filtered)),
                    rep("data3", length(data3_filtered)))
  
  return(pairwise.wilcox.test(combined_data, group_labels, p.adjust.method = "bonferroni"))
}

# Variables to test
vars_to_test <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

# Perform post-hoc tests for 0-2 months age group
post_hoc_0_2 <- lapply(1:length(kruskal_0_2), function(i) {
  if (kruskal_0_2[[i]]$p.value < 0.05) {
    perform_pairwise_wilcoxon(filtered_data7_age0_2, filtered_data8_age0_2, filtered_data9_age0_2, vars_to_test[i])
  } else {
    return(NULL)
  }
})

# Perform post-hoc tests for 3-6 months age group
post_hoc_3_6 <- lapply(1:length(kruskal_3_6), function(i) {
  if (kruskal_3_6[[i]]$p.value < 0.05) {
    perform_pairwise_wilcoxon(filtered_data7_age3_6, filtered_data8_age3_6, filtered_data9_age3_6, vars_to_test[i])
  } else {
    return(NULL)
  }
})

# Print the results
print("Post-hoc pairwise Wilcoxon test results for 0-2 months age group:")
print(post_hoc_0_2)

print("Post-hoc pairwise Wilcoxon test results for 3-6 months age group:")
print(post_hoc_3_6)



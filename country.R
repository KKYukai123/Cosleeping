# Load the data
setwd("D:/study/Dissertation/data")
Country=read.csv("country.csv")

## Print the first few rows
head(Country)
dim(Country)

# Average of infant sleep time according to national calculation
install.packages("dplyr")
install.packages("hms")
library(dplyr)
library(hms)
# Convert related columns to POSIXct time format
Country$sleeps_longest_stretch_time <- as.POSIXct(Country$sleeps_longest_stretch_time, format = "%H:%M")
Country$baby_sleep_time <- as.POSIXct(Country$baby_sleep_time, format = "%H:%M")
Country$baby_wake_time <- as.POSIXct(Country$baby_wake_time, format = "%H:%M")
Country$baby_total_sleep_time <- as.POSIXct(Country$baby_total_sleep_time, format = "%H:%M")

# Calculation of average time for columns based on country
avg_sleep_time <- Country %>%
  group_by(country) %>%
  summarise(avg_sleeps_longest_stretch_time = format(mean(sleeps_longest_stretch_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_sleep_time = format(mean(baby_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_wake_time = format(mean(baby_wake_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_total_sleep_time = format(mean(baby_total_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))

# Columns to check
columns_to_check <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

# Unique countries
unique_countries <- unique(Country$country)

# Loop through each unique country
for (country in unique_countries) {
  print(paste("Country: ", country))
  
  # Subset the data for the current country
  subset_data <- subset(Country, country == country)
  
  # Convert time to total minutes
  subset_data$sleeps_longest_stretch_time <- as.numeric(format(subset_data$sleeps_longest_stretch_time, "%H")) * 60 +
    as.numeric(format(subset_data$sleeps_longest_stretch_time, "%M"))
  
  subset_data$baby_sleep_time <- as.numeric(format(subset_data$baby_sleep_time, "%H")) * 60 +
    as.numeric(format(subset_data$baby_sleep_time, "%M"))
  
  subset_data$baby_wake_time <- as.numeric(format(subset_data$baby_wake_time, "%H")) * 60 +
    as.numeric(format(subset_data$baby_wake_time, "%M"))
  
  subset_data$baby_total_sleep_time <- as.numeric(format(subset_data$baby_total_sleep_time, "%H")) * 60 +
    as.numeric(format(subset_data$baby_total_sleep_time, "%M"))
  
  # Perform Shapiro-Wilk test for each column
  for (column in columns_to_check) {
    print(paste("Shapiro-Wilk Test for column:", column))
    
    # Get the column data
    column_data <- subset_data[[column]]
    
    # Remove NAs
    column_data <- na.omit(column_data)
    
    # Check if data length is sufficient for Shapiro-Wilk test (at least 3)
    if (length(column_data) > 2) {
      test_result <- shapiro.test(column_data)
      print(test_result)
    } else {
      print("Insufficient data for Shapiro-Wilk test.")
    }
  }
}

# Kruskal-Wallis tests
kruskal_test_sleeps_longest_stretch_time <- kruskal.test(sleeps_longest_stretch_time ~ country, data = Country)
kruskal_test_baby_sleep_time <- kruskal.test(baby_sleep_time ~ country, data = Country)
kruskal_test_baby_wake_time <- kruskal.test(baby_wake_time ~ country, data = Country)
kruskal_test_baby_total_sleep_time <- kruskal.test(baby_total_sleep_time ~ country, data = Country)
kruskal_test_past_week_average_woken_up <- kruskal.test(past_week_average_woken_up ~ country, data = Country)

# Print the results
print(kruskal_test_sleeps_longest_stretch_time)
print(kruskal_test_baby_sleep_time)
print(kruskal_test_baby_wake_time)
print(kruskal_test_baby_total_sleep_time)
print(kruskal_test_past_week_average_woken_up)

# Function for performing post-hoc test on different groups (countries in this case)
perform_posthoc <- function(data, response_variable) {
  posthoc_result <- pairwise.wilcox.test(data[[response_variable]], data$country, p.adjust.method = "bonferroni")
  print(paste("Post-hoc for", response_variable))
  print(posthoc_result)
}

# Filter the dataset to only include the columns of interest and the grouping variable
filtered_Country <- Country %>% select(country, baby_sleep_time, baby_wake_time, baby_total_sleep_time)

# Perform the post-hoc tests
perform_posthoc(filtered_Country, "baby_sleep_time")
perform_posthoc(filtered_Country, "baby_wake_time")
perform_posthoc(filtered_Country, "baby_total_sleep_time")

# Convert related columns to POSIXct time format
Country$sleeps_longest_stretch_time <- as.POSIXct(Country$sleeps_longest_stretch_time, format = "%H:%M")
Country$baby_sleep_time <- as.POSIXct(Country$baby_sleep_time, format = "%H:%M")
Country$baby_wake_time <- as.POSIXct(Country$baby_wake_time, format = "%H:%M")
Country$baby_total_sleep_time <- as.POSIXct(Country$baby_total_sleep_time, format = "%H:%M")


library(ggplot2)

# For 'sleeps_longest_stretch_time'
ggplot(Country, aes(x = country, y = as.numeric(sleeps_longest_stretch_time))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1.5) +
  labs(title = "Comparison of Longest Stretch Sleep Time Across Countries",
       x = "Country",
       y = "Longest Stretch Sleep Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))

# For 'baby_sleep_time'
ggplot(Country, aes(x = country, y = as.numeric(baby_sleep_time))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1.5) +
  labs(title = "Comparison of Baby Sleep Onset Time Across Countries",
       x = "Country",
       y = "Baby Sleep Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))

# For 'baby_wake_time'
ggplot(Country, aes(x = country, y = as.numeric(baby_wake_time))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1.5) +
  labs(title = "Comparison of Baby Wake Time Across Countries",
       x = "Country",
       y = "Baby Wake Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))

# For 'baby_total_sleep_time'
ggplot(Country, aes(x = country, y = as.numeric(baby_total_sleep_time))) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1.5) +
  labs(title = "Comparison of Total Baby Sleep Time Across Countries",
       x = "Country",
       y = "Total Baby Sleep Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))

# For 'past_week_average_woken_up'
ggplot(Country, aes(x = country, y = past_week_average_woken_up)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label=sprintf("%.2f", ..y..)), vjust=-1.5) +
  labs(title = "Comparison of Average Times Baby Woke Up in the Past Week Across Countries",
       x = "Country",
       y = "Average Times Woken Up in the Past Week")

# Print average time for each country
print(avg_sleep_time)


# Calculate Average Sleep Metrics for Infants by Country and Age Group
avg_sleep_time <- Country %>%
  mutate(Age_group = cut(Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)) %>%
  group_by(country, Age_group) %>%
  summarise(avg_sleeps_longest_stretch_time = format(mean(sleeps_longest_stretch_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_sleep_time = format(mean(baby_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_wake_time = format(mean(baby_wake_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_total_sleep_time = format(mean(baby_total_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))


# Plotting for Sleeps Longest Stretch
ggplot(avg_sleep_time, aes(x = Age_group, y = avg_sleeps_longest_stretch_time, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_sleeps_longest_stretch_time), vjust = 1.5, position = position_dodge(0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Sleeps Longest Stretch Time by Country and Age Group",
       x = "Age Group",
       y = "Average Sleeps Longest Stretch Time (HH:MM)") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Plotting for baby_sleep_time
ggplot(avg_sleep_time, aes(x = Age_group, y = avg_baby_sleep_time, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_sleep_time), vjust = 1.5, position = position_dodge(0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Sleep Onset Time by Country and Age Group",
       x = "Age Group",
       y = "Average Baby Sleep Time (HH:MM)") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Plotting for Baby Wake Time
ggplot(avg_sleep_time, aes(x = Age_group, y = avg_baby_wake_time, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_wake_time), vjust = 1.5, position = position_dodge(0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Wake Time by Country and Age Group",
       x = "Age Group",
       y = "Average Baby Wake Time (HH:MM)") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Plotting for Baby Total Sleep Time
ggplot(avg_sleep_time, aes(x = Age_group, y = avg_baby_total_sleep_time, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_total_sleep_time), vjust = 1.5, position = position_dodge(0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Total Sleep Time by Country and Age Group",
       x = "Age Group",
       y = "Average Baby Total Sleep Time (HH:MM)") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Plotting for Past Week Average Woken Up
ggplot(avg_sleep_time, aes(x = Age_group, y = avg_past_week_average_woken_up, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", avg_past_week_average_woken_up)), vjust = 1.5, position = position_dodge(0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Past Week Woken Up by Country and Age Group",
       x = "Age Group",
       y = "Average Past Week Woken Up") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10))


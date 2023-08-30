# Load the data
setwd("D:/study/Dissertation/data")
Cosleep=read.csv("co-sleep.csv")

## Print the first few rows
head(Cosleep)
dim(Cosleep)

# Convert related columns to POSIXct time format
Cosleep$sleeps_longest_stretch_time <- as.POSIXct(Cosleep$sleeps_longest_stretch_time, format = "%H:%M")
Cosleep$baby_sleep_time <- as.POSIXct(Cosleep$baby_sleep_time, format = "%H:%M")
Cosleep$baby_wake_time <- as.POSIXct(Cosleep$baby_wake_time, format = "%H:%M")
Cosleep$baby_total_sleep_time <- as.POSIXct(Cosleep$baby_total_sleep_time, format = "%H:%M")

# Calculation of average time for columns based on sleep locations
library(dplyr)
avg_sleep_time <- Cosleep %>%
  group_by(where_sleep_longest) %>%
  summarise(avg_sleeps_longest_stretch_time = format(mean(sleeps_longest_stretch_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_sleep_time = format(mean(baby_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_wake_time = format(mean(baby_wake_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_total_sleep_time = format(mean(baby_total_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))

# Draw boxplots
library(ggplot2)

Cosleep$where_sleep_longest <- as.factor(Cosleep$where_sleep_longest)

ggplot(Cosleep, aes(x = where_sleep_longest, y = as.numeric(sleeps_longest_stretch_time))) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1) +
  labs(title = "Longest Stretch Time by Sleep Location",
       x = "Sleep Location",
       y = "Longest Stretch Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))

ggplot(Cosleep, aes(x = where_sleep_longest, y = as.numeric(baby_sleep_time))) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1) +
  labs(title = "Baby Sleep Onset Time by Sleep Location",
       x = "Sleep Location",
       y = "Baby Sleep Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))


ggplot(Cosleep, aes(x = where_sleep_longest, y = as.numeric(baby_wake_time))) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1) +
  labs(title = "Baby Wake Time by Sleep Location",
       x = "Sleep Location",
       y = "Baby Wake Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))


ggplot(Cosleep, aes(x = where_sleep_longest, y = as.numeric(baby_total_sleep_time))) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "text", aes(label=strftime(as.POSIXct(..y.., origin="1970-01-01"), format="%H:%M")), vjust=-1) +
  labs(title = "Baby Total Sleep Time by Sleep Location",
       x = "Sleep Location",
       y = "Baby Total Sleep Time") +
  scale_y_continuous(labels = function(x) strftime(as.POSIXct(x, origin="1970-01-01"), format="%H:%M"))


ggplot(Cosleep, aes(x = where_sleep_longest, y = past_week_average_woken_up)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "text", aes(label=sprintf("%.2f", ..y..)), vjust=-1) +
  labs(title = "Average Times Woken Up in the Past Week by Sleep Location",
       x = "Sleep Location",
       y = "Average Times Woken Up in the Past Week")


# Print average sleep time 
print(avg_sleep_time)

# Convert POSIXct time format to numbers
Cosleep$sleeps_longest_stretch_mins <- as.numeric(difftime(Cosleep$sleeps_longest_stretch_time, as.POSIXct("00:00", format="%H:%M"), units = "mins"))
Cosleep$baby_sleep_time_mins <- as.numeric(difftime(Cosleep$baby_sleep_time, as.POSIXct("00:00", format="%H:%M"), units = "mins"))
Cosleep$baby_wake_time_mins <- as.numeric(difftime(Cosleep$baby_wake_time, as.POSIXct("00:00", format="%H:%M"), units = "mins"))
Cosleep$baby_total_sleep_time_mins <- as.numeric(difftime(Cosleep$baby_total_sleep_time, as.POSIXct("00:00", format="%H:%M"), units = "mins"))

# Columns to check for normality
columns_to_check <- c("sleeps_longest_stretch_time", "baby_sleep_time", "baby_wake_time", "baby_total_sleep_time", "past_week_average_woken_up")

# Unique sleep locations
unique_sleep_locations <- unique(Cosleep$where_sleep_longest)

# Loop through each unique sleep location
for (location in unique_sleep_locations) {
  
  print(paste("Sleep Location: ", location))
  
  # Subset the data for the current sleep location
  subset_data <- subset(Cosleep, where_sleep_longest == location)
  
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


# Kruskal-Wallis test for sleeps_longest_stretch_time
kruskal_sleeps_longest_stretch_time <- kruskal.test(sleeps_longest_stretch_time ~ where_sleep_longest, data = Cosleep)
print(paste("P-value for longest sleep stretches: ", kruskal_sleeps_longest_stretch_time$p.value))

# Kruskal-Wallis test for baby_sleep_time
kruskal_baby_sleep_time <- kruskal.test(baby_sleep_time ~ where_sleep_longest, data = Cosleep)
print(paste("P-value for sleep onset time: ", kruskal_baby_sleep_time$p.value))

# Kruskal-Wallis test for baby_wake_time
kruskal_baby_wake_time <- kruskal.test(baby_wake_time ~ where_sleep_longest, data = Cosleep)
print(paste("P-value for baby wake time: ", kruskal_baby_wake_time$p.value))

# Kruskal-Wallis test for baby_total_sleep_time
kruskal_baby_total_sleep_time <- kruskal.test(baby_total_sleep_time ~ where_sleep_longest, data = Cosleep)
print(paste("P-value for baby total sleep time: ", kruskal_baby_total_sleep_time$p.value))

# Kruskal-Wallis test for past_week_average_woken_up
kruskal_past_week_average_woken_up <- kruskal.test(past_week_average_woken_up ~ where_sleep_longest, data = Cosleep)
print(paste("P-value for past week average woken up: ", kruskal_past_week_average_woken_up$p.value))

# Load the necessary libraries
library(stats)

# Post-hoc pairwise Wilcoxon tests for sleeps_longest_stretch_mins
pairwise_wilcox_sleeps_longest_stretch_mins <- pairwise.wilcox.test(Cosleep$sleeps_longest_stretch_mins, Cosleep$where_sleep_longest, p.adjust.method = "BH")
print("Pairwise Wilcoxon tests for sleeps_longest_stretch_mins:")
print(pairwise_wilcox_sleeps_longest_stretch_mins)

# Post-hoc pairwise Wilcoxon tests for baby_sleep_time_mins
pairwise_wilcox_baby_sleep_time_mins <- pairwise.wilcox.test(Cosleep$baby_sleep_time_mins, Cosleep$where_sleep_longest, p.adjust.method = "BH")
print("Pairwise Wilcoxon tests for baby_sleep_time_mins:")
print(pairwise_wilcox_baby_sleep_time_mins)

# Post-hoc pairwise Wilcoxon tests for baby_wake_time_mins
pairwise_wilcox_baby_wake_time_mins <- pairwise.wilcox.test(Cosleep$baby_wake_time_mins, Cosleep$where_sleep_longest, p.adjust.method = "BH")
print("Pairwise Wilcoxon tests for baby_wake_time_mins:")
print(pairwise_wilcox_baby_wake_time_mins)

# Post-hoc pairwise Wilcoxon tests for baby_total_sleep_time_mins
pairwise_wilcox_baby_total_sleep_time_mins <- pairwise.wilcox.test(Cosleep$baby_total_sleep_time_mins, Cosleep$where_sleep_longest, p.adjust.method = "BH")
print("Pairwise Wilcoxon tests for baby_total_sleep_time_mins:")
print(pairwise_wilcox_baby_total_sleep_time_mins)

# Post-hoc pairwise Wilcoxon tests for past_week_average_woken_up
pairwise_wilcox_past_week_average_woken_up <- pairwise.wilcox.test(Cosleep$past_week_average_woken_up, Cosleep$where_sleep_longest, p.adjust.method = "BH")
print("Pairwise Wilcoxon tests for past_week_average_woken_up:")
print(pairwise_wilcox_past_week_average_woken_up)



# Calculation of average time for columns based on age
avg_sleep_time <- Cosleep %>%
  mutate(Age_group = cut(Age_in_months_at_reg,
                         breaks = c(0, 3, 7, 13, 19, 25),
                         labels = c("0-2 months", "3-6 months", "7-12 months", "13-18 months", "19-24 months"),
                         include.lowest = TRUE,
                         right = FALSE)) %>%
  group_by(where_sleep_longest, Age_group) %>%
  summarise(avg_sleeps_longest_stretch_time = format(mean(sleeps_longest_stretch_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_sleep_time = format(mean(baby_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_wake_time = format(mean(baby_wake_time, na.rm = TRUE), format = "%H:%M"),
            avg_baby_total_sleep_time = format(mean(baby_total_sleep_time, na.rm = TRUE), format = "%H:%M"),
            avg_past_week_average_woken_up = mean(past_week_average_woken_up, na.rm = TRUE))

library(RColorBrewer)
# Set 'where_sleep_longest' as an ordered factor
avg_sleep_time$where_sleep_longest <- factor(avg_sleep_time$where_sleep_longest, levels = 0:6, ordered = TRUE)

# Plotting for Sleeps Longest Stretch
ggplot(avg_sleep_time, aes(x = where_sleep_longest, 
                           y = avg_sleeps_longest_stretch_time, 
                           fill = as.factor(where_sleep_longest))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_sleeps_longest_stretch_time), 
            vjust = 0.5, position = position_dodge(3), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Sleeps Longest Stretch Time by Sleep Location and Age Group",
       x = "Sleep Location",
       y = "Average Sleeps Longest Stretch Time (HH:MM)",
       fill = "Sleep Location") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  facet_grid(~ Age_group, scales = "free", space = "free")

# Plotting for Baby Sleep Time
ggplot(avg_sleep_time, aes(x = where_sleep_longest, y = avg_baby_sleep_time, fill = as.factor(where_sleep_longest))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_sleep_time), vjust = 0.5, position = position_dodge(3), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Sleep Time by Sleep Location and Age Group",
       x = "Sleep Location",
       y = "Average Baby Sleep Time (HH:MM)",
       fill = "Sleep Location") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  facet_grid(~ Age_group, scales = "free", space = "free")

# Plotting for Baby Wake Time
ggplot(avg_sleep_time, aes(x = where_sleep_longest, y = avg_baby_wake_time, fill = as.factor(where_sleep_longest))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_wake_time), vjust = 0.5, position = position_dodge(3), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Wake Time by Sleep Location and Age Group",
       x = "Sleep Location",
       y = "Average Baby Wake Time (HH:MM)",
       fill = "Sleep Location") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  facet_grid(~ Age_group, scales = "free", space = "free")

# Plotting for Baby Total Sleep Time
ggplot(avg_sleep_time, aes(x = where_sleep_longest, y = avg_baby_total_sleep_time, fill = as.factor(where_sleep_longest))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = avg_baby_total_sleep_time), vjust = 0.5, position = position_dodge(3), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Baby Total Sleep Time by Sleep Location and Age Group",
       x = "Sleep Location",
       y = "Average Baby Total Sleep Time (HH:MM)",
       fill = "Sleep Location") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  facet_grid(~ Age_group, scales = "free", space = "free")

# Plotting for Past Week Average Woken Up
ggplot(avg_sleep_time, aes(x = where_sleep_longest, y = avg_past_week_average_woken_up, fill = as.factor(where_sleep_longest))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f", avg_past_week_average_woken_up)), vjust = 0.5, position = position_dodge(3), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Average Times Woken Up Over the Past Week by Sleep Location and Age Group",
       x = "Sleep Location",
       y = "Average Times Woken Up",
       fill = "Sleep Location") +
  theme_minimal() +
  theme(plot.margin = margin(10, 10, 10, 10)) +
  facet_grid(~ Age_group, scales = "free", space = "free")


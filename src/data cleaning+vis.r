# We load the necessary libraries
library(dplyr)       # Data manipulation
library(stringdist)  # Fuzzy matching for typos
library(readr)       # Reading CSV files
library(caret)       # Handling missing values
library(ggplot2)
library(ggridges)
library(lubridate)
library(randomForest)
library(viridis)

# Density Plot to show the distribution of downtime per OS
ggplot(Uncleaned_data, aes(x = cleandowntime, color = os_standardized)) +
  geom_density(size = 1.2, alpha = 0.7) +  # Creates a smooth density plot with thicker lines and transparency
  scale_color_viridis_d(option = "plasma") +  # Applies a visually accessible color palette
  labs(
    title = "Density Plot of Downtime per OS",  # Title of the plot
    subtitle = "Kernel Density Plot Downtime Across Different Operating Systems.",  # Subtitle to clarify the visualization
    x = "Downtime",  # X-axis label
    y = "Density"  # Y-axis label
  ) +
  theme_minimal()  # Uses a minimalistic theme for a clean look


# Create downtime categories
Uncleaned_data$Downtime_Group <- cut(Uncleaned_data$cleandowntime, 
                                     breaks = seq(0, max(Uncleaned_data$cleandowntime), by = 10), 
                                     include.lowest = TRUE)

# Plot the relationship between downtime and financial loss
ggplot(Uncleaned_data, aes(x = Downtime_Group, y = loss)) +
  geom_bar(stat = "identity", alpha = 0.8) +  # Creates a bar chart where height represents total financial loss
  scale_fill_viridis_d(option = "plasma") +  # Uses an accessible color palette
  labs(
    title = "Bar Plot of Total Financial Loss by Downtime Range",  # Title of the chart
    subtitle = "Downtime is grouped into 10-day intervals, and financial loss is aggregated to show total impact at each downtime level.",  # Subtitle explaining grouping logic
    x = "Downtime",  # X-axis label
    y = "Total Financial Loss"  # Y-axis label
  ) +
  theme_minimal() +  # Uses a clean layout
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")  # Rotates x-axis labels for better readability

# Create a pie chart to visualize OS usage distribution
ggplot(os_usage_counts, aes(x = "", y = Usage_Count, fill = OS_Standardized)) + 
  geom_bar(stat = "identity", width = 1) +  # Creates a bar chart where bar height represents Usage_Count
  coord_polar("y", start = 0) +  # Converts bar chart into a pie chart using polar coordinates
  scale_fill_viridis_d(option = "plasma") +  # Uses the "plasma" color palette for better contrast
  labs(
    title = "OS Usage Distribution (Pie Chart)",  # Main title of the chart
    subtitle = "Proportion of OS usage in dataset",  # Subtitle explaining the chart
    x = NULL, y = NULL  # Removes x and y axis labels for a cleaner look
  ) +
  theme_void() +  # Removes background elements and gridlines for a neat presentation
  theme(legend.position = "right")  # Positions the legend on the right for readability

# Scatter plot to visualize financial loss distribution per OS
ggplot(Uncleaned_data, aes(x = os_standardized, y = loss, color = os_standardized)) +
  geom_jitter(alpha = 0.5, width = 0.2) +  # Adds random jitter to reduce overlapping points
  scale_color_viridis_d(option = "plasma") +  # Uses a visually accessible color scale
  facet_wrap(~ os_standardized, scales = "free_x") +  # Creates a separate panel for each OS
  labs(
    title = "Faceted Scatter plot of Financial Loss per OS",  # Main title
    subtitle = "Each point represents a single instance of financial loss",  # Clarifies the visualization
    x = "Operating System",  # X-axis label
    y = "Financial Loss"  # Y-axis label
  ) +
  theme_minimal() +  # Clean and minimalistic theme
  theme(legend.position = "none")  # Hides the legend as OS names are already labeled


ggplot(Uncleaned_data, aes(x = DownTime, fill = OS_Standardized)) +
  geom_density(alpha = 0.7) +  # Creates smooth density curves with transparency
  scale_fill_viridis_d(option = "plasma") +  # Applies a colorblind-friendly palette
  facet_wrap(~ OS_Standardized) +  # Creates separate density plots for each OS
  labs(
    title = "Faceted Density Plot of Downtime per OS",  # Main title
    subtitle = "Separate density plots per OS ",  # Subtitle for clarification
    x = "Downtime",  # X-axis label
    y = "Density"  # Y-axis label
  ) +
  theme_minimal()  # Uses a clean and modern theme



# Extract day from date and cycle every 30 days
ggplot(Uncleaned_data, aes(x = factor(day(date) %% 30))) +
  geom_bar(fill = "steelblue", alpha = 0.8, width = 0.8) +  # Creates a bar chart with transparency
  coord_polar(start = 0) +  # Converts the bar chart into a circular plot
  labs(
    title = "Cyclical Downtime Distribution",  # Main title
    x = "Month Cycle",  # X-axis label
    y = "Count of Downtime Incidents"  # Y-axis label
  ) +
  theme_minimal()  # Clean layout


# Extract the day of the month
Uncleaned_data$Day_of_Month <- as.numeric(format(Uncleaned_data$date, "%d"))

# Count the number of downtime incidents per day
downtime_counts <- Uncleaned_data %>%
  group_by(Day_of_Month) %>%
  summarise(Incident_Count = n())

# Identify peak period (e.g., Days 13-21 based on visual trends)
peak_days <- downtime_counts %>%
  filter(Day_of_Month >= 10 & Day_of_Month <= 20)

# Calculate percentage of incidents in the peak period
total_incidents <- sum(downtime_counts$Incident_Count)
peak_incidents <- sum(peak_days$Incident_Count)
peak_percentage <- (peak_incidents / total_incidents) * 100

# Print result
cat("Percentage of downtime incidents occurring between Days 13-21:", round(peak_percentage, 2), "%\n")


# Summing financial loss per OS
os_loss <- Uncleaned_data %>%
  group_by(os_standardized) %>%
  summarise(Total_Loss = sum(cleaned_loss, na.rm = TRUE))

# Calculate percentage of total loss
os_loss <- os_loss %>%
  mutate(Loss_Percentage = (Total_Loss / sum(Total_Loss)) * 100)

# Display results
print(os_loss)

# Count occurrences of each OS for downtime
os_downtime_counts <- Uncleaned_data %>%
  group_by(os_standardized) %>%
  summarise(Downtime_Count = n()) %>%
  mutate(Percentage = (Downtime_Count / sum(Downtime_Count)) * 100)

# Summing financial loss per OS
os_loss <- Uncleaned_data %>%
  group_by(os_standardized) %>%
  summarise(Total_Loss = sum(cleaned_loss, na.rm = TRUE)) %>%
  mutate(Loss_Percentage = (Total_Loss / sum(Total_Loss)) * 100)

# Merge both datasets
os_analysis <- merge(os_downtime_counts, os_loss, by = "os_standardized")

# Display results
print(os_analysis)

# Convert OS to a categorical variable (factor)
Uncleaned_data$os_standardized <- as.factor(Uncleaned_data$os_standardized)

# Remove missing values if any
Uncleaned_data <- Uncleaned_data %>% drop_na(loss, cleandowntime)

# View summary to ensure correct data types
summary(Uncleaned_data)

set.seed(123) # For reproducibility

# Split data into training (80%) and testing (20%)
trainIndex <- createDataPartition(Uncleaned_data$cleaned_loss, p = 0.8, list = FALSE)
train_data <- Uncleaned_data[trainIndex, ]
test_data <- Uncleaned_data[-trainIndex, ]

# Train the Random Forest model
rf_model <- randomForest(cleaned_loss ~ os_standardized + cleandowntime, 
                         data = train_data, 
                         ntree = 500, # Number of trees
                         mtry = 2,    # Number of predictors to consider at each split
                         importance = TRUE)

# View feature importance
importance(rf_model)

# Plot feature importance
varImpPlot(rf_model)

# Predict financial loss for test data
predictions <- predict(rf_model, test_data)

# Compare actual vs predicted
results <- data.frame(Actual_Loss = test_data$Cleaned_Loss, Predicted_Loss = predictions)
head(results)


# Compute Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((results$Actual_Loss - results$Predicted_Loss)^2))
print(paste("RMSE:", rmse))

# Compute R-Squared (Model Fit)
r_squared <- cor(results$Actual_Loss, results$Predicted_Loss)^2
print(paste("R-Squared:", r_squared))



ggplot(results, aes(x = Actual_Loss, y = Predicted_Loss)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Random Forest: Actual vs Predicted Financial Loss",
       x = "Actual Financial Loss ",
       y = "Predicted Financial Loss ") +
  theme_minimal()

# Filter out Jupiter, Legacy OS, and BSD
filtered_data <- Uncleaned_data %>%
  filter(!os_standardized %in% c("Juniper", "Legacy OS", "BSD"))
library(ggplot2)

# Generate Density Plot for Downtime by OS (Excluding Jupiter, Legacy OS, and BSD)
ggplot(filtered_data, aes(x = cleandowntime, color = os_standardized)) +
  geom_density(size = 1) +  # Creates a smooth density curve with line thickness of 1
  labs(
    title = "Density Plot of Downtime per OS",  # Main title
    subtitle = "Kernel Density Plot of Downtime Excluding High-Downtime OSs",  # Subtitle explaining the exclusion
    x = "Downtime",  # X-axis label (Downtime duration)
    y = "Density",  # Y-axis label (Density of downtime occurrences)
    color = "OS Standardized"  # Legend label for OS categories
  ) +
  theme_minimal() +  # Uses a clean and modern theme
  theme(legend.position = "right")  # Places legend on the right for clarity



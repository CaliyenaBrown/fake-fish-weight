#load packages 
library(ggplot2)
library(tidyr)

#set working directory 
setwd("~/Documents/SCHOOL /5th YEAR /MARI 3603")
fish <- read.csv("fish_data.csv")


#histogram 

# Convert diet1 and diet2 columns to numeric
fish$Diet1 <- as.numeric(as.character(fish$Diet1))
fish$Diet2 <- as.numeric(as.character(fish$Diet2))

# Filter out non-finite values
fish_data <- na.omit(fish)

# Reshape the data into long format
fish_data_long <- pivot_longer(fish_data, cols = c(Diet1, Diet2), names_to = "Diet")

# Create a histogram for both diets on one graph
ggplot(fish_data_long, aes(x = value, fill = Diet)) +
  geom_histogram(binwidth = 15, position = "dodge", alpha = 0.9) +
  labs(
       x = "Fish Weight (g)",
       y = "Counts") +
  scale_fill_manual(values = c("blue", "orange")) +  # Customizing fill colors
  coord_cartesian(xlim = c(0, 800)) +  # Setting x-axis limit
  theme_bw() +  # Change theme to have white background
  theme(panel.grid = element_blank())  # Remove grid lines

#save plot 
ggsave("weight_histogram.png",last_plot(), width = 8, height = 6, 
       units = c("in"), dpi = 300)

#boxplot 
# Calculate upper limit for y-axis
y_upper <- quantile(fish_data_long$value, 0.99, na.rm = TRUE)

# Create a boxplot for both diets on one graph without outliers
ggplot(fish_data_long, aes(x = Diet, y = value, fill = Diet)) +
  geom_boxplot(outlier.shape = NA) + # Remove outliers
  labs(
       x = "Diet",
       y = "Fish Weight (g)") +
  scale_fill_manual(values = c("blue", "orange")) +  # Customizing fill colors
  theme_bw() +  # Change theme to have white background
  theme(panel.grid = element_blank()) +  # Remove grid lines
  coord_cartesian(ylim = c(0, y_upper)) +  # Adjust y-axis limits
  scale_x_discrete(labels = function(x) NULL)

#save plot 
ggsave("weight_boxplot.png", last_plot(), width = 8, height = 6, 
       units = c("in"), dpi = 300)

#calculate summary statistics 

# Calculate mean for each column
mean_values <- sapply(fish, mean, na.rm = TRUE)
# Calculate median for each column
median_values <- sapply(fish, median, na.rm = TRUE)
# Calculate standard deviation for each column
sd_values <- sapply(fish, sd, na.rm = TRUE)
# Calculate minimum for each column
min_values <- sapply(fish, min, na.rm = TRUE)
# Calculate maximum for each column
max_values <- sapply(fish, max, na.rm = TRUE)
# Combine results into a data frame
summary_stats <- data.frame(
Mean = mean_values,
Median = median_values,
SD = sd_values,
Min = min_values,
Max = max_values
)
# Print summary statistics
print(summary_stats)

# Estimate Treatment Growth Percentage 
# Assuming you have calculated the means for each treatment and stored them in variables
mean_diet1 <- mean(fish$Diet1, na.rm = TRUE)
mean_diet2 <- mean(fish$Diet2, na.rm = TRUE)

# Calculate the percent change from diet 1 to diet 2
percent_change <- ((mean_diet2 - mean_diet1) / mean_diet1) * 100

# Print the percent change
print(paste("Treatment Growth Percent:", percent_change, "%"))


# Perform t-test
t_test_result <- t.test(fish$Diet1, fish$Diet2)

# Print the result
print(t_test_result)


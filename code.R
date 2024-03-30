library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)

BulkEM <- fread("datalog/BulkEM.csv", header = TRUE)
str(BulkEM)

# Classes ‘data.table’ and 'data.frame':	1009 obs. of  8 variables:
#   $ Time                 : num  45354 45354 45354 45354 45354 ...
# $ ms                   : int  0 0 0 0 0 0 0 0 0 0 ...
# $ Pane1-EM102A_flow    : chr  "6,023.1651003" "5,968.5410307" "5,994.6484085" "6,032.3488774" ...
# $ Pane1-EM102A_power   : chr  "12,491.3637805" "12,364.949673" "12,452.898568" "12,568.305648" ...
# $ Pane1-EM102A_retTemp : num  11.5 11.5 11.5 11.5 11.5 ...
# $ Pane1-EM102A_suppTemp: num  5.2 5.21 5.22 5.2 5.2 ...
# $ Pane1-PT102A1_pv     : num  3.88 3.89 3.89 3.89 3.88 ...
# $ Pane1-PT102A2_pv     : num  5.7 5.71 5.71 5.7 5.7 ...

# print(BulkEM, digits = 21)
# BulkEM <- read.table("datalog/BulkEM.xls", fileEncoding="UTF-16LE", header = TRUE)

ymd_hms()

BulkEMGOOD <-  BulkEM %>% 
                mutate (Time1 = as.numeric(Time)) %>% 
                mutate (as_datetime(Time, origin = "1970-01-01"))
# Admin_Flow <- 
#   mutate(Admin_Flow, Time1=Time0+days(7842)) %>%
#   select(Time1, Data) %>%
#   rename (Flow = Data)











# Assuming your data is in a dataframe called `data`

# Convert time column to date (assuming it's numeric)
data$time <- as.Date(data$time)

# Calculate week number (1-52) considering Monday as the start of the week
data$week <- format(data$time, "%U")

# Group by week and calculate weekly mean for value
weekly_means <- aggregate(value ~ week, data, FUN = mean)

# Get the current week number (adjust if needed)
current_week <- format(Sys.Date(), "%U")

# Define a color gradient function
color_gradient <- function(current_week, num_weeks = 52) {
  week_diff <- abs(weekly_means$week - current_week)
  # Adjust the range of alpha (transparency) for color intensity
  alpha <- 1 - week_diff / (num_weeks / 2)
  return(scale_color_gradient(low = "blue", high = "darkblue", alpha = alpha))
}

# Create the plot
ggplot(weekly_means, aes(x = week, y = value)) +
  geom_line() +  # No color argument here
  color_gradient(current_week) +  # Call the color gradient function
  labs(title = "Weekly Means (Monday-Sunday)",
       x = "Week Number",
       y = "Mean Value") +
  theme_classic()








# dcdcdc
# Explanation of changes:
#   dcdcddccd
# now i try edit in github
#   We define a function called color_gradient that takes the current week number and the total number of weeks as arguments.
# Inside the function, it calculates the absolute difference between each week in the data and the current week.
# Based on this difference, we create an alpha value using a scaling function. Weeks closer to the current week will have a higher alpha (more opaque), resulting in a darker color.
# In the ggplot call, we remove the color argument from geom_line and instead use the color_gradient function to dynamically set the color based on the week's proximity to the current week.
# This approach ensures the line representing the most recent week is the darkest, with colors gradually fading for earlier weeks.

setwd("C:/Hsuan_Repo/R/Google_Case1")
getwd()
data_2019<-read.csv("Divvy_Trips_2019_Q1.csv")
data_2020<-read.csv("Divvy_Trips_2020_Q1.csv")
data_2019
data_2020
summary(data.frame(data_2019))
summary(data.frame(data_2020))

class(data_2019)
class(data_2020)



ride_length2019 <- as.numeric(difftime(data_2019$end_time, data_2019$start_time, units="mins"))
ride_length2020 <- as.numeric(difftime(data_2020$ended_at, data_2020$started_at, units="mins"))
ride_length2019
ride_length2020

mean_ride_length2019 <- mean(ride_length2019,na.rm=T)
print(mean_ride_length2019)
mean_ride_length2020 <- mean(ride_length2020,na.rm=T)
print(mean_ride_length2020)

max_ride_length2019 <- max(ride_length2019,na.rm=T)
print(max_ride_length2019)
max_ride_length2020 <- max(ride_length2020,na.rm=T)
print(max_ride_length2020)

min_ride_length2019 <- min(ride_length2019,na.rm=T)
print(min_ride_length2019)
min_ride_length2020 <- min(ride_length2020,na.rm=T)
print(min_ride_length2020)



install.packages("tidyverse")
library("lubridate")
datetimes2019 <-ymd_hms(data_2019$start_time)
data_2019date <- as.Date(datetimes2019, format="%Y-%m-$d %H:%M:%S")
dayofweek2019<- wday(datetimes2019)
dayofweek2019

typeof(data_2020$started_at)
datetimes2020 <-ymd_hms(data_2020$started_at)
data_2020date <- as.Date(datetimes2020, format="%Y-%m-$d %H:%M:%S")
dayofweek2020<- wday(datetimes2020)
dayofweek2020


install.packages("modeest")
library(modeest)
mode_dayofweek2019<- mlv(dayofweek2019, method = "mfv")
print(mode_dayofweek2019)
mode_dayofweek2020<- mlv(dayofweek2020, method = "mfv")
print(mode_dayofweek2020)


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming that ride_length2019 and usertype are columns in data_2019
rides2019 <- data.frame(
  member_casual2019 = data_2019$usertype,
  ride_length2019 = as.numeric(ride_length2019)
)

# Calculate average ride length for each member type
average_ride_length2019 <- rides2019 %>%
  group_by(member_casual2019) %>%
  summarize(avg_ride_length2019 = mean(ride_length2019, na.rm = TRUE))

# Plot the summarized data
ggplot(average_ride_length2019, aes(x = member_casual2019, y = avg_ride_length2019, fill = member_casual2019)) +
  geom_bar(stat = "identity") +
  labs(title = "2019 Average Ride Length by Member Type",
       x = "Member Type",
       y = "Average Ride Length (minutes)") +
  theme_minimal() +
  scale_fill_manual(values = c("Subscriber" = "blue", "Customer" = "red"))


# Assuming that ride_length2020 and usertype are columns in data_2020
rides2020 <- data.frame(
  member_casual2020 = data_2020$member_casual,
  ride_length2020 = as.numeric(ride_length2020)
)

# Calculate average ride length for each member type
average_ride_length2020 <- rides2020 %>%
  group_by(member_casual2020) %>%
  summarize(avg_ride_length2020 = mean(ride_length2020, na.rm = TRUE))

# Plot the summarized data
ggplot(average_ride_length2020, aes(x = member_casual2020, y = avg_ride_length2020, fill = member_casual2020)) +
  geom_bar(stat = "identity") +
  labs(title = "2020 Average Ride Length by Member Type",
       x = "Member Type",
       y = "Average Ride Length (minutes)") +
  theme_minimal() +
  scale_fill_manual(values = c("member" = "green", "casual" ="orange"))


# Assuming that ride_length2019, usertype, day_of_week are columns in rides_DOW2019
rides_DOW2019 <- data.frame(
  member_casual2019 = data_2019$usertype,
  ride_length2019 = ride_length2019,
  day_of_week2019= dayofweek2019
)

# Calculate average ride length for each member type and Day of week
average_ride_length_by_day2019 <- rides_DOW2019 %>%
  group_by(member_casual2019, day_of_week2019) %>%
  summarize(avg_ride_length2019 = mean(ride_length2019, na.rm = TRUE))


# Faceted Bar Plot
ggplot(average_ride_length_by_day2019, aes(x = day_of_week2019, y = avg_ride_length2019, fill = member_casual2019)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Length by User Type and Day of Week in 2019",
       x = "Day of the Week",
       y = "Average Ride Length (minutes)") +
  facet_wrap(~ member_casual2019, ncol = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("Subscriber" = "blue", "Customer" = "red"))
  







# Assuming that ride_length2020, member_casual, day_of_week are columns in rides_DOW2020
rides_DOW2020 <- data.frame(
  member_casual2020 = data_2020$member_casual,
  ride_length2020 = ride_length2020,
  day_of_week2020= dayofweek2020
)

# Calculate average ride length for each member type and Day of week
average_ride_length_by_day2020 <- rides_DOW2020 %>%
  group_by(member_casual2020, day_of_week2020) %>%
  summarize(avg_ride_length2020 = mean(ride_length2020, na.rm = TRUE))


# Faceted Bar Plot
ggplot(average_ride_length_by_day2020, aes(x = day_of_week2020, y = avg_ride_length2020, fill = member_casual2020)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Length by User Type and Day of Week",
       x = "Day of the Week",
       y = "Average Ride Length (minutes)") +
  facet_wrap(~ member_casual2020, ncol = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("member" = "green", "casual" = "orange"))




# Assuming that member_casual2019, trip_id2019, day_of_week2019 are columns in rides_ID2019
rides_ID2019 <- data.frame(
  member_casual2019 = data_2019$usertype,
  trip_id2019 = data_2019$trip_id,
  day_of_week2019 = dayofweek2019
)


count_rides_by_day2019 <- rides_ID2019 %>%
  group_by(member_casual2019, day_of_week2019) %>%
  summarize(ride_count2019 = n())

ggplot(count_rides_by_day2019, aes(x = day_of_week2019, y = ride_count2019, fill = member_casual2019)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides by User Type and Day of Week in 2019",
       x = "Day of the Week",
       y = "Number of Rides") +
  facet_wrap(~ member_casual2019, ncol = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("Subscriber" = "blue", "Customer" = "red"))



# Assuming that member_casual2020, trip_id2020, day_of_week2020 are columns in rides_ID2020
rides_ID2020 <- data.frame(
  member_casual2020 = data_2020$member_casual,
  trip_id2020 = data_2020$ride_id,
  day_of_week2020 = dayofweek2020
)


count_rides_by_day2020 <- rides_ID2020 %>%
  group_by(member_casual2020, day_of_week2020) %>%
  summarize(ride_count2020 = n())

ggplot(count_rides_by_day2020, aes(x = day_of_week2020, y = ride_count2020, fill = member_casual2020)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides by User Type and Day of Week in 2020",
       x = "Day of the Week",
       y = "Number of Rides") +
  facet_wrap(~ member_casual2020, ncol = 1) +
  theme_minimal() +
  scale_fill_manual(values = c("member" = "green", "casual" = "orange"))




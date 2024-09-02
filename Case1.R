setwd("C:/Users/shawn/Dropbox/Hsuan_Repo/R/Google_Case/Case1")
getwd()
data_2019<-read.csv("Divvy_Trips_2019_Q1.csv")
data_2020<-read.csv("Divvy_Trips_2020_Q1.csv")
data_2019
data_2020
summary(data.frame(data_2019))
summary(data.frame(data_2020))

class(data_2019)
class(data_2020)

rm(df_2019)

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
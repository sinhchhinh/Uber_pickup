#Data visualization and data analysis following Data Flair's Project
#Using Uber Pickups in New York City dataset
#Data source: https://www.kaggle.com/fivethirtyeight/uber-pickups-in-new-york-city?select=uber-raw-data-janjune-15.csv

#Importing essential packages
#install.packages('ggthemes')
#install.packages('DT')

#Loading essential libraries 
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(tidyverse)
library(class) # Using for KNN Algorithm

#Creating a vector of colors, will be using for  to be implemented in plots 
colors = c("#A93226", "#9B59B6", "#5499C7", "#1ABC9C", "#F7DC6F", "#EDBB99", "#34495E")

#Loading raw data
apr_data <-  read.csv("Uber_pickup_Data/uber-raw-data-apr14.csv")
may_data <- read.csv("Uber_pickup_Data/uber-raw-data-may14.csv")
jun_data <- read.csv("Uber_pickup_Data/uber-raw-data-jun14.csv")
jul_data <- read.csv("Uber_pickup_Data/uber-raw-data-jul14.csv")
aug_data <- read.csv("Uber_pickup_Data/uber-raw-data-aug14.csv")
sep_data <- read.csv("Uber_pickup_Data/uber-raw-data-sep14.csv")

#Combine 2014 dataset
data_2014 <- rbind(apr_data,may_data,jun_data,jul_data,aug_data,sep_data)

#Changing the date format
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time) #Automatically assign UTC zone
data_2014$day   <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$year  <- factor(year(data_2014$Date.Time))

data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#Plotting the trips by the hours in a day                           
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = colors[1], color = "gray") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) + 
  labs(title = "Total of Uber trips by the hours in a day, NYC 2014",
       caption = "Data source: Dataflair")  #Lable the title for x and Y

#Trips by Hour and Month
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  scale_y_continuous(labels = comma) + 
  labs(title = "Total of Uber trips by the hours and month in NYC 2014",
       caption = "Data source: Dataflair") 

#Plotting data by trips during every day of the month
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) + 
  labs(title = "Total of everyday trips in NYC 2014",
       caption = "Data source: Dataflair") 

#Trip by day and month
day_month_group <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge", color = "gray") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors) + 
  labs(title = "Trip by day and month NYC 2014",
       caption = "Data source: Dataflair") 

#Trip by day and month of September
sep_group <- data_2014 %>%
  group_by(month = "Sep", day) %>%
  dplyr::summarize(Total = n())

ggplot(sep_group, aes(month, Total, fill = day)) + 
  geom_bar( stat = "identity", position = "dodge", color = "gray") +
  ggtitle("Trips by Day and Month on September") +
  scale_y_continuous(labels = comma) +
  labs(title = "Trip by day in September NYC 2014",
       caption = "Data source: Dataflair") 

#Number of Trips taking place during months in a year
month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)

ggplot( month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors) + 
  labs(title = "Trip by day and month NYC 2014",
       caption = "Data source: Dataflair") 

#Finding out the number of Trips by bases
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  labs(title = "A histogram displaying the number of Trips by bases",
       caption = "Data source: Dataflair") 

#Finding out the number of Trips within a specific bases and month
ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Displaying the number of trips by Bases and Month") +
  scale_fill_manual(values = colors)

#Displaying the number of trips by Bases and DayofWeek
ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values = colors)

#Creating a Heatmap visualization of day, hour and month
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

#Creating a Heatmap visualization by hour and day
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")

#Creating a Heatmap visualization by Month and Day
ggplot(day_month_group, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day")

#Heat Map by Month and Bases
month_base <-  data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarize(Total = n()) 

day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n())

#Heat Map by Month and Bases
ggplot(month_base, aes(Base, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Bases")

#Heat Map by Bases and Day of Week
ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


################################################################################################
#Creating a map visualization of rides in New York
################################################################################################
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004
ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")



################################################
## Modeling                                    #
## KNN                                         #
################################################

# Create separate training and test sets
# 60% of the sample
data_2014 <-  na.omit(data_2014)
smp_size <- floor( 0.6 * nrow(data_2014))

#Set the seed to make partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_2014)), size = smp_size)
train <- data_2014[train_ind, ]
test <- data_2014[-train_ind, ]

#Buiding and triaing model with training data
contSet.knn.train <- train[,2:11] #Dispalying column 2 and 11
contSet.knn.test <- test[,2:11]
contSet.knn.traincl <- train[,6]

contSet.pred.knn.1 <- knn(contSet.knn.train, contSet.knn.test, contSet.knn.traincl)
contSet.eval.knn.1 <- confusionMatrix(contSet.pred.knn.1, test$Base)

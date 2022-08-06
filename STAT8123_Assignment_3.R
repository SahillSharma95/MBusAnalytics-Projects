## STAT8123 ASSIGNMENT 3 - R FILE ##

# Sensor = sensor name
# Date_Time = date time when pedestrian counts are recorded
# Date = date associated with Date_Time
# Time = Time of day, 0 to 23
# Count = Hourly counts of pedestrians

library(tidyverse)
install.packages("lubridate")
install.packages("ggalluvial")
install.packages("ggalt")
citation("ggalt")
install.packages("modelr")
citation("modelr")
library(lubridate)
library(ggalluvial)
library(modelr)
options(scipen = 999)
getwd()
setwd("/Users/sahillsharma/Desktop/SAHILL/Master of Business Analytics/STAT8123/Assessments/Assignment 3")

# Import datafile
pedestrians_2021 <- read_csv("pedestrians_2021.csv")
head(pedestrians_2021,n=5)
View(pedestrians_2021)

# QUESTION 1
# Make a time series plot of pedestrian counts and explain what you learn about the daily pedestrian pattern
# in inner city of Melbourne in 2021?

q1 <- pedestrians_2021 %>%
  group_by(Date) %>%
  summarise(ped_count = sum(Count))

## USE THIS ONE
ggplot(q1, aes(Date, ped_count)) + geom_line(colour = "blue") + labs(title = "Time-series of Pedestrians vs Year - 2021", 
                                                    x = "Month (2021)", y = "Pedestrian Count")

# ggplot(pedestrians_2021, aes(x = Date, y = Count)) + geom_line() + labs(title = "Time-series of Pedestrians vs Year - 2021", x = "Month (2021)", y = "Pedestrian Count")
                                                                        

# In 2021, it certainly appears that pedestrian activity was heavier between the months of April and July, with significantly lower
# volumes in the months prior to, and following this period. This variability can almost certainly be attributed to the multiple
# lockdowns enforced due to the COVID-19 pandemic. 



# QUESTION 2
# Create plots of the monthly and weekly pedestrian counts. a) Which month has the largest pedestrian count in 2021? 
# ANS: April is the month with the highest pedestrian count in 2021 (16,696,489), followed closely by March (14,644,469). 


## FOR MONTHLY ##
# Add month and day columns to dataset
pedestrians_2021_month_day <- pedestrians_2021 %>%
  mutate(month = month(Date, label=TRUE, abbr=TRUE)) %>%
  mutate(day = wday(Date, label=TRUE, abbr=TRUE)) 

# bar chart of month
monthly <- pedestrians_2021_month_day %>%
  group_by(month) %>%
  summarise(ped_count = sum(Count)) %>%
  arrange(desc(ped_count))

ggplot(monthly, aes(month, ped_count, fill = month)) + geom_bar(stat = "identity") + labs(title = "Pedestrian Count by Month - 2021", 
                                                                            x = "Month", y = "Pedestrian Count")

# bar chart of day
daily <- pedestrians_2021_month_day %>%
  group_by(day) %>%
  summarise(ped_count = sum(Count)) %>%
  arrange(desc(ped_count))

ggplot(daily, aes(day, ped_count, fill = day)) + geom_bar(stat = "identity") + labs(title = "Pedestrian Count per Day - 2021",
                                                                        x = "Day of Week", y = "Pedestrian Count")




# QUESTION 3
# Use the alluvial plot to compare the pedestrian traffic flow at Melbourne Central, New Quay and Southbank for their day-of-week pattern.
# get day-of-week information for each location

unique(pedestrians_2021_month_day$Sensor)

# all locations
all_locns <- pedestrians_2021_month_day %>%
  filter(Sensor %in% c("Melbourne Central", "New Quay", "Southbank"))

# group by sensor and day for these locations
all_locns_grouped <- all_locns %>%
  group_by(Sensor, day) %>%
  summarise(ped_count = sum(Count)) %>%
  arrange(desc(ped_count))
                                    
# Alluvial plot

p1<-ggplot(as.data.frame(all_locns_grouped),
           aes(y = ped_count, axis1 = Sensor, axis2 = day)) +
  geom_alluvium(aes(fill = day),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("Sensor", "Day")) +
  ggtitle("Pedestrian Travel in Southbank, New Quay, and Central - by Day") + ylab("Count") + xlab("Stratum")
p1

## QUESTION 4 ##
# Create a dumbbell chart to compare the hourly pedestrian counts in January and April 2021 at Southern
# Cross Station.

# filter for Southern Cross Station
q4_data <- pedestrians_2021_month_day %>%
  filter(Sensor == "Southern Cross Station")

# Southern Cross Station January and April data
q4_jan_apr <- q4_data %>%
  filter(month %in% c("Jan", "Apr") & Time %in% c(0, 23))

# Group data by month and Time (hour)
q4_grouped <- q4_jan_apr %>%
  group_by(month, Time) %>%
  summarise(ped_count = sum(Count))

glimpse(q4_grouped)
# convert data to 'wide' format for dumbbell chart
q4_pivot <- pivot_wider(q4_grouped,
                        names_from = Time,
                        values_from = ped_count)

# assign names for chart
names(q4_pivot) <- c("month", "hour0", "hour23")

library(ggalt)
# Dumbbell chart for Passenger Count Change per Year - Jan to Apr
ggplot(q4_pivot, aes(y = month, x = hour0, xend = hour23)) +
  geom_dumbbell(size = 1.2,size_x = 3, size_xend = 3,colour = "grey",
                colour_x = "#FAAB18", colour_xend = "#1380A1") + theme_minimal() + 
  labs(title = "Change in Hourly Passenger Count at Southern Cross Station",
       x = "Passenger Count (per Hour)",y = "Month")
       
                                                                    
  

## QUESTION 5 ##
# Using box plots to compare the monthly pattern of the pedestrian counts. Then fit a linear model for each month for pedestrian counts and comment on the performance of the model with residual plots. [You can use
# add_residuals function from moldelr package to indicate residuals in your plot].

# obtain count of each date, within each Month. This will allow boxplot to display spread of data
q5 <- pedestrians_2021_month_day %>%
  group_by(Date, month) %>%
  summarise(count = sum(Count))

# boxplot of Pedestrian Count vs Month
ggplot(q5, aes(month, count)) + geom_boxplot()

# Fitting of Linear Model
mod <- lm(count ~ month+Date, data = q5)
grid <- q5 %>% data_grid(q5) %>%
  add_predictions(mod, "count")

# Overlay onto original box plot
ggplot(q5, aes(month, count)) + geom_boxplot() +geom_point(data = grid, colour = "red", size = 4)

# add the residuals to the q5 data frame
q5 <- q5 %>% add_residuals(mod)

# residual plot (get the over/under fitting for each date within each month)
q5 %>% ggplot(aes(Date, resid)) + geom_ref_line(h = 0) + geom_line() +
  labs(title = "Residual Plot for Q5 Linear Model")

summary(mod)



## QUESTION 6 ##
# Filter out ten dates where the model from Question 5 fails, overestimating and underestimating the pedestrian count, respectively. Use what you know about Melbourne or the internet to explore the possible reasons for
# the misfits. What does it suggest for your modelling? Is a linear model a good choice for this data


## All Melbourne Lockdowns ##
# Lockdown 1: 30/03/2020 - 12/05/2020
# Lockdown 2: 08/07/2020 - 27/10/2020
# Lockdown 3: 12/02/2021 - 17/02/2021
# Lockdown 4: 27/05/2021 - 10/06/2021
# Lockdown 5: 15/07/2021 - 27/07/2021

# max 5 dates that were underestimated (01/05), (01/04), (01/08), (01/02), (01/06)
q5 %>% filter(Date == "2021-01-26") # overfit = 20,060 (Australia Day)
q5 %>% filter(Date == "2021-02-14") # underfit = -212,726 (Valentine's Day)
q5 %>% filter(Date == "2021-02-19") # overfit = 41,054 (2 days after lockdown 3 ended)
q5 %>% filter(Date == "2021-03-08") # underfit = -96,949 (Labour Day Public Holiday)
q5 %>% filter(Date == "2021-04-25") # underfit = -15,484 (Anzac Day)
q5 %>% filter(Date == "2021-05-27") # underfit = -58,653 (Start of Lockdown 4)
q5 %>% filter(Date == "2021-06-14") # overfit = 2,521 (Queen's Birthday Public Holiday)
q5 %>% filter(Date == "2021-07-16") # underfit = -197,737 (Day 2 of Lockdown 5)
q5 %>% filter(Date == "2021-07-30") # overfit = 46,074 (2 days after lockdown 5 ended)
q5 %>% filter(Date == "2021-09-24") # underfit = -7,834 (AFL Grand Final Parade)

q5$Date <- as.character(q5$Date)
q5_dates <- q5 %>%
  filter(Date %in% c("2021-01-26",
                     "2021-02-14",
                     "2021-02-19",
                     "2021-03-08",
                     "2021-04-25",
                     "2021-05-27",
                     "2021-06-14",
                     "2021-07-16",
                     "2021-07-30",
                     "2021-09-24")) %>%
  mutate(resid_sum = count + resid)

q5_dates$Date <- as.Date(q5_dates$Date)

# residual plot of the selected dates
ggplot(q5_dates, aes(Date, resid)) + geom_ref_line(h = 0) + geom_line()


## QUESTION 7 ##
# Relationship between count (dependent) and sensors and day (independent), at time = 12

# Filter the data to get pedestrian counts where Time == 12
q7_data <- pedestrians_2021_month_day %>%
  filter(Time %in% c(12))

# linear model, with Sensor and day as the predictors
q7_model <- lm(Count ~ Sensor + day, data = q7_data)
summary(q7_model)

# Generation of model diagnostic plots
par(mfrow = c(2,2))
plot(q7_model)

# use broom to tidy up the data
tidy(q7_model)


citation("broom")
citation("lubridate")

## STAT8123 ASSIGNMENT 2 CODE ##
install.packages("formatR")
tinytex::install_tinytex()
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60), tidy = TRUE)
getwd()
setwd("/Users/sahillsharma/Desktop/SAHILL/Master of Business Analytics/STAT8123/Assessments/Assignment 2/Assignment 2 Files")
library(tidyverse)
library(sf)
options(scipen = 9999)
View(zooplankton)
head(zooplankton)
colSums(is.na(zooplankton))  # there are some NAs in this dataset - may need to remove
zooplankton <- read_csv("zooplankton.csv")
## Question 1 ##
# Which zooplankton species was most abundant overall?
# The most abundant zooplankton species in this study was Oithona_similis (2,366,224), followed closely by Calanoida_inlet (1,937,377)
# The least abundant zooplankton species in this study was Calanus_simillimus (351,673). 
sum(zooplankton$Calanoida_indet)
sum(zooplankton$Calanus_simillimus)
sum(zooplankton$Foraminifera_indet)
sum(zooplankton$Fritillaria_sp)
sum(zooplankton$Oithona_similis)

species <- c("Calanoida_indet", "Calanus_simillimus", "Foraminifera_indet", "Fritillaria_sp", "Oithona_similis")
number <- c(sum(zooplankton$Calanoida_indet), sum(zooplankton$Calanus_simillimus), sum(zooplankton$Foraminifera_indet),
            sum(zooplankton$Fritillaria_sp), sum(zooplankton$Oithona_similis))


species.data <- data.frame(species, number)
ggplot(species.data, aes(x = species, y = number)) + geom_bar(stat = "identity") + ggtitle("Abundance of each species") +
  xlab("Species Type") + ylab("Count of each species")


## Question 2 ##
# How did average total abundance change by year? [Note that each year a different number of segments of
                                                 # different lengths were taken; this needs to be taken into account].

zooplankton_2 <- zooplankton %>%
  group_by(Year) %>%
  summarise(count = n(), total_segment_length = sum(Segment_Length), total_abundance = sum(Total_abundance)) %>%
  mutate(average_per_segment_length = round(total_abundance/total_segment_length,2))


ggplot(zooplankton_2, aes(x = Year, y = average_per_segment_length)) + geom_line(colour = "blue") + ggtitle("Scatterplot of Average Abundance Per Segment Length By Year") +
  ylab("Average Abundance")
  

# The average abundance per segment length appears to have increased steadily from 1995 until around 2010, where the abundance began to plateau and
# eventually experience a sharp decline until 2020. Since then, the abundance has experienced a moderate growth phase. 


## Question 3 ##
# Is there a change in average water temperature over time? How could the graphic be misleading and how
# could you fix it? [Explain in words, provide plot(s) if possible].
# There are NA/blank temperatures recorded. One way to fix this could be to replace NA with average values for the year. 

zooplankton3 <- zooplankton %>%
  group_by(Year) %>%
  summarise(count = n(), total_temp = sum(Water_Temperature, na.rm=TRUE)) %>%
  mutate(avg_temp = total_temp/count)

ggplot(zooplankton3, aes(x = Year, y = avg_temp)) + geom_line() # The graph is definitely misleading. The 2019 and 2020 yearly data contains significantly
# fewer segments (i.e. sample size) than the other years, hence the dramatic positive skew in average sea temp for these years. 
# Due to the lack of available information for these years, I would rectify the misleading graphic by applying the average temp of all previous years (i.e
# as the values for 2019 and 2020), in order to ensure the graphic provides a more realistic interpretation of the data.

# adjust the 2020 temp
zooplankton3[zooplankton3$Year==2020, "avg_temp"] <- sum(zooplankton3$avg_temp)/nrow(zooplankton3)

# adjust the 2021 temp
zooplankton3[zooplankton3$Year==2021, "avg_temp"] <- sum(zooplankton3$avg_temp)/nrow(zooplankton3)

zooplankton3_new <- zooplankton3
ggplot(zooplankton3_new, aes(x = Year, y = avg_temp)) + geom_line() # This graph looks much more even, and provides a much better real-world representation of
# average temperature for 2020 and 2021. 

## Question 4 ##
# For 2019 only, display the proportion of each plankton species observed in each observation month. Provide
# an summary in words. Is the same species most abundant in all observation months?
zooplankton_2019 <- zooplankton %>%
  select(Year, Month, Calanoida_indet, Calanus_simillimus, Foraminifera_indet, Fritillaria_sp, Oithona_similis) %>%
  filter(Year == 2019)

zooplankton_2019 %>%
  group_by(Year, Month) %>%
  summarise(calanoida_indet = sum(Calanoida_indet), calanus_simillimus = sum(Calanus_simillimus), 
            foraminifera_indet = sum(Foraminifera_indet), fritillaria_sp = sum(Fritillaria_sp), 
            oithona_similis = sum(Oithona_similis))

DF <- data.frame(Month = c("December", "January", "March"), 
                 calanoida_inlet = c(850, 11109, 3241), 
                 calanus_simillimus = c(3, 4766, 0), 
                 foraminifera_indet = c(0, 2382, 27), 
                 fritillaria_sp = c(92, 2998, 153), 
                 oithona_similis = c(431, 19158, 1363))

DFtall <- DF %>%
  gather(key = Species, value = Count, calanoida_inlet:oithona_similis)
DFtall

ggplot(DFtall, aes(Month, Count, fill = Species)) + geom_col(position = "dodge")
species_2019 <- c("Calanoida_indet", "Calanus_simillimus", "Foraminifera_indet", "Fritillaria_sp", "Oithona_similis")
number_2019 <- c(sum(zooplankton_2019$Calanoida_indet), sum(zooplankton_2019$Calanus_simillimus), sum(zooplankton_2019$Foraminifera_indet),
            sum(zooplankton_2019$Fritillaria_sp), sum(zooplankton_2019$Oithona_similis))

# Species Calanoida_inlet is the most abundant species in January, while species Oithona_similis is most abundant in January and March. 
# January appears to be the most favourable month for zooplankton species overall
# In December, there were no Calanus_simillimus or foraminifera_indet, while in March, there were no Calanus_simillimus
# the higher numbers in January can almost certainly be attributed to the far higher number of segments in this month. 

## Question 5 ##
# Create a map of Antarctica using the TM_WORLD_BORDERS_SIMPL-0.3.shp file. Add the zooplankton segment
# sample observations and information about the ship that made the observation. From the graphic, determine
# which ship made the most observations and which ship probably made the least.

# {r, echo = TRUE, message = FALSE, eval=TRUE, warning = FALSE, fig.height=8, fig.width=8, fig.cap = c( "Original Plot With No Amendments" )}




getwd()
ant_shp <- st_read("TM_WORLD_BORDERS_SIMPL-0.3.shp")
unique(ant_shp$NAME) # "Antarctica"

colors()
ant_map <- filter(ant_shp, NAME == "Antarctica")


ggplot(ant_map) + geom_sf(fill = "seagreen4", alpha=0.2) +
  geom_point(data=zooplankton, aes(x = zooplankton$Longitude, y = zooplankton$Latitude, colour = zooplankton$Ship_Code),
             size=0.5, alpha=0.2) + scale_color_manual(values = c("red", "green", "orange", "blue", "purple", 
                                                                                     "pink", "yellow", "blue", "magenta", "black", 
                                                                                     "grey", "maroon")) +
  ggtitle("Map of ships studying in Antarctica") +
  xlab("Antarctica Longitude") +
  ylab("Antarctica Latitude")

zooplankton5 <- zooplankton %>%
  group_by(Ship_Code) %>%
  summarise(count = n())

unique(zooplankton$Ship_Code)


## Question 6 ##
# Clearly define your own research question related to the provided data set, then make an analysis graphically
# and report your finding. Higher marks will be awarded to a well motivated question and insightful discovery.

zooplankton6f <- zooplankton %>%
  group_by(Year) %>%
  summarise(count = n(), total_fluor = sum(Fluorescence, na.rm=TRUE)) %>%
  mutate(avg_fluor = total_fluor/count)

zooplankton6s <- zooplankton %>%
  group_by(Year) %>%
  summarise(count = n(), total_salinity = sum(Salinity, na.rm=TRUE)) %>%
  mutate(avg_salinity = total_salinity/count) %>%
  mutate(avg_fluor = zooplankton6f$total_fluor/count)

# fix up averages for 2020 and 2021
# adjust the 2020 fluro
zooplankton6s[zooplankton6s$Year==2020, "avg_fluor"] <- 
  sum(zooplankton6s$avg_fluor)/nrow(zooplankton6s)

# adjust the 2021 fluro
zooplankton6s[zooplankton6s$Year==2021, "avg_fluor"] <- 
  sum(zooplankton6s$avg_fluor)/nrow(zooplankton6s)

# adjust the 2020 salinity
zooplankton6s[zooplankton6s$Year==2020, "avg_salinity"] <- 
  sum(zooplankton6s$avg_salinity)/nrow(zooplankton6s)

# adjust the 2021 salinity
zooplankton6s[zooplankton6s$Year==2021, "avg_salinity"] <- 
  sum(zooplankton6s$avg_salinity)/nrow(zooplankton6s)

zooplankton6s <- zooplankton6s %>%
  mutate(avg_temp = zooplankton3_new$avg_temp)

# ggplot with both salinity and fluorescence (x axis = average temp)
ggplot(zooplankton6s, aes(x = avg_temp)) + geom_line(aes(y = avg_fluor), color = "darkred") +
  geom_line(aes(y = avg_salinity), color = "steelblue", linetype = "twodash") +
  labs(x = "Average Temperature",
       y = "Average Salinity/Fluorescence",
       color = "Legend") +
  scale_color_manual(values = colors)


 




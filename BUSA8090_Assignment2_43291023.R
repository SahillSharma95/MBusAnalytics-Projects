### BUSA8090 ASSIGNMENT 2 R CODE - SAHILL SHARMA (SID = 43291023) ###


library(pacman)
library(tidyverse)


# Import the dataset
df = read_csv(
  file = 'Music Sales (Music Store.xlsx).csv')

# Q1 Visualisation - Which genre sells the most?

## EUROPE ##
df_euro <- df %>%
  select(Genre, Quantity, Region) %>%
  group_by(Genre, Region) %>%
  filter(Region == 'Europe') %>%
  summarise(sum = sum(Quantity))

# Bar Chart displaying which genre sells the most - EUROPE
ggplot(df_euro, aes(x = reorder(Genre, sum), y = sum, fill=Genre)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Sum Of Each Genre - Europe") +
  coord_flip() +
  xlab("Genre Type") + ylab("Count Of Each Genre")

## ASIA ##
df_asia <- df %>%
  select(Genre, Quantity, Region) %>%
  group_by(Genre, Region) %>%
  filter(Region == 'Asia') %>%
  summarise(sum = sum(Quantity))

# Bar Chart displaying which genre sells the most - ASIA
ggplot(df_asia, aes(x = reorder(Genre, sum), y = sum, fill=Genre)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Sum Of Each Genre - Asia") +
  coord_flip() +
  xlab("Genre Type") + ylab("Count Of Each Genre")

# The genre which sells the most in Europe is 'Rock'. 
# The genre which sells the most in Asia is also 'Rock'. Therefore, both Europe and Asia share a common Genre which sells the most, that being 'Rock'. 

# Q2 - TOP 20 ARTISTS EUROPE AND ASIA

#### EUROPE ####
df2_euro <- df %>%
  select(Artist, Quantity, Region) %>%
  group_by(Artist, Region) %>%
  filter(Region == 'Europe') %>%
  summarise(sum = sum(Quantity)) %>%
  arrange(desc(sum))

# Get only the top 20 artists from df2_euro
df2_euro <- df2_euro %>%
  head(df2_euro, n = 20)

# Horizontal bar chart of Top 20 artists
ggplot(df2_euro, aes(x = reorder(Artist, sum), y = sum, fill=Artist)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Top 20 Artists - Europe") +
  coord_flip() +
  xlab("Genre Type") + ylab("Count Of Each Artist")


#### ASIA ####
df2_asia <- df %>%
  select(Artist, Quantity, Region) %>%
  group_by(Artist, Region) %>%
  filter(Region == 'Asia') %>%
  summarise(sum = sum(Quantity)) %>%
  arrange(desc(sum))

# Get only the top 20 artists from df2_asia
df2_asia <- df2_asia %>%
  head(df2_asia, n = 20)

# Horizontal bar chart of Top 20 artists
ggplot(df2_asia, aes(x = reorder(Artist, sum), y = sum, fill=Artist)) + geom_bar(position = "dodge", stat = "identity") + ggtitle("Top 20 Artists - Asia") +
  coord_flip() +
  xlab("Genre Type") + ylab("Count Of Each Artist")
  

### Q3 - SALES DISTRIBUTION BY GEOGRAPHY ###
### EUROPE ###
df3_euro <- df %>%
  select(Genre, Quantity, Region, City) %>%
  group_by(Genre, Region, City) %>%
  filter(Region == 'Europe') %>%
  summarise(sum = sum(Quantity))

### VISUALISATION - EUROPE ###
ggplot(df3_euro, aes(x = reorder(City, sum), y = sum, fill=Genre)) + geom_bar(position = "stack", stat = "identity") + ggtitle("Sales By City - Europe") +
  coord_flip() +
  xlab("European City") + ylab("Count Of Each Genre By City")


### ASIA ###
df3_asia <- df %>%
  select(Genre, Quantity, Region, City) %>%
  group_by(Genre, Region, City) %>%
  filter(Region == 'Asia') %>%
  summarise(sum = sum(Quantity))

### VISUALISATION - ASIA ###
ggplot(df3_asia, aes(x = City, y = sum, fill=Genre)) + geom_bar(position = "stack", stat = "identity") + ggtitle("Sales By City - Asia") +
  coord_flip() +
  xlab("Asian City") + ylab("Count Of Each Genre By City")

# Rock appears to be, by far, the best selling genre across all cities in Europe, and Asia.

### Q4 - COMPARE SALES DISTRIBUTION. HOW DOES THE SALES DISTRIBUTION OF ROCK TRACKS COMPARE AGAINST ALTERNATIVE AND HEAVY METAL TRACKS? ###
### EUROPE ###
df4_euro <- df %>%
  select(Genre, Quantity, Region, City) %>%
  group_by(Genre, Region, City) %>%
  filter(Region == 'Europe' & Genre %in% c("Rock", "Alternative", "Heavy Metal")) %>%
  summarise(sum = sum(Quantity))

### VISUALISATION - EUROPE ###
ggplot(df4_euro, aes(x = reorder(City, sum), y = sum, fill=Genre)) + geom_bar(position = "stack", stat = "identity") + 
  ggtitle("Rock, Alternative & Heavy Metal Sales - Europe") +
  coord_flip() +
  xlab("European City") + ylab("Count Of Each Genre By City")

# Rock outsells 'Heavy Metal', and 'Alternative', considerably across all cities in the Europe region.

### ASIA ###
df4_asia <- df %>%
  select(Genre, Quantity, Region, City) %>%
  group_by(Genre, Region, City) %>%
  filter(Region == 'Asia' & Genre %in% c("Rock", "Alternative", "Heavy Metal")) %>%
  summarise(sum = sum(Quantity))

### VISUALISATION - ASIA ###
ggplot(df4_asia, aes(x = City, y = sum, fill=Genre)) + geom_bar(position = "stack", stat = "identity") + 
  ggtitle("Rock, Alternative & Heavy Metal Sales - Asia") +
  coord_flip() +
  xlab("Asian City") + ylab("Count Of Each Genre By City")


## The sales distribution of Rock compared to Alternative and Heavy Metal tracks differ significantly both within, and between the two geographic
# regions of Europe and Asia. For Europe, Rock is, by quite a significant margin, the best selling genre in every European city, followed by Alternative and Heavy Metal. Interestingly,
# every city had rock sales, however only three cities (Oslo, Dijon and Berlin) had alternative sales, and only two cities (Porto and Berlin) had
# Heavy Metal sales. 

# For Asia, Rock was the exclusive genre sold, with the greater number of sales occurring in Delhi. Overall, there is a far greater number of record sales
# in Europe compared to Asia, however this could be explained by the fact that Europe has more cities represented in this dataset than Asia. 

### Q5 - ONE VISUALISATION THAT CAN BE USED AS PREDICTIVE ANALYTICS TO IMPROVE BUSINESS DECISION MAKING


### Q: WHAT ARE THE TOP 20 BEST SELLING ALBUMS WORLDWIDE?

# FIRST REMOVE 'M' FROM END OF EACH VALUE, AND CONVERT THE BYTES COLUMN TO NUMERIC


# TOP 20 SELLING ALBUMS WORLDWIDE
df5_2 <- df %>%
  select(Album, Quantity) %>%
  group_by(Album) %>%
  summarize(sum=sum(Quantity)) %>%
  arrange(desc(sum))

df5_2_head <- df5_2 %>%
  head(df5_2, n = 20)

ggplot(df5_2_head, aes(x = reorder(Album, sum), y = sum, fill = Album)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  ggtitle("Top 20 Album Sales - Worldwide") +
  theme(legend.position = "none") +
  coord_flip() +
  xlab("Album Name") + ylab("Sum Of Each Album Sold")

# Top three selling albums = 'Greatest Hits', 'Minha Historia', 'Unplugged'













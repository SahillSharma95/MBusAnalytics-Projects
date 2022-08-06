setwd("/Users/sahillsharma/Desktop/SAHILL/Master of Business Analytics/BUSA8000/Assessments/Quiz 1")

data <- read_csv("data_scientist_hr.csv")
str(data)
unique(data$gender)

# Question 2
data2 <- data %>%
  filter(gender == "Other")

# Question 3 - for female DS, how many had graduate degree listed as their higher level of education
unique(data$education_level)
data3 <- data %>%
  filter(gender == "Female" & education_level == "Graduate")

# Question 4 - How many data scientists are listed as having fewer than 5 years experience in this data set?
unique(data$experience)
data4 <- data %>%
  mutate(experience = replace(experience, experience == "<1", "1"))

unique(data4$experience)
data4$experience <- as.integer(data4$experience)
unique(data4$experience)
data_lt5 <- data4 %>%
  filter(experience < 5)
unique(data_lt5$experience)

# Question 5 - for data scientists with no enrolment in university, what proportion have relevant experience?
unique(data$enrolled_university)
data5 <- data %>%
  filter(enrolled_university == "no_enrollment")

unique(data$relevent_experience)
data5rel <- data5 %>%
  filter(relevent_experience == "Has relevent experience")

# For questions 7, 8 and 9
unique(data$training_hours)
data_lt50 <- data %>%
  filter(training_hours < 50)

data_ge50 <- data %>%
  filter(training_hours >= 50)

# Question 7 - what proportion of data scientists have fewer than 50 hours of training completed?
prop = (9914/19158) * 100
        
# Question 8 - for data scientists with at least 50 hours of training, how many do not have their years of experience listed?
unique(data_ge50$experience)
noexp <- data_ge50 %>%
  filter(is.na(experience))

# Question 9 - for data scientists with less than 50 hours of training, what proportion of data scientists have at most 10 years of experience?
unique(data_lt50$experience)
lt50_10 <- data_lt50 %>%
  mutate(experience = replace(experience, experience == "<1", "1"))

lt50_10$experience <- as.integer(lt50_10$experience)
unique(lt50_10$experience)

exp_lt10 <- lt50_10 %>%
  filter(experience <= 10)

# Question 10 - there are 460 missing entries in the "education level" column. Will use the knn algorithm to estimate these values. 
# predicted value was GRADUATE for the student. 

# Question 11
# First, for data scientists with STEM as their major discipline, calculate the proportion (as a %) that are looking for new jobs. Then do the same for a Business discipline
unique(data$major_discipline)
stem <- data %>%
  filter(major_discipline == "STEM")

stem_NJ <- stem %>%
  filter(target == 1)

business <- data %>%
  filter(major_discipline == "Business Degree")

business_NJ <- business %>%
  filter(target == 1) 

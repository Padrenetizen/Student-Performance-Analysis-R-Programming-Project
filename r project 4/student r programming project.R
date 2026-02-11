# loading the libraries
library(tidyr)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

# loading the dataset
student <- read.csv("student_data.csv")
head(student)
str(student)

# removing missing values
student <- student %>% drop_na()

# checking the no of rows removed
n_before <- nrow(student)
student <- student %>% drop_na()
n_after <- nrow(student)
n_before - n_after
# none were removed

# exploratory data analysis


# removing duplicates
student <- student |> distinct()

# checking for left over duplicates
nrow(student)-nrow(distinct(student))

summary(student)

# converting character columns to logical
logical_cols <- c(
  "schoolsup", "famsup", "paid", "activities", "nursery", 
  "higher", "internet", "romantic"
)
student <- student |> mutate(across(all_of(logical_cols),
                                    ~ .x == "yes"))

student <- student %>%
  mutate(across(c(Mjob, Fjob, reason, guardian), 
                str_to_title))

student <- student %>%
  mutate(across(c(Mjob), 
                ~ str_to_sentence(gsub("_", " ", .))))

# saving the cleaned version
write.csv(student, "student_cleaned.csv", 
          row.names = FALSE)

# loading in the cleaned dataset
student_data <- read.csv("student_cleaned.csv")
head(student_data)
str(student_data)

# exploratory data analysis
# top 10 performance by guardian
performance_by_guardian <- student_data %>%
  group_by(guardian) %>%
  summarise(Total_grade = sum(G1 + G2 + G3)) %>%
  arrange(desc(Total_grade)) %>%
  head(3)
performance_by_guardian
# this reveals that students who were raised by their mothers had the highest grade

# internet usage and it's contribution to educational performance
internet_by_total_grade <- student_data %>%
  group_by(internet) %>%
  summarise(Total_grade = sum(G1 + G2 + G3)) %>%
  arrange(desc(Total_grade)) %>%
  head(2)
internet_by_total_grade
# internet access had an important role to play in the academic performance of the students

#nursery students count
nursery_count <- student_data %>%
  group_by(nursery) %>%
  summarise(Student_count = n()) %>%
  arrange(desc(Student_count))
nursery_count

# filtering for only the true nursery values
nursery_count %>%
  filter(nursery == TRUE)
  
# higher education student count
higher_student_count <- student_data %>%
  filter(higher == TRUE) %>%
  summarise(school = n())
higher_student_count

# reasons for enrollment by total grades
reason_by_total_grades <- student_data %>%
  group_by(reason) %>%
  summarise(Total_grades = sum(G1 + G2 + G3)) %>%
  arrange(desc(Total_grades)) %>%
  head(6)
reason_by_total_grades

# internet by failures
internet_by_failures <- student_data %>%
  group_by(internet) %>%
  summarise(Total_failures = sum(G1 + G2 + G3)) %>%
  arrange(desc(Total_failures)) %>%
  head(3)
internet_by_failures
# this shows that internet played a great role in the failures of the student

internet_by_failures$percent <- internet_by_failures$Total_failures /
  sum(internet_by_failures$Total_failures) * 100
internet_by_failures

#study time by mother and father education
par(mfrow = c(2, 1))
boxplot(student_data$Medu ~ student_data$studytime,
        main = "Study Time By Mother's Education",
        xlab = "Mother's Education Level",
        ylab = "Study Time",
        col = "red" )

boxplot(student_data$Fedu ~ student_data$studytime,
        main = "Study Time By Mother's Education",
        xlab = "Father's Education Level",
        col = "brown")
    
  
#failures by travel time scatter or histogram


# reason for joining and total grades
ggplot(reason_by_total_grades, aes(x = reason, y = Total_grades)) +
  geom_col(fill = "#027DBF") +
  labs(title = "Total Grades By Reason",
      x = "Reason",
      y = "Total Grades") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# this chart shows that the students that genuinely picked the school for their personal interest, get better grades compared to those who had other reasons for joining


#internet by failures pie chart
ggplot(internet_by_failures, aes(x = "", y = Total_failures, fill = internet)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(round(percent, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  labs(
    title = "Failure Distribution By Internet",
    fill = "Internet"
  ) +
  theme_void()


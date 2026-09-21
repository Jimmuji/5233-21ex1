#--------------------------------------------------
# Week 1: In-class assignment
#--------------------------------------------------

# There is no one correct way to write the code to answer the questions
# But your code needs to 
# a. answer the question
# b. be fully reproducible

# For this assignment, we will use 
# the `yrbss` data 
# in the `openintro` package 

install.packages("openintro")
library(openintro)

# Other useful packages
install.packages("tidyverse")
library(tidyverse)

# Read the documentation for `yrbss` to learn about all the variables.
?yrbss

# The code below uses the `flextable` package to create a table of summary characteristics of
# Grade and Gender
# Modify the code below such that the grade shows in increasing order
# and all category labels start with a capital letter

install.packages("flextable")
library(flextable)

yrbss$Grade <- factor(yrbss$grade,
                      levels = c("9", "10", "11", "12", "other"),
                      labels = c("9", "10", "11", "12", "Other"))
yrbss$Gender <- factor(yrbss$gender,
                       levels = c("female", "male"),
                       labels = c("Female", "Male"))

z <- summarizor(
  yrbss[c("Grade", "Gender")],
  overall_label = NULL
)
ft_1 <- as_flextable(z) 
ft_1


# To understand the pattern of physical activity by grade and gender,
# 1) aggregate  `physically_active_7d` by calculating its mean within each grade and gender
# 2) create a plot showing the average number of physically active days
#      x-axis: grade
#      y-axis: Mean of `physcially_active_7d`
#      Distinguish gender using different colors, symbols, or lines
# *** I would use the following functions: aggregate(), ggplot(), geom_line() but there is 
# no one correct way to do this
# Ensure that the figure is clearly labeled and includes an appropriate legend

aggregate(physically_active_7d ~ Grade + Gender,
          data = subset(yrbss, Grade != "Other"),
          FUN = mean) |>
  ggplot(aes(x = Grade, y = physically_active_7d,
             color = Gender, shape = Gender, group = Gender)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Average number of physically active days by grade and gender",
       x = "Grade",
       y = "Mean number of days physically active (past 7 days)",
       color = "Gender", shape = "Gender") +
  theme_minimal()


# Create a plot that shows the relationship betwen physical activity and bmi
# among female students in grade 12 
# Ensure that the figure is clearly labeled and includes an appropriate legend

yrbss |>
  filter(Gender == "Female", Grade == "12") |>
  mutate(bmi = weight / height^2) |>
  filter(!is.na(bmi), !is.na(physically_active_7d)) |>
  ggplot(aes(x = factor(physically_active_7d), y = bmi,
             fill = factor(physically_active_7d))) +
  geom_boxplot() +
  labs(title = "BMI by physical activity among female students in grade 12",
       x = "Number of days physically active (past 7 days)",
       y = "BMI (kg/m^2)",
       fill = "Days physically active") +
  theme_minimal()

# Push your completed code to your GitHub repository

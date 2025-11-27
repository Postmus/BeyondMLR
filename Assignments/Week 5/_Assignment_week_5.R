
# Load the required libraries
library(dplyr)
library(ggplot2)
library(lmerTest)

jsp_data <- read.csv("downloads/jsp_data.csv")

# Convert all character variables into factors
jsp_data <- jsp_data %>%
  mutate(across(where(is.character), as.factor))

# Inspect the structure of the dataset
str(jsp_data)

###

# str(jsp_data)
# 'data.frame':	3236 obs. of  8 variables:
#   $ School               : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Class                : int  1 1 1 1 1 1 1 1 1 1 ...
# $ Gender               : Factor w/ 2 levels "Boy","Girl": 2 2 2 1 1 1 1 1 1 1 ...
# $ Ravens.test.in.year.1: int  23 23 23 15 15 22 22 22 14 14 ...
# $ Student.ID           : int  1 1 1 2 2 3 3 3 4 4 ...
# $ English.test         : int  72 80 39 7 17 88 89 83 12 25 ...
# $ Mathematics.test     : int  23 24 23 14 11 36 32 39 24 26 ...
# $ Junior.school.year   : int  0 1 2 0 1 0 1 2 0 1 ...

###

# Add a factor variable version of Junior.school.year
jsp_data <- jsp_data %>%
  mutate(Junior.school.year.factor = factor(Junior.school.year))

### Exploratory data analysis

# Summarize the nesting of students within schools
jsp_data %>%
  group_by(School) %>%
  summarise(n_students = n_distinct(Student.ID))

# Group the data by Junior.school.year (numeric) and Gender, and calculate the mean English test score and store in a different r object
data_grouped <- jsp_data %>%
  group_by(Junior.school.year, Gender) %>%
  summarise(mean_English_test = mean(English.test, na.rm = TRUE),
            sd_English_test = sd(English.test, na.rm = TRUE),
            mean_Mathematics_test = mean(Mathematics.test, na.rm = TRUE),
            sd_Mathematics_test = sd(Mathematics.test, na.rm = TRUE),
            n = n())

# Plot the mean English test score by Junior.school.year and colour by Gender
data_grouped %>%
  ggplot(aes(x = Junior.school.year, y = mean_English_test, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Mean English test score by Junior school year",
    x = "Junior school year",
    y = "Mean English test score"
  )

# Plot the mean mathematics test score by Junior.school and colour by Gender
data_grouped %>%
  ggplot(aes(x = Junior.school.year, y = mean_Mathematics_test, color = Gender)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Mean Mathematics test score by Junior school year",
    x = "Junior school year",
    y = "Mean Mathematics test score"
  )

jspr <- jsp_data

full_model_eng <- lmer(English.test ~ poly(Junior.school.year,2) + Gender + poly(Junior.school.year, 2) * Gender + (1| School/Student.ID),data = jspr)
summary(full_model_eng)

linear_model_eng <-lmer(English.test ~ Junior.school.year + Gender + Junior.school.year * Gender + (1 | School/Student.ID),data = jspr)
anova(linear_model_eng, full_model_eng, refit = TRUE)

### Fit linear mixed effects models for the English test scores

# Fit the random intercept model wih nesting of students within schools, ignoring classes
model_ri <- lmer(English.test ~ Junior.school.year + (1 | School/Student.ID), data = jsp_data)
summary(model_ri)

model_ri_2 <- lmer(English.test ~ Junior.school.year.factor + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_2)

model_ri_3 <- lmer(English.test ~ Junior.school.year.factor*Gender + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_3)

anova(model_ri_2, model_ri_3, refit=T) # Significant interaction between Junior.school

model_ri_4 <- lmer(English.test ~ Junior.school.year.factor*Gender + (1 + Junior.school.year.factor| School) + (1 | School:Student.ID), data = jsp_data)
summary(model_ri_4)

anova(model_ri_3, model_ri_4, refit=F) 

### Evaluate estimated marginal means

ls_means(model_ri_3)
ls_means(model_ri_3, pairwise = T)

# Create a scatter plot of the conditional residuals agianst the linear predictor (fitted value)
conditional.residuals <- resid(model_ri_4, type = "response")
linear.predictor <- predict(model_ri_4, re.form = NULL)
plot(linear.predictor, conditional.residuals, xlab = "Fitted values", ylab = "Conditional residuals")

qqnorm(conditional.residuals)
qqline(conditional.residuals)

### Fit linear mixed effects models for the mathematics test scores

model_ri_math <- lmer(Mathematics.test ~ Junior.school.year + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_math)

model_ri_math.f <- lmer(Mathematics.test ~ Junior.school.year.factor + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_math.f)

model_ri_math.2 <- lmer(Mathematics.test ~ Junior.school.year * Gender + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_math.2)

model_ri_math.2.f <- lmer(Mathematics.test ~ Junior.school.year.factor * Gender + (1 | School/Student.ID), data = jsp_data)
summary(model_ri_math.2.f)

anova(model_ri_math.2.f, model_ri_math.f, refit=T)

model_ri_math.3 <- lmer(Mathematics.test ~ Junior.school.year * Gender + (1 | Student.ID), data = jsp_data)
summary(model_ri_math.3)

anova(model_ri_math.2, model_ri_math.3, refit=F)

ls_means(model_ri_math.f)
ls_means(model_ri_math.f, pairwise = T)

conditional.residuals <- resid(model_ri_math, type = "response")
linear.predictor <- predict(model_ri_math, re.form = NA)
plot(linear.predictor, conditional.residuals, xlab = "Fitted values", ylab = "Conditional residuals")

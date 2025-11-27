library(dplyr)
library(ggplot2)
library(haven)
library(lmerTest)

### Analysis of the growth data ###

# The dataset contains longitudinal data on the growth curves of 100 boys.
# The dataset includes the following variables:
# - id: Unique
# - height: Height in cm
# - age: Age in weeks
# - bf : Breastfeeding status (1 = exclusive breast feeding, 0 = otherwise)
# - sm : Smoking status of mother (1 = Smoker, 0 = Non-smoker)
# - bw : Birth weight in grams

### Data preparation ###

# Load data
growth <- read_sav("growth.sav")

# Convert labelled data to factors
growth$bf <- as_factor(growth$bf)
growth$sm <- as_factor(growth$sm)

### Exploratory data analysis ###

# Individual trajectories of height growth with data points for sampled individuals

# Sample 20 unique individuals
set.seed(123)  # Set seed for reproducibility
sampled_ids <- sample(unique(growth$id), 20)

# Filter data for sampled individuals
sampled_growth <- growth %>%
  filter(id %in% sampled_ids)

# Plot individual trajectories with data points
sampled_growth %>%
  ggplot(aes(x = age, y = height, group = id)) +
  geom_line() +
  geom_point() +
  labs(title = "Individual Growth Trajectories (random sample of 20 boys)",
       x = "Age (weeks)", y = "Height (cm)") +
  theme_minimal()

### Random intercept model ###

# center the age variable
growth <- growth %>%
  mutate(age_centered = age - mean(age, na.rm = TRUE))

# Fit a random intercept model to account for individual variability in height
basic_growth_model_1 <- lmer(height ~ age_centered + (1 | id), data = growth)
summary(basic_growth_model_1)

# Fit a random slope model to account for individual variability in growth rate
basic_growth_model_2 <- lmer(height ~ age_centered + (age_centered | id), data = growth)
summary(basic_growth_model_2)

# Resolve convergence issues by changing the optimizer
basic_growth_model_2 <- lmer(
  height ~ age_centered + (age_centered | id),
  data = growth,
  control = lmerControl(optimizer = "bobyqa")
)
summary(basic_growth_model_2)

# Compare the models using ANOVA
anova(basic_growth_model_1, basic_growth_model_2, refit = FALSE)

## Quadratic growth model

# Add a quadratic term for age to the dataset
growth <- growth %>%
  mutate(age_centered_squared = age_centered^2)

# Fit a quadratic growth model
quadratic_growth_model <- lmer(height ~ age_centered + age_centered_squared + (age_centered | id), data = growth, 
                               control = lmerControl(optimizer = "bobyqa"))
summary(quadratic_growth_model)

# Compare the quadratic model with the linear model
anova(basic_growth_model_2, quadratic_growth_model, refit = TRUE)

# Add a random slope for the quadratic term
quadratic_growth_model_2 <- lmer(height ~ age_centered + age_centered_squared + (age_centered + age_centered_squared | id), data = growth, 
                               control = lmerControl(optimizer = "bobyqa"))
summary(quadratic_growth_model_2)

# Compare the models
anova(quadratic_growth_model, quadratic_growth_model_2, refit = FALSE)


### Exploring the effect of breastfeeding status and smoking status ###

# exploratory data analysis

# Calculate the average height growth by breastfeeding status
growth_summary_bf <- growth %>%
  group_by(bf, age) %>%
  summarise(mean_height = mean(height, na.rm = TRUE))

# Calculate the average height growth by smoking status
growth_summary_sm <- growth %>%
  group_by(sm, age) %>%
  summarise(mean_height = mean(height, na.rm = TRUE))

# Create a line plot of the average height growth by breastfeeding status and store it in the r object p1
p1 <- growth_summary_bf %>%
  ggplot(aes(x = age, y = mean_height, color = bf)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) + # Use linewidth for lines
  labs(title = "Smoothed Average Height Growth by Breastfeeding Status",
       x = "Age (weeks)", y = "Mean Height (cm)", color = "Breastfeeding Status") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(0, 50, 10))


# Create a line plot of the average height growth by smoking status and store it in the r object p2
p2 <- growth_summary_sm %>%
  ggplot(aes(x = age, y = mean_height, color = sm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.5, linewidth = 1) + # Smoothed trajectories
  labs(title = "Smoothed Average Height Growth by Smoking Status",
       x = "Age (weeks)", y = "Mean Height (cm)", color = "Smoking Status") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 50, 10))

# Put the two plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


# Fit a random slope model with both breastfeeding and smoking status as predictors
growth_model_both <- lmer(height ~ age_centered + age_centered_squared + bf + sm + (age_centered | id), data = growth)
summary(growth_model_both)

# Obtain the anova table for the model with both predictors
anova(growth_model_both)

# Fit the same model using ordinary linear regression
growth_lm <- lm(height ~ age_centered + age_centered_squared + bf + sm, data = growth)
summary(growth_lm)



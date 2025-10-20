# Load necessary libraries
library(ggplot2)
library(lmerTest)

# Hypothetical data with larger cage effect
set.seed(329)  # For reproducibility

# Increase between-cage variability
between_cage_sd <- 2  # Standard deviation for the cage effect

# Cage effect (one random effect per cage)
cage_effect <- rnorm(8, mean = 0, sd = between_cage_sd)

# Simulate data with larger cage effect
data <- data.frame(
  Cage = rep(1:8, times = 3),
  Treatment = rep(c("Placebo", "20 mg", "50 mg"), each = 8),
  Weight_Change = c(rnorm(8, mean = 14 + cage_effect, sd = 2),
                    rnorm(8, mean = 13 + cage_effect, sd = 2),
                    rnorm(8, mean = 11 + cage_effect, sd = 2))
)

data$Treatment <- factor(data$Treatment, levels=c("Placebo", "20 mg", "50 mg"))

# Create the scatter plot
ggplot(data, aes(x = Weight_Change, y = factor(Cage), color = Treatment)) +
  geom_point(size = 4) +
  labs(title = "Weight Change by Cage and Treatment",
       x = "Weight Change (grams)", 
       y = "Cage",
       color = "Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


# Analysis 1: one-way ANOVA, ignoring the cage effect

# Fit the one-way ANOVA model
model <- lm(Weight_Change ~ Treatment, data = data) 
summary(model)

# display the ANOVA table
anova(model)

# Analysis 2: mixed effects model with treatment as a fixed effect and cage as a random effect

# Fit the mixed effects model
mixed_model <- lmer(Weight_Change ~ Treatment + (1|Cage), data = data)
summary(mixed_model)
anova(mixed_model)

m3 <- lmer(Weight_Change ~ Treatment + (1 | Cage), data = data)
summary(m3)
anova(m3)

# Extract variance components
var_comp <- VarCorr(mixed_model)
var_comp

var_BB <- var_comp$Cage[1]  # Between-cage variance
var_WB <- summary(mixed_model)$sigma^2  # Within-cage variance
var_BB
var_WB

# Calculate total variance and standard deviation of the total variance
var_total <- var_BB + var_WB # Total variance
sd_total <- sqrt(var_total) # Standard deviation of the total variance
var_total
sd_total

# Calculate ICC
ICC <- var_BB / var_total
ICC

####################################################################

# Reshape the data into wide format for Friedman's test
# Wide format: Each row is a block (cage), and each column is a treatment's response
data_wide <- reshape(data, idvar = "Cage", timevar = "Treatment", direction = "wide")

# Perform Friedman's Test
friedman_test_result <- friedman.test(as.matrix(data_wide[, 2:4]))  # columns 2-4 are treatments

# View results
friedman_test_result
print(friedman_test_result)

summary(lm(Weight_Change~Treatment, data))
boxplot(Weight_Change~Treatment, data)

kruskal.test(Weight_Change~Treatment, data = data)



# Install pacman if not already available
if (!require("pacman")) install.packages("pacman")

# Use pacman to install (if needed) and load the required packages
pacman::p_load(dplyr, ggplot2, emmeans)

# Generating data for the single-factor experimental design
set.seed(123)
control <- rnorm(40, mean = -2, sd = 10)
drugA <- rnorm(40, mean = -20, sd = 10)
drugB <- rnorm(40, mean = -5, sd = 10)

# Combining data into a data frame
data_oneway <- data.frame(
  Treatment = factor(rep(c("Control", "DrugA", "DrugB"), each = 40),
                     levels = c("Control", "DrugA", "DrugB")),
  BP_change = c(control, drugA, drugB)
)

# Visualizing the results
ggplot(data_oneway, aes(x = Treatment, y = BP_change, fill = Treatment)) +
  geom_boxplot() +
  labs(title = NULL,
       x = "Treatment", y = "Change in SBP (mm Hg)") +
  theme_minimal() +
  theme(legend.position = "none")

  # Summarizing data
summary_stats <- data_oneway |>
  group_by(Treatment) |>
  summarise(
    Mean_BP_change = mean(BP_change),
    SD_BP_change = sd(BP_change)
  )

summary_stats

options(contrasts = c("contr.sum", "contr.poly"))  # Effects coding
contrasts(data_oneway$Treatment) # Inspect contrasts for Treatment

# Fitting the model with effects coding
model_oneway <- lm(BP_change ~ Treatment, data = data_oneway)
summary(model_oneway)

# ANOVA table
anova_results <- anova(model_oneway)
anova_results

emms <- emmeans(model_oneway, ~ Treatment)
emms

# Performing post-hoc analysis with emmeans
contrast(emms, method="pairwise", adjust="Bonferroni")

# Residuals vs Fitted
plot(model_oneway, which = 1, main = "Residuals vs Fitted")

# Normal Q-Q
plot(model_oneway, which = 2, main = "Normal Q-Q")

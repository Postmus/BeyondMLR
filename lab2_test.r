# Install pacman if not already available
if (!require("pacman")) install.packages("pacman")

# Use pacman to install (if needed) and load the required packages
pacman::p_load(dplyr, ggplot2, emmeans, lmerTest)

# Set seed for reproducibility
set.seed(456)

# Define blocks and treatments
blocks <- factor(paste0("Laboratory ", rep(1:5, each = 3)))  # Labs 1 to 5
treatments <- factor(rep(c("A", "B", "C"), times = 5))

# Simulate data
block_effect <- rnorm(5, mean = 0, sd = 2)  # Random effect for each block
treatment_effect <- c(A = 5, B = 7, C = 6)  # Fixed effects for treatments

# Create data frame
data_block <- data.frame(
  Laboratory = blocks,
  Treatment = treatments,
  WoundHealing = NA
)

# Assign responses
for (i in 1:nrow(data_block)) {
  b <- as.numeric(data_block$Laboratory[i])
  t <- data_block$Treatment[i]
  data_block$WoundHealing[i] <-
    50 + block_effect[b] + treatment_effect[t] + rnorm(1, mean = 0, sd = 1)
}

ggplot(data_block, aes(x = WoundHealing, y = Laboratory, color = Treatment)) +
  geom_point(size = 4) +
  labs(title = "Wound Healing by Treatment and Laboratory",
       x = "Wound Healing Measure",
       y = "Laboratory") +
  theme_minimal()

  # Summarizing data by Treatment
summary_stats_treatment <- data_block |>
  group_by(Treatment) |>
  summarise(
    Mean_WoundHealing = mean(WoundHealing),
    SD_WoundHealing = sd(WoundHealing)
  )

# Summarizing data by Laboratory
summary_stats_laboratory <- data_block |>
  group_by(Laboratory) |>
  summarise(
    Mean_WoundHealing = mean(WoundHealing),
    SD_WoundHealing = sd(WoundHealing)
  )

# Displaying the produced summary tables
summary_stats_treatment
summary_stats_laboratory

options(contrasts = c("contr.sum", "contr.poly"))  # Effects coding
contrasts(data_block$Treatment) # Inspect contrasts for Treatment

model_block <- lmer(WoundHealing ~ Treatment + (1 | Laboratory), data = data_block)
summary(model_block)

anova(model_block)
car::Anova(model_block, type = "III", test.statistic = "F")
car::Anova(model_block, type = "III", test.statistic = "Wald")

ls_means(model_block)
emmeans(model_block)

# Compute pairwise differences of the estimated marginal means
ls_means(model_block)

emms <- ls_means(model_block, pairwise=TRUE)
emms
p.adjust(emms$`Pr(>|t|)`, method="bonferroni")

emms2 <- emmeans(model_block, ~ Treatment)
emms2
contrast(emms2, method="pairwise", adjust="Bonferroni")

# Perform pairwise comparisons with Bonferroni adjustment
pairs(emms2, adjust = "bonferroni")

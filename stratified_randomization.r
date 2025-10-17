set.seed(431)

# design
trt_levels  <- c("Control","DrugA","DrugB")
grades      <- c("Grade1","Grade2","Grade3")              # SBP-only grading
n_per_trt   <- 45                                          # divisible by 3 grades 
n_per_cell  <- n_per_trt / length(grades)                  
sd_eps      <- 8

# treatment means (overall), mmHg change over 12 weeks (plausible magnitudes)
trt_mean <- c(Control = -4, DrugA = -11, DrugB = -8)

# strong baseline grade effect on change (higher baseline → larger drop)
grade_effect <- c(Grade1 = 0, Grade2 = -6, Grade3 = -12)

# helper to simulate baseline SBP within each grade
r_sbp <- function(grade, n) {
  if (grade == "Grade1") rnorm(n, mean = 148, sd = 6)     # 140–159
  else if (grade == "Grade2") rnorm(n, mean = 168, sd = 6) # 160–179
  else rnorm(n, mean = 185, sd = 7)                        # ≥180
}

# build stratified dataset: equal n per grade × treatment
df_bp_strat <- do.call(
  rbind,
  lapply(grades, function(g) {
    do.call(
      rbind,
      lapply(trt_levels, function(t) {
        mu <- trt_mean[[t]] + grade_effect[[g]]
        n  <- n_per_cell
        data.frame(
          HypertensionGrade = g,
          Treatment = t,
          Baseline_SBP = r_sbp(g, n),
          SBP_change  = rnorm(n, mean = mu, sd = sd_eps)
        )
      })
    )
  })
)

df_bp_strat$HypertensionGrade <- factor(df_bp_strat$HypertensionGrade, levels = grades)
df_bp_strat$Treatment <- factor(df_bp_strat$Treatment, levels = trt_levels)

# models: one-way vs adjusted (two-way with grade)
fit_oneway <- lm(SBP_change ~ Treatment, data = df_bp_strat)
fit_adj <- lm(SBP_change ~ Treatment + HypertensionGrade, data = df_bp_strat)

anova(fit_oneway)
anova(fit_adj)

# quick check: perfect balance from stratified randomization
with(df_bp_strat, table(Treatment, HypertensionGrade))

# EDA: interaction plot (means and 95% CI by Treatment × HypertensionGrade)
library(dplyr)
library(ggplot2)

eda_sum <- df_bp_strat %>%
  group_by(Treatment, HypertensionGrade) %>%
  summarise(
    n    = n(),
    mean = mean(SBP_change),
    sd   = sd(SBP_change),
    se   = sd / sqrt(n),
    tcrt = qt(0.975, df = n - 1),
    ci   = tcrt * se,
    .groups = "drop"
  )

ggplot(eda_sum, aes(x = Treatment, y = mean,
                    group = HypertensionGrade,
                    color = HypertensionGrade, shape = HypertensionGrade)) +
  geom_line() +
  geom_point(size = 3) +
  #geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), width = 0.12) +
  labs(
    x = "Treatment",
    y = "Mean change in SBP (mmHg)",
    title = "Interaction plot: Treatment by hypertension grade"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")

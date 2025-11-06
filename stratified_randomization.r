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

####################################

set.seed(42)

# ----------- parameters -----------
N_total      <- 800          # total sample size
p_female     <- 0.65         # unbalanced gender mix (set 0.5 for balanced F/M)
alloc_ratio  <- 0.5          # within-stratum treatment proportion

# outcome model: Y = mu + tau*T + gamma*G + delta*(T*G) + e
mu    <- 0
tau   <- 1.0                 # marginal treatment effect (for G = 0 baseline)
gamma <- 2.0                 # main effect for G = 1 vs 0
delta <- -0.8                # treatment×gender interaction (effect modification)
sigma <- 1.0                 # residual SD

# ----------- generate gender -----------
n_F <- rbinom(1, N_total, p_female)
n_M <- N_total - n_F
gender <- factor(c(rep("F", n_F), rep("M", n_M)))

# ----------- stratified randomization within gender -----------
assign_within <- function(n, p = alloc_ratio) {
  z <- rep(0L, n)
  n1 <- round(n * p)
  z[sample.int(n, n1)] <- 1L
  z
}
T_F <- assign_within(n_F, alloc_ratio)
T_M <- assign_within(n_M, alloc_ratio)
treat <- c(T_F, T_M)                       # 0/1 assignment within each stratum

# ----------- simulate outcomes with interaction -----------
G_num <- ifelse(gender == "M", 1L, 0L)
Y <- mu + tau * treat + gamma * G_num + delta * (treat * G_num) + rnorm(N_total, 0, sigma)

dat <- data.frame(Y = Y, T = factor(treat), G = gender)

# ----------- fit models -----------
options(contrasts = c("contr.sum", "contr.poly"))  # default treatment coding
fit_add   <- lm(Y ~ G + T, data = dat)     # additive two-way ANOVA (no interaction)
fit_int   <- lm(Y ~ G * T, data = dat)     # with interaction

summary(fit_add)
summary(fit_int)

library(emmeans)
emmeans(fit_add, ~ T)
emmeans(fit_int, ~ T)

emmeans(fit_add, ~ G)
emmeans(fit_int, ~ G)

library(ggplot2)
ggplot(dat, aes(x = T, y = Y, color = G, group = G)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = NULL,
       x = "Therapy Type", 
       y = "Mean recovery rate (%)",
       color = "gnnder") +
  theme_minimal()



# ----------- stratum-specific effects and weighted average -----------
mean_diff <- function(d, g) {
  with(subset(d, G == g), mean(Y[T == 1]) - mean(Y[T == 0]))
}
tau_F_hat <- mean_diff(dat, "F")
tau_M_hat <- mean_diff(dat, "M")

w_F <- mean(dat$G == "F")
w_M <- 1 - w_F
marginal_sample_weighted <- w_F * tau_F_hat + w_M * tau_M_hat

# additive model treatment coefficient (reference level T=0)
add_coef <- unname(coef(fit_add)["T1"])

# compute the marginal ATE from the interaction model by averaging over sample G mix
# predicted mean under T=1 minus T=0, averaging over observed G distribution
pred_T1 <- with(dat, tapply(predict(fit_int, newdata = data.frame(T = factor(1, levels = c(0,1)), G = G)), G, mean))
pred_T0 <- with(dat, tapply(predict(fit_int, newdata = data.frame(T = factor(0, levels = c(0,1)), G = G)), G, mean))
marginal_from_fit_int <- w_F * (pred_T1["F"] - pred_T0["F"]) + w_M * (pred_T1["M"] - pred_T0["M"])

# ----------- print comparisons -----------
cat("n_F =", n_F, " n_M =", n_M, "  (w_F =", round(w_F, 3), ", w_M =", round(w_M, 3), ")\n\n")

cat("stratum-specific treatment effects:\n")
cat("  tau_F_hat =", round(tau_F_hat, 3), "\n")
cat("  tau_M_hat =", round(tau_M_hat, 3), "\n\n")

cat("sample-weighted marginal ATE (by hand):", round(marginal_sample_weighted, 3), "\n")
cat("additive model: coef[T=1]            :", round(add_coef, 3), "\n")
cat("interaction model: marginal ATE       :", round(marginal_from_fit_int, 3), "\n\n")

cat("SEs (additive vs interaction) for treatment main effect:\n")
se_add <- sqrt(vcov(fit_add)["T1","T1"])
# for the interaction model, report the SE of the sample-marginal ATE via delta method (simple plug-in here)
# alternatively, show SEs of the simple effects:
se_tauF <- sqrt(vcov(fit_int)["T1","T1"])
se_tauM <- sqrt(vcov(fit_int)["T1","T1"] + vcov(fit_int)["T1:G M","T1:G M"] + 2*vcov(fit_int)["T1","T1:G M"])
cat("  additive SE[T=1]      =", round(se_add, 3), "\n")
cat("  interaction SE simple effects: SE_F =", round(se_tauF,3), " SE_M =", round(se_tauM,3), "\n")




# ----------- optional: wrap into a function to run many sims -----------
simulate_once <- function(N = 800, pF = 0.65, tau = 1, gamma = 2, delta = -0.8, sigma = 1, alloc = 0.5) {
  nF <- rbinom(1, N, pF); nM <- N - nF
  G <- factor(c(rep("F", nF), rep("M", nM)))
  assign_within <- function(n, p) { z <- rep(0L, n); n1 <- round(n * p); z[sample.int(n, n1)] <- 1L; z }
  T <- c(assign_within(nF, alloc), assign_within(nM, alloc))
  Gnum <- ifelse(G == "M", 1L, 0L)
  Y <- mu + tau*T + gamma*Gnum + delta*(T*Gnum) + rnorm(N, 0, sigma)
  d <- data.frame(Y = Y, T = factor(T), G = G)
  fitA <- lm(Y ~ T + G, d)
  fitI <- lm(Y ~ T*G, d)
  tauF <- with(subset(d, G=="F"), mean(Y[T==1]) - mean(Y[T==0]))
  tauM <- with(subset(d, G=="M"), mean(Y[T==1]) - mean(Y[T==0]))
  wF <- mean(d$G=="F"); wM <- 1-wF
  c(
    add_coef = unname(coef(fitA)["T1"]),
    weighted = wF*tauF + wM*tauM,
    tauF = tauF, tauM = tauM
  )
}

# example: check equality over multiple replicates
res <- replicate(200, simulate_once(), simplify = "array")
cat("\nmean(additive coef)  =", round(mean(res["add_coef",]), 3), "\n")
cat("mean(weighted ATE)   =", round(mean(res["weighted",]), 3), "\n")

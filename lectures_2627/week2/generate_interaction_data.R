# Lecture teaching data: treatment effects that vary across hospitals in a HIPARCO-inspired CPAP trial.
# Run from any directory: Rscript /path/to/week2/generate_interaction_data.R
library(ggplot2)
library(jsonlite)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- dirname(normalizePath(sub("^--file=", "", script_arg)))
source(file.path(script_dir, "../../slides-common/R/common_fig.R"))
output_dir <- file.path(script_dir, "assets/fixed-interaction")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

seed <- 20261113L
set.seed(seed)
centers <- LETTERS[1:6]
patients_per_arm <- c(12L, 16L, 18L, 22L, 24L, 28L)
baseline <- 2
population_treatment_effect <- 3.1
individual_sd <- 8
intercept_sd <- 4
slope_sd <- 4
intercept_slope_correlation <- 0.5
covariance <- matrix(c(
  intercept_sd^2, intercept_slope_correlation * intercept_sd * slope_sd,
  intercept_slope_correlation * intercept_sd * slope_sd, slope_sd^2
), nrow = 2)

# One bivariate Normal draw per center, without recentering the realized effects.
# This supports the later random-slope section; the analysis below treats the
# six realized intercepts and treatment effects as fixed parameters.
z <- matrix(rnorm(2 * length(centers)), ncol = 2)
center_draws <- z %*% chol(covariance)
truth <- data.frame(
  center = centers,
  intercept_deviation = center_draws[, 1],
  treatment_deviation = center_draws[, 2],
  control_mean = baseline + center_draws[, 1],
  treatment_effect = population_treatment_effect + center_draws[, 2]
)
truth$cpap_mean <- truth$control_mean + truth$treatment_effect
# Translate the realized effects into the sum-to-zero fixed-model parameters.
truth$fixed_center_effect <- truth$control_mean - mean(truth$control_mean)
truth$fixed_interaction_effect <- truth$treatment_effect - mean(truth$treatment_effect)

cpap <- do.call(rbind, lapply(seq_along(centers), function(j) {
  treatment <- sample(rep(c("Control", "CPAP"), each = patients_per_arm[j]))
  data.frame(
    patient = paste0(centers[j], "-", seq_len(2 * patients_per_arm[j])),
    center = centers[j],
    treatment = treatment,
    bp_reduction = round(truth$control_mean[j] +
      truth$treatment_effect[j] * (treatment == "CPAP") +
      rnorm(2 * patients_per_arm[j], sd = individual_sd), 1)
  )
}))
cpap$treatment <- factor(cpap$treatment, levels = c("Control", "CPAP"))
cpap$center <- factor(cpap$center, levels = centers)
contrasts(cpap$treatment) <- contr.treatment(levels(cpap$treatment))
contrasts(cpap$center) <- contr.sum(levels(cpap$center))
write.csv(cpap, file.path(output_dir, "hiparco_simulated.csv"), row.names = FALSE)
write.csv(truth, file.path(output_dir, "generating_center_effects.csv"), row.names = FALSE)

model_additive <- lm(bp_reduction ~ treatment + center, data = cpap)
model_interaction <- lm(bp_reduction ~ treatment * center, data = cpap)
grid <- expand.grid(treatment = levels(cpap$treatment), center = centers)
grid$treatment <- factor(grid$treatment, levels = levels(cpap$treatment))
grid$center <- factor(grid$center, levels = centers)
cell_means <- cbind(grid, as.data.frame(predict(
  model_interaction, newdata = grid, interval = "confidence"
)))
names(cell_means)[3:5] <- c("estimate", "lower_95", "upper_95")

# Calculate CPAP-minus-control contrasts from the model matrix, retaining
# covariance between fitted means. These are the contrasts for a later emmeans lab.
design <- model.matrix(delete.response(terms(model_interaction)), grid,
                       contrasts.arg = model_interaction$contrasts)
contrast_matrix <- design[grid$treatment == "CPAP", , drop = FALSE] -
  design[grid$treatment == "Control", , drop = FALSE]
estimates <- drop(contrast_matrix %*% coef(model_interaction))
standard_errors <- sqrt(diag(contrast_matrix %*% vcov(model_interaction) %*% t(contrast_matrix)))
critical <- qt(0.975, df.residual(model_interaction))
comparisons <- data.frame(
  center = centers, estimate = estimates, standard_error = standard_errors,
  df = df.residual(model_interaction),
  lower_95 = estimates - critical * standard_errors,
  upper_95 = estimates + critical * standard_errors,
  row.names = NULL
)
comparisons$p <- 2 * pt(-abs(comparisons$estimate / comparisons$standard_error),
                       df = comparisons$df)
model_result <- function(model) {
  ci <- confint(model, "treatmentCPAP")
  list(estimate = unname(coef(model)["treatmentCPAP"]),
       standard_error = unname(coef(summary(model))["treatmentCPAP", "Std. Error"]),
       lower_95 = unname(ci[1]), upper_95 = unname(ci[2]),
       p = unname(coef(summary(model))["treatmentCPAP", "Pr(>|t|)"]),
       residual_sd = sigma(model), residual_df = df.residual(model),
       residual_sum_squares = deviance(model))
}
interaction_test <- anova(model_additive, model_interaction)

# All fitted coefficients in the same estimate/SE/CI/p format as Week 1.
# The sixth center's deviations are implied by effects coding, not extra coefficients.
coefficient_summary <- coef(summary(model_interaction))
coefficient_intervals <- confint(model_interaction)
coefficient_results <- data.frame(
  term = rownames(coefficient_summary),
  label = c("Intercept (\u03b2\u2080)", "CPAP (\u03b2\u2081)",
            paste("Hospital", LETTERS[1:5]),
            paste("CPAP \u00d7 Hospital", LETTERS[1:5])),
  estimate = coefficient_summary[, "Estimate"],
  standard_error = coefficient_summary[, "Std. Error"],
  lower_95 = coefficient_intervals[, 1],
  upper_95 = coefficient_intervals[, 2],
  t = coefficient_summary[, "t value"],
  p = coefficient_summary[, "Pr(>|t|)"],
  df = df.residual(model_interaction),
  row.names = NULL
)
stopifnot(identical(coefficient_results$term,
  c("(Intercept)", "treatmentCPAP", paste0("center", 1:5),
    paste0("treatmentCPAP:center", 1:5))))
write.csv(coefficient_results, file.path(output_dir, "regression_coefficients.csv"), row.names = FALSE)

# Check the design and the intended meaning of effects coding using independent
# observed-cell summaries and a reference-coded fit.
observed_means <- tapply(cpap$bp_reduction, list(cpap$center, cpap$treatment), mean)
reference_model <- lm(bp_reduction ~ treatment * center, data = cpap,
                      contrasts = list(treatment = "contr.treatment", center = "contr.treatment"))
average_contrast <- colMeans(contrast_matrix)
stopifnot(
  nrow(cpap) == 2L * sum(patients_per_arm),
  all(table(cpap$center, cpap$treatment) == patients_per_arm),
  all(is.finite(cpap$bp_reduction)),
  max(abs(estimates - (observed_means[, "CPAP"] - observed_means[, "Control"]))) < 1e-9,
  abs(coef(model_interaction)["(Intercept)"] - mean(observed_means[, "Control"])) < 1e-9,
  abs(coef(model_interaction)["treatmentCPAP"] - mean(estimates)) < 1e-9,
  abs(coef(model_additive)["treatmentCPAP"] - weighted.mean(estimates, patients_per_arm)) < 1e-9,
  max(abs(fitted(model_interaction) - fitted(reference_model))) < 1e-9,
  abs(drop(average_contrast %*% vcov(model_interaction) %*% average_contrast) -
        vcov(model_interaction)["treatmentCPAP", "treatmentCPAP"]) < 1e-9,
  abs(sum(truth$fixed_center_effect)) < 1e-9,
  abs(sum(truth$fixed_interaction_effect)) < 1e-9
)

write.csv(cell_means, file.path(output_dir, "fitted_cell_means.csv"), row.names = FALSE)
write.csv(comparisons, file.path(output_dir, "within_center_comparisons.csv"), row.names = FALSE)
write_json(list(
  provenance = list(type = "Simulated teaching data", inspiration = "HIPARCO trial",
    citation = "Martinez-Garcia et al. JAMA 2013;310:2407-2415",
    doi = "10.1001/jama.2013.281250",
    note = "The six-hospital design, patient records and center heterogeneity are simulated."),
  outcome = "Reduction in 24-hour mean blood pressure from baseline to 12 weeks (mmHg); positive values indicate improvement",
  treatment_coding = "0 Control: usual antihypertensive medication; 1 CPAP: the same medication plus CPAP during sleep",
  seed = seed, observations = nrow(cpap), centers = length(centers),
  patients_per_treatment_per_center = patients_per_arm,
  allocation = data.frame(center = centers, control = patients_per_arm, CPAP = patients_per_arm),
  generating_parameters = list(baseline = baseline,
    population_treatment_effect = population_treatment_effect,
    intercept_sd = intercept_sd, slope_sd = slope_sd,
    intercept_slope_correlation = intercept_slope_correlation,
    individual_sd = individual_sd),
  realized_fixed_parameters = list(intercept = mean(truth$control_mean),
    average_treatment_effect = mean(truth$treatment_effect)),
  additive_model = model_result(model_additive),
  interaction_model = model_result(model_interaction),
  regression_coefficients = coefficient_results,
  interaction_test = list(numerator_df = interaction_test$Df[2],
    denominator_df = df.residual(model_interaction),
    F = interaction_test$F[2], p = interaction_test$`Pr(>F)`[2]),
  interval_method = "Pointwise 95% t intervals; no multiplicity adjustment.",
  simple_effect_test_method = "Two-sided t tests of zero CPAP-minus-control difference; pointwise 95% confidence intervals and unadjusted p-values.",
  fitted_cell_means = cell_means,
  within_center_comparisons = comparisons,
  session = capture.output(sessionInfo())
), file.path(output_dir, "model_results.json"), pretty = TRUE, auto_unbox = TRUE, digits = 12)

figure_theme <- theme_minimal(base_size = 16) + theme(
  panel.grid.minor = element_blank(),
  legend.position = "top",
  plot.margin = margin(12, 16, 12, 12)
)
# The full cell-means model reproduces the observed treatment-by-center means.
# Show the traditional interaction plot, without individual patient observations.
interaction_plot <- ggplot(cell_means,
                           aes(center, estimate, color = treatment, group = treatment)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3.5) +
  scale_x_discrete(labels = paste("Hospital", centers)) +
  scale_color_manual(values = c(Control = color_primary, CPAP = color_accent)) +
  labs(x = NULL, y = "Mean reduction (mmHg)", color = NULL) +
  figure_theme
ggsave(file.path(output_dir, "interaction-outcomes.png"), interaction_plot,
       width = 12, height = 5, dpi = 200, bg = "white")
lecture_outcomes <- interaction_plot +
  theme(axis.text = element_text(size = 18), axis.text.x = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.position = "top",
        legend.text = element_text(size = 18))
ggsave(file.path(output_dir, "interaction-outcomes-slide.png"), lecture_outcomes,
       width = 12, height = 5.3, dpi = 200, bg = "white")

contrast_plot <- ggplot(comparisons, aes(center, estimate)) +
  geom_hline(yintercept = 0, color = color_muted, linewidth = 0.5) +
  geom_hline(yintercept = mean(estimates), color = color_accent, linetype = "dashed") +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.12,
                color = color_primary, linewidth = 0.8) +
  geom_point(color = color_primary, size = 3) +
  labs(x = NULL, y = "CPAP minus control (mmHg)",
       caption = "Points: estimated center-specific effects; bars: pointwise 95% confidence intervals.\nDashed line: equally weighted average estimated treatment effect.") +
  figure_theme + theme(plot.caption = element_text(size = 12, hjust = 0))
ggsave(file.path(output_dir, "interaction-contrasts.png"), contrast_plot,
       width = 10, height = 5.5, dpi = 200, bg = "white")
lecture_contrasts <- contrast_plot + labs(caption = NULL, y = "Treatment difference (mmHg)") +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 22))
ggsave(file.path(output_dir, "interaction-contrasts-slide.png"), lecture_contrasts,
       width = 12, height = 5.3, dpi = 200, bg = "white")

# Diagnostics for the fitted full interaction model, using observed residuals.
diagnostics <- data.frame(center = cpap$center, treatment = cpap$treatment,
  fitted = fitted(model_interaction), residual = resid(model_interaction),
  standardized = rstandard(model_interaction))
write.csv(diagnostics, file.path(output_dir, "interaction_diagnostics.csv"), row.names = FALSE)
diagnostic_theme <- theme_minimal(base_size = 18) + theme(
  panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
  plot.title = element_text(size = 20, face = "bold", color = color_primary),
  axis.title = element_text(size = 18, color = color_primary),
  axis.text = element_text(size = 16, color = color_primary),
  plot.margin = margin(8, 12, 6, 8))
residual_plot <- ggplot(diagnostics, aes(fitted, residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = color_muted) +
  geom_point(color = color_primary, alpha = .65, size = 1.8) +
  labs(title = "Residuals vs fitted values", x = "Fitted reduction (mmHg)", y = "Residual (mmHg)") +
  diagnostic_theme
qq_plot <- ggplot(diagnostics, aes(sample = standardized)) +
  stat_qq_line(color = color_accent, linewidth = .8) +
  stat_qq(color = color_primary, alpha = .65, size = 1.8) +
  labs(title = "Normal Q-Q plot", x = "Theoretical quantiles", y = "Std. residual") +
  diagnostic_theme
ggsave(file.path(output_dir, "interaction-residuals-vs-fitted.png"), residual_plot,
       width = 5.80, height = 2.90, dpi = 250, bg = "white")
ggsave(file.path(output_dir, "interaction-normal-qq.png"), qq_plot,
       width = 5.80, height = 2.90, dpi = 250, bg = "white")

print(comparisons, digits = 4, row.names = FALSE)
cat("\nAverage treatment effect and uncertainty:\n")
print(rbind(additive = unlist(model_result(model_additive)),
            interaction = unlist(model_result(model_interaction))), digits = 4)
cat("\nInteraction test:\n")
print(interaction_test)

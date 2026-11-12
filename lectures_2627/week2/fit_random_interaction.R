# Refit the simulated HIPARCO-inspired observations; never regenerate the dataset.
library(lmerTest)
library(ggplot2)
library(jsonlite)
args <- commandArgs(FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)])))
source(file.path(here, "../../slides-common/R/common_fig.R"))
out <- file.path(here, "assets/random-interaction")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
data_path <- file.path(here, "assets/fixed-interaction/hiparco_simulated.csv")
d <- read.csv(data_path)
d$center <- factor(d$center)
d$T <- as.integer(d$treatment == "CPAP")
allocation <- table(d$center, d$T)
patients_per_arm <- as.numeric(allocation[, "0"])
stopifnot(nrow(d) == 240, nlevels(d$center) == 6,
          all(allocation[, "0"] == allocation[, "1"]),
          length(unique(patients_per_arm)) > 1)
model <- lmer(bp_reduction ~ T + (1 + T | center), data = d, REML = TRUE,
              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))
stopifnot(!isSingular(model), model@optinfo$conv$opt == 0,
          is.null(model@optinfo$conv$lme4$messages))
co <- coef(summary(model, ddf = "Satterthwaite"))
fixed <- data.frame(term = rownames(co), estimate = co[, "Estimate"],
                    standard_error = co[, "Std. Error"], df = co[, "df"],
                    t = co[, "t value"], p = co[, "Pr(>|t|)"], row.names = NULL)
fixed$lower_95 <- fixed$estimate - qt(.975, fixed$df) * fixed$standard_error
fixed$upper_95 <- fixed$estimate + qt(.975, fixed$df) * fixed$standard_error

# Pre-lab lecture comparison: the same observations and fixed-effects formula,
# but a common treatment effect with random intercepts only.
intercept_model <- lmer(bp_reduction ~ T + (1 | center), data = d, REML = TRUE,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000)))
stopifnot(!isSingular(intercept_model), intercept_model@optinfo$conv$opt == 0,
          is.null(intercept_model@optinfo$conv$lme4$messages))
ri_co <- coef(summary(intercept_model, ddf = "Satterthwaite"))
ri_fixed <- data.frame(term = rownames(ri_co), estimate = ri_co[, "Estimate"],
                       standard_error = ri_co[, "Std. Error"], df = ri_co[, "df"],
                       t = ri_co[, "t value"], p = ri_co[, "Pr(>|t|)"], row.names = NULL)
ri_fixed$lower_95 <- ri_fixed$estimate - qt(.975, ri_fixed$df) * ri_fixed$standard_error
ri_fixed$upper_95 <- ri_fixed$estimate + qt(.975, ri_fixed$df) * ri_fixed$standard_error
# Within each hospital allocation is 50:50. The random-intercept-only
# treatment coefficient is the patient-count-weighted within-hospital contrast.
cell_means <- tapply(d$bp_reduction, list(d$center, d$T), mean)
stopifnot(abs(fixef(intercept_model)["T"] -
                weighted.mean(cell_means[, "1"] - cell_means[, "0"], patients_per_arm)) < 1e-7,
          abs(ri_fixed$standard_error[ri_fixed$term == "T"] -
                sigma(intercept_model) * sqrt(2 / sum(patients_per_arm))) < 1e-7)
write.csv(ri_fixed, file.path(out, "random_intercept_coefficients.csv"), row.names = FALSE)
writeLines(trimws(capture.output(summary(intercept_model)), which = "right"), file.path(out, "random_intercept_summary.txt"))
G <- as.matrix(VarCorr(model)$center)
variances <- list(intercept_variance = G[1, 1], slope_variance = G[2, 2],
                  intercept_slope_covariance = G[1, 2], residual_variance = sigma(model)^2,
                  intercept_sd = sqrt(G[1, 1]), slope_sd = sqrt(G[2, 2]),
                  residual_sd = sigma(model), correlation = cov2cor(G)[1, 2])
means <- tapply(d$bp_reduction, list(d$center, d$T), mean)
raw_effects <- means[, "1"] - means[, "0"]
center_coef <- coef(model)$center
center_results <- data.frame(center = rownames(center_coef),
                              fixed_baseline = means[, "0"], fixed_effect = raw_effects,
                              random_baseline = center_coef[, "(Intercept)"],
                              random_effect = center_coef[, "T"], row.names = NULL)
grid <- expand.grid(T = 0:1, center = levels(d$center))
grid$conditional_mean <- predict(model, newdata = grid, re.form = NULL)

# Independent GLS and conditional-mean checks allow hospital sizes to differ.
# Each hospital contributes its control mean and CPAP-minus-control difference.
cell_summary <- cbind(control = means[, "0"], effect = raw_effects)
R <- lapply(patients_per_arm, function(n) sigma(model)^2 / n * matrix(c(1, -1, -1, 2), 2))
precision <- lapply(R, function(r) solve(G + r))
fixed_direct <- solve(Reduce(`+`, precision),
  Reduce(`+`, lapply(seq_along(R), function(j) precision[[j]] %*% cell_summary[j, ])))
pooled_direct <- t(vapply(seq_along(R), function(j)
  drop(fixef(model) + G %*% precision[[j]] %*% (cell_summary[j, ] - fixef(model))), numeric(2)))
stopifnot(max(abs(fixef(model) - fixed_direct)) < 1e-7,
          max(abs(as.matrix(center_coef) - pooled_direct)) < 1e-7,
          max(abs(center_results$random_effect -
                    (grid$conditional_mean[grid$T == 1] - grid$conditional_mean[grid$T == 0]))) < 1e-7)

diagnostics <- data.frame(center = d$center, treatment = d$treatment,
                          fitted = fitted(model), residual = residuals(model),
                          scaled_residual = residuals(model) / sigma(model))
write.csv(fixed, file.path(out, "fixed_coefficients.csv"), row.names = FALSE)
write.csv(as.data.frame(VarCorr(model)), file.path(out, "variance_components.csv"), row.names = FALSE)
write.csv(center_results, file.path(out, "center_effects.csv"), row.names = FALSE)
write.csv(diagnostics, file.path(out, "diagnostics.csv"), row.names = FALSE)
write_json(list(formula = "bp_reduction ~ T + (1 + T | center)", treatment_coding = "0 Control; 1 CPAP",
                data_md5 = unname(tools::md5sum(data_path)), observations = nrow(d), centers = nlevels(d$center),
                patients_per_treatment_per_center = patients_per_arm,
                estimation = "REML; correlated random intercept and slope; bobyqa optimizer",
                inference = "Two-sided t tests and pointwise 95% t intervals using coefficient-specific Satterthwaite degrees of freedom; no multiplicity adjustment",
                fixed_coefficients = fixed, variance_components = variances,
                random_intercept_model = list(
                  formula = "bp_reduction ~ T + (1 | center)", estimation = "REML",
                  fixed_coefficients = ri_fixed,
                  intercept_variance = as.numeric(VarCorr(intercept_model)$center),
                  residual_variance = sigma(intercept_model)^2,
                  singular = isSingular(intercept_model), convergence = intercept_model@optinfo$conv),
                center_effects = center_results, singular = isSingular(model),
                convergence = model@optinfo$conv, session = capture.output(sessionInfo())),
           file.path(out, "model_results.json"), pretty = TRUE, auto_unbox = TRUE, digits = 12)
writeLines(trimws(capture.output(summary(model)), which = "right"), file.path(out, "model_summary.txt"))

plot_theme <- theme_minimal(base_size = 18, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 18, color = color_text),
        axis.title = element_text(size = 20, color = color_text),
        legend.position = "top", legend.text = element_text(size = 18),
        plot.margin = margin(10, 18, 10, 10))
center_results$x <- seq_len(nrow(center_results))
pool_plot <- ggplot(center_results) +
  geom_hline(yintercept = fixef(model)["T"], color = color_muted, linetype = "dashed", linewidth = .7) +
  geom_segment(aes(x = x - .10, xend = x + .10, y = fixed_effect, yend = random_effect),
               color = color_muted, linewidth = .8) +
  geom_point(aes(x = x - .10, y = fixed_effect, color = "Fixed interaction", shape = "Fixed interaction"), size = 3.8) +
  geom_point(aes(x = x + .10, y = random_effect, color = "Random intercept and slope", shape = "Random intercept and slope"), size = 3.8) +
  scale_color_manual(values = c("Fixed interaction" = color_primary, "Random intercept and slope" = color_accent)) +
  scale_shape_manual(values = c("Fixed interaction" = 16, "Random intercept and slope" = 17)) +
  scale_x_continuous(breaks = center_results$x, labels = paste("Hospital", center_results$center), limits = c(.5, 6.5)) +

  labs(x = NULL, y = "Treatment effect (mmHg)", color = NULL, shape = NULL) + plot_theme
ggsave(file.path(out, "partial-pooling.png"), pool_plot, width = 12.21, height = 4.58, dpi = 240, bg = "white")
ggsave(file.path(out, "partial-pooling.pdf"), pool_plot, width = 12.21, height = 4.58, device = cairo_pdf)

diagnostic_theme <- plot_theme + theme(
  plot.title = element_text(size = 20, face = "bold", color = color_primary),
  axis.title = element_text(size = 18), axis.text = element_text(size = 16),
  plot.margin = margin(8, 12, 6, 8))
residual_plot <- ggplot(diagnostics, aes(fitted, residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = color_muted) +
  geom_point(color = color_primary, alpha = .65, size = 1.8) +
  labs(title = "Residuals vs fitted values", x = "Fitted reduction (mmHg)", y = "Residual (mmHg)") + diagnostic_theme
qq_plot <- ggplot(diagnostics, aes(sample = scaled_residual)) +
  stat_qq_line(color = color_accent, linewidth = .8) +
  stat_qq(color = color_primary, alpha = .65, size = 1.8) +
  labs(title = "Normal Q-Q plot", x = "Theoretical quantiles", y = "Scaled residual") + diagnostic_theme
ggsave(file.path(out, "residuals-vs-fitted.png"), residual_plot, width = 5.80, height = 2.90, dpi = 250, bg = "white")
ggsave(file.path(out, "normal-qq.png"), qq_plot, width = 5.80, height = 2.90, dpi = 250, bg = "white")
print(fixed)
print(variances)
print(center_results)
cat("\nRandom-intercept-only comparison:\n")
print(ri_fixed)

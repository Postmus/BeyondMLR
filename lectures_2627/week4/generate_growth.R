library(ggplot2)
library(lme4)
library(emmeans)

script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
week <- dirname(normalizePath(script))
root <- normalizePath(file.path(week, "../.."))
source(file.path(root, "slides-common/R/common_fig.R"))
assets <- file.path(week, "assets")
dir.create(assets, showWarnings = FALSE)

growth <- foreign::read.spss(file.path(root, "labs_2526/data/growth.sav"), to.data.frame = TRUE)
growth$id <- factor(growth$id)
growth$t <- growth$age - 26
growth$w <- growth$bw / 1000 - 3.5
control <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
ri <- lmerTest::lmer(height ~ t + (1 | id), data = growth, control = control)
quad_ri <- lmerTest::lmer(height ~ t + I(t^2) + (1 | id), data = growth, control = control)
quad_slope <- lmerTest::lmer(height ~ t + I(t^2) + (1 + t | id), data = growth, control = control)
# Scale the full model's random coefficients for numerical stability; report them in weeks.
growth$t10 <- growth$t / 10
quad_full <- lmer(height ~ t + I(t^2) + (1 + t10 + I(t10^2) | id), data = growth, control = control)
comparison <- AIC(quad_full, quad_slope, quad_ri)
rownames(comparison) <- NULL
comparison$model <- c("full", "slope", "intercept")
# Compare nested mean structures on the same visits using maximum likelihood.
age_ml <- update(quad_slope, REML = FALSE)
weight_ml <- update(age_ml, . ~ . + w)
interaction_ml <- update(weight_ml, . ~ . + t:w + I(t^2):w)
weight_final <- update(interaction_ml, REML = TRUE)
likelihood_test <- function(reduced, full) {
  test <- anova(reduced, full, refit = FALSE)
  list(reduced_loglik = test$logLik[1], full_loglik = test$logLik[2],
       chisq = test$Chisq[2], df = test$Df[2], p = test[["Pr(>Chisq)"]][2])
}

theme_set(theme_minimal(base_size = 19, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = color_grid),
        axis.title = element_text(colour = color_text), axis.text = element_text(colour = color_text),
        legend.position = "top", legend.title = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA)))
save_plot <- function(name, plot, width, height, bg = "white") {
  ggsave(file.path(assets, paste0(name, ".png")), plot, width = width, height = height,
         dpi = 160, bg = bg, device = grDevices::png)
}
# Show the same six children in the exploratory figures.
ids <- c("1", "8", "43", "50", "402", "509")
sampled <- subset(growth, id %in% ids)
age_axis <- scale_x_continuous(breaks = c(8, 16, 26, 36, 44, 52))
growth_axes <- list(age_axis, coord_cartesian(xlim = c(6, 52), ylim = c(54, 82)),
                    labs(x = "Age (weeks)", y = "Height (cm)"))
visits <- ggplot(sampled, aes(age, height, group = id)) +
  geom_line(colour = color_secondary, linewidth = 0.9) +
  geom_point(colour = color_secondary, size = 1.9) + growth_axes
hero <- ggplot(subset(sampled, id %in% c("1", "43", "509")), aes(age, height, group = id)) +
  geom_line(colour = color_accent_light, linewidth = 1.2) +
  geom_point(colour = color_accent_light, size = 2.6) + growth_axes +
  theme(axis.text = element_text(colour = color_white), axis.title = element_text(colour = color_white),
        panel.grid.major = element_line(colour = "#426067"),
        plot.background = element_rect(fill = color_primary, colour = NA))
save_plot("opening", hero, 5.6, 4.6, color_primary)
observed <- visits + geom_smooth(data = growth, aes(group = 1), method = "loess",
  formula = y ~ x, se = FALSE, colour = color_primary, linewidth = 1.7)
save_plot("observed", observed, 10.5, 4.15)
save_plot("observed_panel", observed, 6, 4)

grid <- data.frame(age = seq(6, 52, length.out = 150))
grid$t <- grid$age - 26
# Draw illustrative pairs around the same fitted mean to isolate the role of each random term.
structures <- c("Intercept only", "Add linear age", "Add quadratic age")
examples <- expand.grid(age = 10:42, structure = factor(structures, levels = structures))
z <- (examples$age - 26) / 16
examples$gap <- with(examples, ifelse(structure == structures[1], 4,
                               ifelse(structure == structures[2], 4 + 2*z, 3 + 2.5*z + 1.5*z^2)))
examples$mean <- predict(quad_ri, data.frame(t = examples$age - 26), re.form = NA)
examples$high <- examples$mean + examples$gap / 2
examples$low <- examples$mean - examples$gap / 2
gaps <- subset(examples, age %in% c(10, 26, 42))
examples_plot <- ggplot(examples, aes(age)) +
  geom_line(aes(y = high), colour = color_primary, linewidth = 1.25) +
  geom_line(aes(y = low), colour = color_secondary, linewidth = 1.25) +
  geom_segment(data = gaps, aes(xend = age, y = low, yend = high),
               colour = color_muted, linewidth = 1, linetype = "dashed") +
  facet_wrap(~ structure, nrow = 1) +
  scale_x_continuous(breaks = c(10, 26, 42)) +
  labs(x = "Age (weeks)", y = "Height (cm)") +
  theme(strip.text = element_text(face = "bold", colour = color_primary))
save_plot("random_structures", examples_plot, 10.5, 3.8)

fixed_results <- function(model) {
  coefs <- coef(summary(model))
  margin <- qt(.975, coefs[, "df"]) * coefs[, "Std. Error"]
  data.frame(row.names = NULL, term = rownames(coefs), estimate = coefs[, "Estimate"],
             se = coefs[, "Std. Error"], lower = coefs[, "Estimate"] - margin,
             upper = coefs[, "Estimate"] + margin, p = coefs[, "Pr(>|t|)"])
}
ri_variance <- as.data.frame(VarCorr(ri))[c("grp", "vcov", "sdcor")]
rs_sd <- attr(VarCorr(quad_slope)$id, "stddev")
full_cov <- VarCorr(quad_full)$id

for (kind in c("linear", "quadratic")) {
  model <- if (kind == "linear") ri else quad_ri
  growth$error <- residuals(model)
  residual_plot <- ggplot(growth, aes(age, error)) +
    geom_hline(yintercept = 0, colour = color_muted, linetype = 2) +
    geom_point(colour = color_secondary, alpha = .50, size = 1.8) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = color_primary, linewidth = 1.4) +
    age_axis + coord_cartesian(ylim = c(-9, 6)) +
    labs(x = "Age (weeks)", y = "Conditional residual (cm)")
  save_plot(paste0("residual_", kind), residual_plot, 6, 3.75)
}
grid$linear <- predict(ri, newdata = grid, re.form = NA)
two_boys <- subset(sampled, id %in% c("1", "509"))
# Draw smooth child-specific predictions only over each boy's observed age range.
boy_grid <- do.call(rbind, lapply(split(two_boys, two_boys$id, drop = TRUE), function(boy) {
  age <- seq(min(boy$age), max(boy$age), length.out = 120)
  data.frame(id = boy$id[1], age = age, t = age - 26)
}))
plot_two_boys <- function(model) {
  grid$mean_height <- predict(model, newdata = grid, re.form = NA)
  boy_grid$fitted_height <- predict(model, newdata = boy_grid)
  two_boys$fitted_height <- predict(model, newdata = two_boys)
  ggplot(two_boys, aes(age, height, colour = id)) +
    geom_line(data = grid, aes(age, mean_height), inherit.aes = FALSE,
              colour = color_primary, linetype = "longdash", linewidth = 1) +
    geom_line(data = boy_grid, aes(y = fitted_height), linewidth = 1.1) +
    geom_segment(aes(xend = age, yend = fitted_height), linewidth = .7) +
    geom_point(size = 2.8) +
    scale_colour_manual(values = c("1" = color_secondary, "509" = "#AD641F")) +
    growth_axes + theme(legend.position = "none")
}
save_plot("intercept", plot_two_boys(ri), 6, 4)
save_plot("selected_trajectories", plot_two_boys(quad_slope), 6, 4)
grid$quadratic <- predict(quad_ri, newdata = grid, re.form = NA)
curves <- ggplot(grid, aes(age)) +
  geom_line(aes(y = linear, colour = "Linear"), linewidth = 1.2, linetype = 2) +
  geom_line(aes(y = quadratic, colour = "Quadratic"), linewidth = 1.4) +
  scale_colour_manual(values = c(Linear = color_muted, Quadratic = color_primary)) +
  growth_axes + labs(y = "Mean height (cm)")
save_plot("curvature", curves, 6, 4)

# Use one birth weight per boy when defining tertiles and reference weights.
boys <- unique(growth[c("id", "bw")])
stopifnot(nrow(boys) == nlevels(growth$id))
cutoffs <- unname(quantile(boys$bw, c(1/3, 2/3)))
references <- unname(quantile(boys$bw, c(1/6, 5/6))) / 1000
growth$tertile <- cut(growth$bw, c(-Inf, cutoffs, Inf),
                      labels = c("Lower third", "Middle third", "Upper third"))
birth_colours <- c("#A65E1B", "#52765B", color_primary)
eda <- ggplot(growth, aes(age, height, colour = tertile)) +
  geom_point(alpha = .16, size = 1.45, show.legend = FALSE) +
  geom_smooth(aes(linetype = tertile), method = "loess", formula = y ~ x, span = .85,
              se = FALSE, linewidth = 1.25) +
  scale_colour_manual(values = birth_colours) +
  scale_linetype_manual(values = c("dashed", "dotdash", "solid")) +
  age_axis + labs(x = "Age (weeks)", y = "Height (cm)") +
  theme(panel.grid.major.x = element_blank(), legend.key.width = grid::unit(1.3, "cm"))
save_plot("birth_weight_tertiles", eda, 9.4, 3.6)

# Illustrate the null hypotheses and the alternative with the same underlying mean.
shapes <- c("Age only", "Add birth weight", "Add both age interactions")
sketch <- expand.grid(age = 8:48, model = factor(shapes, levels = shapes))
sketch$mean <- predict(quad_slope, data.frame(t = sketch$age - 26), re.form = NA)
sketch$gap <- with(sketch, ifelse(model == shapes[1], 0,
                          ifelse(model == shapes[2], 3, 1.5 + .005 * (age - 42)^2)))
sketch_plot <- ggplot(sketch, aes(age)) +
  geom_line(aes(y = mean + gap / 2), colour = birth_colours[3], linewidth = 1.4) +
  geom_line(aes(y = mean - gap / 2), colour = birth_colours[1], linewidth = 1.1, linetype = "dashed") +
  facet_wrap(~model, nrow = 1) + scale_x_continuous(breaks = c(8, 26, 48)) +
  labs(x = "Age (weeks)", y = "Mean height (cm)") +
  theme(strip.text = element_text(face = "bold", colour = color_primary))
save_plot("birth_weight_models", sketch_plot, 10.5, 3.75)

emm_ages <- seq(min(growth$age), max(growth$age), by = .5)
reference_ages <- c(8, 26, 48)
weight_emm <- emmeans(weight_final, ~ w | t,
  at = list(w = references - 3.5, t = emm_ages - 26), lmer.df = "satterthwaite")
pred <- as.data.frame(weight_emm)
pred$age <- pred$t + 26
pred$weight <- factor(pred$w, labels = paste(formatC(references, format = "f", digits = 2), "kg"))
emm_plot <- ggplot(pred, aes(age, emmean, colour = weight, linetype = weight)) +
  geom_line(linewidth = 1.4) +
  geom_point(data = subset(pred, age %in% reference_ages), size = 3.3, show.legend = FALSE) +
  scale_colour_manual(values = birth_colours[c(1, 3)]) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_x_continuous(breaks = c(8, 16, 26, 36, 48, 52)) +
  coord_cartesian(xlim = c(6, 52), ylim = c(54, 82)) +
  labs(x = "Age (weeks)", y = "Mean height (cm)")
save_plot("birth_weight_emms", emm_plot, 9.4, 3.6)

gap <- as.data.frame(confint(pairs(weight_emm, reverse = TRUE), adjust = "none"))
gap$age <- gap$t + 26
marked <- subset(gap, age %in% reference_ages)
gap_plot <- ggplot(gap, aes(age, estimate)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "#DDEBE4") +
  geom_hline(yintercept = 0, colour = color_muted, linewidth = .5) +
  geom_line(colour = color_primary, linewidth = 1.35) +
  geom_point(data = marked, colour = color_primary, size = 2.7) +
  geom_text(data = marked, aes(label = sprintf("%.2f", estimate)), nudge_y = .23,
            fontface = "bold", size = 5, colour = color_primary) +
  scale_x_continuous(breaks = c(8, 16, 26, 36, 48, 52)) +
  labs(x = "Age (weeks)", y = "Height gap (cm)")
save_plot("birth_weight_contrast", gap_plot, 9.4, 3.6)

# Adolescent alcohol-use scores at three common visit ages.
alcohol <- foreign::read.spss(file.path(root, "labs_2526/data/alcoholpp.sav"), to.data.frame = TRUE)
alcohol$id <- factor(alcohol$id)
alcohol$t <- alcohol$age - 14
alcohol$visit <- factor(alcohol$age)
stopifnot(all(table(alcohol$id, alcohol$visit) == 1), !anyNA(alcohol$alcuse))
visit_linear <- lmer(alcuse ~ t + (1 | id), data = alcohol)
visit_factor <- lmer(alcuse ~ visit + (1 | id), data = alcohol)
visit_quad <- lmer(alcuse ~ t + I(t^2) + (1 | id), data = alcohol)
visits3 <- aggregate(alcuse ~ age, alcohol, mean)
observed_scores <- ggplot(alcohol, aes(age, alcuse)) +
  geom_line(aes(group = id, colour = "Individual adolescent"), alpha = .20, linewidth = .48) +
  geom_line(data = visits3, aes(colour = "Visit mean"), linewidth = 1.6) +
  geom_point(data = visits3, shape = 21, fill = "#B14721", colour = "white", stroke = .7, size = 3.5) +
  scale_colour_manual(values = c("Individual adolescent" = "#417472", "Visit mean" = "#B14721"),
                      breaks = c("Individual adolescent", "Visit mean")) +
  guides(colour = guide_legend(override.aes = list(alpha = c(.55, 1), linewidth = c(.6, 1.6)))) +
  scale_x_continuous(NULL, breaks = 14:16, labels = paste("Age", 14:16)) +
  labs(y = "Alcohol-use score") +
  theme(panel.grid.major.x = element_blank())
save_plot("alcohol_observed_trajectories", observed_scores, 12, 5.75)
visits3$t <- visits3$age - 14
visits3$visit <- factor(visits3$age, levels = levels(alcohol$visit))
visits3$linear <- predict(visit_linear, visits3, re.form = NA)
visits3$factor_mean <- predict(visit_factor, visits3, re.form = NA)
visits3$quadratic <- predict(visit_quad, visits3, re.form = NA)
stopifnot(max(abs(visits3$quadratic - visits3$factor_mean)) < 1e-6)
age_grid <- data.frame(age = seq(14, 16, length.out = 101))
age_grid$t <- age_grid$age - 14
age_grid$score <- predict(visit_quad, age_grid, re.form = NA)
visit_plot <- ggplot(age_grid, aes(age, score)) +
  geom_line(colour = color_secondary, linewidth = 1.4) +
  geom_point(data = visits3, aes(y = factor_mean), colour = color_primary, size = 4.2) +
  scale_x_continuous(breaks = 14:16) +
  labs(x = "Age (years)", y = "Mean score")
save_plot("alcohol_visit_means", visit_plot, 6, 3.5)

# Schematic residual spreads illustrate an extension, independently of the study data.
spreads <- data.frame(age = 14:16, sd = c(0.35, 0.6, 0.9))
spread_plot <- ggplot(spreads, aes(age, 0)) +
  geom_hline(yintercept = 0, colour = color_grid) +
  geom_errorbar(aes(ymin = -sd, ymax = sd), width = .22, linewidth = 1.25,
                colour = color_secondary) +
  geom_point(size = 2.4, colour = color_primary) +
  scale_x_continuous(breaks = 14:16) + scale_y_continuous(breaks = -1:1, limits = c(-1, 1)) +
  labs(x = "Age (years)", y = "Residual")
save_plot("visit_residual_spread", spread_plot, 5.5, 2.55)

# Compare the two boys using the common curvature from the random linear slope model in both fits.
# Subtracting the population curve makes the smaller shifts in fitted lines visible.
boy_grid$pooled <- predict(quad_slope, boy_grid) - predict(quad_slope, boy_grid, re.form = NA)
two_boys$deviation <- two_boys$height - predict(quad_slope, two_boys, re.form = NA)
boy_grid$separate <- NA_real_
for (boy in unique(two_boys$id)) {
  rows <- boy_grid$id == boy
  separate_fit <- lm(deviation ~ t, data = two_boys[two_boys$id == boy, ])
  boy_grid$separate[rows] <- predict(separate_fit, boy_grid[rows, ])
}
boy_grid$boy <- paste("Boy", boy_grid$id)
two_boys$boy <- paste("Boy", two_boys$id)
pool_plot <- ggplot(boy_grid, aes(age)) +
  geom_hline(yintercept = 0, colour = color_muted, linewidth = .7) +
  geom_line(aes(y = separate, colour = "Boy alone", linetype = "Boy alone"), linewidth = 1.2) +
  geom_line(aes(y = pooled, colour = "Mixed model", linetype = "Mixed model"), linewidth = 1.3) +
  geom_point(data = two_boys, aes(y = deviation), colour = color_text, size = 2.6) +
  facet_wrap(~ boy, nrow = 1) +
  scale_colour_manual(name = "Fit", values = c("Boy alone" = "#AD641F", "Mixed model" = color_primary)) +
  scale_linetype_manual(name = "Fit", values = c("Boy alone" = "dashed", "Mixed model" = "solid")) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50)) +
  labs(x = "Age (weeks)", y = "Height minus mean (cm)") +
  theme(strip.text = element_text(face = "bold", colour = color_primary),
        legend.key.width = grid::unit(1.3, "cm"))
save_plot("growth_partial_pooling", pool_plot, 10.2, 3.8)

ages <- c(12, 20, 36, 44)
heights <- predict(quad_ri, newdata = data.frame(t = ages - 26), re.form = NA)
results <- list(alcohol_n = nlevels(alcohol$id),
  alcohol_visits = visits3[c("age", "linear", "quadratic", "factor_mean")], children = nlevels(growth$id), age_range = range(growth$age),
  ri_fixed = fixed_results(ri), ri_variance = ri_variance,
  comparison = comparison[c("model", "df", "AIC")],
  full_sd = unname(attr(full_cov, "stddev") / c(1, 10, 100)),
  full_cor = unname(attr(full_cov, "correlation")), full_residual_sd = sigma(quad_full),
  slope_sd = unname(rs_sd["t"]),
  rs_intercept_sd = unname(rs_sd["(Intercept)"]), rs_residual_sd = sigma(quad_slope),
  correlation = attr(VarCorr(quad_slope)$id, "correlation")[1, 2],
  quad_fixed = fixed_results(quad_ri), gain = heights[c(2, 4)] - heights[c(1, 3)],
  selected_fixed = fixed_results(quad_slope),
  birth_cutoffs = cutoffs, birth_references = references,
  overall_test = likelihood_test(age_ml, interaction_ml),
  interaction_test = likelihood_test(weight_ml, interaction_ml),
  birth_fixed = fixed_results(weight_final),
  birth_contrasts = marked[c("age", "estimate", "lower.CL", "upper.CL")])
jsonlite::write_json(results, file.path(assets, "growth_results.json"), auto_unbox = TRUE,
                     digits = 9, pretty = TRUE)

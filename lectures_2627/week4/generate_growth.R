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
# The SPSS label defines breastfeeding status at four weeks.
growth$bf <- factor(growth$bf, levels = c("otherwise", "exclusive breast feeding"),
                    labels = c("Other feeding", "Exclusive breastfeeding"))
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
feeding <- lmer(height ~ t * bf + I(t^2) + (1 + t | id), data = growth, control = control)

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

feeding_colors <- c("Other feeding" = color_primary, "Exclusive breastfeeding" = "#AD641F")
feeding_style <- list(scale_colour_manual(values = feeding_colors),
  scale_linetype_manual(values = c("Other feeding" = "solid", "Exclusive breastfeeding" = "longdash")))
# Use the means at each week to show an empirical trend for each feeding group.
weekly <- aggregate(height ~ age + bf, growth, mean)
eda <- ggplot(weekly, aes(age, height, colour = bf, linetype = bf)) +
  geom_point(size = 2.0, alpha = .60) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, linewidth = 1.4) +
  growth_axes + feeding_style + labs(y = "Mean height (cm)")
save_plot("feeding_observed", eda, 10, 4.1)
feeding_grid <- merge(grid[c("age", "t")], unique(growth["bf"]))
feeding_grid$height <- predict(feeding, newdata = feeding_grid, re.form = NA)
fitted_plot <- ggplot(feeding_grid, aes(age, height, colour = bf, linetype = bf)) +
  geom_vline(xintercept = c(26, 40), colour = color_muted, linetype = 3) +
  geom_line(linewidth = 1.4) + growth_axes + feeding_style + labs(y = "Fitted mean height (cm)") +
  theme(legend.text = element_text(size = 14))
save_plot("feeding_fitted", fitted_plot, 6, 4)

ages <- c(12, 20, 36, 44)
heights <- predict(quad_ri, newdata = data.frame(t = ages - 26), re.form = NA)
# Within-age comparisons use the model's covariance of the fixed-effect estimates.
means <- emmeans(feeding, ~ bf | t, at = list(t = c(0, 14)), lmer.df = "asymptotic")
contrasts <- as.data.frame(confint(pairs(means, reverse = TRUE)))
results <- list(children = nlevels(growth$id), age_range = range(growth$age),
  ri_fixed = fixed_results(ri), ri_variance = ri_variance,
  comparison = comparison[c("model", "df", "AIC")],
  full_sd = unname(attr(full_cov, "stddev") / c(1, 10, 100)),
  full_cor = unname(attr(full_cov, "correlation")), full_residual_sd = sigma(quad_full),
  slope_sd = unname(rs_sd["t"]),
  rs_intercept_sd = unname(rs_sd["(Intercept)"]), rs_residual_sd = sigma(quad_slope),
  correlation = attr(VarCorr(quad_slope)$id, "correlation")[1, 2],
  quad_fixed = fixed_results(quad_ri), gain = heights[c(2, 4)] - heights[c(1, 3)],
  selected_fixed = fixed_results(quad_slope),
  contrasts = contrasts[c("estimate", "asymp.LCL", "asymp.UCL")],
  interaction = unname(tail(fixef(feeding), 1)))
jsonlite::write_json(results, file.path(assets, "growth_results.json"), auto_unbox = TRUE,
                     digits = 9, pretty = TRUE)

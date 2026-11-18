library(ggplot2)
library(jsonlite)
library(nlme)
library(emmeans)

source("slides-common/R/common_fig.R")
assets <- "lectures_2627/week3/assets"
dir.create(assets, recursive = TRUE, showWarnings = FALSE)

# A new trial in the Week 1 setting, with one assigned program per center.
set.seed(262703)
programs <- c("Standard care", "Enhanced care")
# The score pool defines the same CCI probabilities for every patient.
cci_scores <- rep(0:5, times = c(4, 5, 4, 3, 2, 2))
patients_per_center <- 20
expected_cci <- mean(cci_scores)
centers <- data.frame(
  center = 1:6,
  program = factor(sample(rep(programs, each = 3)), levels = programs),
  center_effect = c(-17, 16, 8, -29, 27, -5)
)
patients <- centers[rep(seq_len(nrow(centers)), each = patients_per_center), ]
patients$center <- factor(patients$center)
patients$patient <- seq_len(nrow(patients))
patients$cci <- sample(cci_scores, size = nrow(patients), replace = TRUE)
enhanced <- patients$program == "Enhanced care"

# Comorbidity predicts recovery in both arms. The enhanced program has a
# shallower negative slope, so its expected benefit grows with comorbidity.
patients$distance <- with(patients, round(
  360 + center_effect - 9 * (cci - expected_cci) +
    enhanced * (18 + 6 * (cci - expected_cci)) + rnorm(nrow(patients), sd = 24),
  digits = 1
))
patients$center_effect <- NULL

ordinary <- lm(distance ~ program + cci, data = patients)
adjusted <- lme(distance ~ program + cci, random = ~ 1 | center, data = patients)
interaction <- update(adjusted, distance ~ program * cci)
terms <- c("programEnhanced care", "cci")
ordinary_results <- coef(summary(ordinary))[terms, ]
adjusted_results <- summary(adjusted)$tTable[terms, 1:2]
stopifnot(
  all(table(patients$center) == patients_per_center),
  all(table(centers$program) == 3),
  ordinary_results[1, 2] < adjusted_results[1, 2],
  ordinary_results[2, 2] > adjusted_results[2, 2]
)

line_data <- expand.grid(
  program = factor(programs, levels = programs),
  cci = seq(min(cci_scores), max(cci_scores), by = 0.1)
)
line_data$distance <- predict(interaction, newdata = line_data, level = 0)

adjusted_variances <- as.numeric(VarCorr(adjusted)[, "Variance"])
interaction_sds <- as.numeric(VarCorr(interaction)[, "StdDev"])

# Estimate both treatment means at each selected CCI, then subtract standard care.
selected_cci <- c(1, 4)
means <- emmeans(interaction, ~ program | cci, at = list(cci = selected_cci),
                 mode = "containment")
means_table <- as.data.frame(means)
contrasts <- as.data.frame(confint(pairs(means, reverse = TRUE, adjust = "none")))
standard <- means_table[means_table$program == programs[1], ]
enhanced <- means_table[means_table$program == programs[2], ]
stopifnot(
  identical(contrasts$cci, selected_cci),
  identical(standard$cci, selected_cci),
  identical(enhanced$cci, selected_cci),
  isTRUE(all.equal(
    means_table$emmean, as.numeric(predict(interaction, newdata = means_table, level = 0))
  )),
  isTRUE(all.equal(enhanced$emmean - standard$emmean, contrasts$estimate)),
  isTRUE(all.equal(
    diff(contrasts$estimate) / diff(selected_cci),
    fixef(interaction)[["programEnhanced care:cci"]]
  ))
)

fixed_effect_rows <- function(model) {
  coefficients <- summary(model)$tTable
  ci <- intervals(model, which = "fixed")$fixed[rownames(coefficients), ]
  data.frame(estimate = unname(coefficients[, "Value"]),
             se = unname(coefficients[, "Std.Error"]),
             lower = unname(ci[, "lower"]),
             upper = unname(ci[, "upper"]),
             p_value = unname(coefficients[, "p-value"]))
}
write_json(list(
  patients = nrow(patients), centers = nrow(centers),
  patients_per_center = patients_per_center,
  cci_range = range(patients$cci),
  center_mean_cci_range = range(tapply(patients$cci, patients$center, mean)),
  ordinary = data.frame(estimate = unname(ordinary_results[, 1]),
                        se = unname(ordinary_results[, 2]),
                        p_value = unname(ordinary_results[, "Pr(>|t|)"])),
  random_intercept = list(
    terms = fixed_effect_rows(adjusted),
    center_variance = adjusted_variances[1],
    residual_variance = adjusted_variances[2],
    icc = adjusted_variances[1] / sum(adjusted_variances)
  ),
  interaction = list(
    terms = fixed_effect_rows(interaction),
    center_sd = interaction_sds[1],
    residual_sd = interaction_sds[2],
    treatment_effects = data.frame(cci = selected_cci,
                                   standard = standard$emmean, enhanced = enhanced$emmean,
                                   estimate = contrasts$estimate,
                                   lower = contrasts$lower.CL, upper = contrasts$upper.CL)
  )
), file.path(assets, "model_results.json"), auto_unbox = TRUE, digits = 8, pretty = TRUE)
write.csv(patients, file.path(assets, "rehabilitation_cluster_trial.csv"), row.names = FALSE)

arm_colors <- setNames(c(color_primary, color_accent), programs)
centers$row <- ave(centers$center, centers$program, FUN = seq_along)
allocation <- ggplot(centers, aes(program, 4 - row)) +
  geom_tile(aes(fill = program), width = .84, height = .69, alpha = .16) +
  geom_text(aes(label = paste("Center", center)), family = "Liberation Sans",
            size = 6.5, color = color_text) +
  scale_fill_manual(values = arm_colors, guide = "none") +
  scale_x_discrete(position = "top", expand = expansion(add = .56)) +
  scale_y_continuous(expand = expansion(add = .16)) +
  theme_void(base_family = "Liberation Sans", base_size = 19) +
  theme(axis.text.x.top = element_text(face = "bold", color = color_primary,
                                       margin = margin(b = 8)),
        plot.margin = margin(8, 4, 5, 4))
ggsave(file.path(assets, "center_allocation.png"), allocation,
       width = 6.1, height = 3.8, dpi = 230, bg = color_white)

outcome_theme <- theme_minimal(base_family = "Liberation Sans", base_size = 16) +
  theme(axis.text = element_text(color = color_text),
        axis.title = element_text(color = color_primary),
        axis.title.y = element_text(margin = margin(r = 8)),
        axis.title.x = element_text(margin = margin(t = 8)),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = color_grid),
        plot.margin = margin(4, 12, 9, 10))

distance_limits <- range(patients$distance)
by_center <- ggplot(patients, aes(center, distance, fill = program)) +
  geom_boxplot(width = .63, outlier.shape = NA, color = color_primary,
               alpha = .55, linewidth = .7) +
  scale_fill_manual(values = arm_colors) +
  coord_cartesian(ylim = distance_limits) +
  labs(x = "Center", y = "Observed walking distance (m)", fill = NULL) +
  outcome_theme +
  theme(legend.position = "top")

by_cci <- ggplot(patients, aes(cci, distance)) +
  geom_point(size = 2.2, alpha = .50, color = color_primary) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = color_primary_dark, linewidth = 1) +
  scale_x_continuous(breaks = sort(unique(cci_scores))) +
  coord_cartesian(ylim = distance_limits) +
  labs(x = "Baseline CCI (points)", y = "Observed walking distance (m)") +
  outcome_theme +
  # Match the plotting height of the boxplots, which have a legend above them.
  theme(plot.margin = margin(52, 12, 9, 10))

ggsave(file.path(assets, "walking_distance_by_center.png"), by_center,
       width = 5.4, height = 3.85, dpi = 230, bg = color_white)
ggsave(file.path(assets, "walking_distance_by_cci.png"), by_cci,
       width = 5.4, height = 3.85, dpi = 230, bg = color_white)

fit_plot <- ggplot(patients, aes(cci, distance, color = program)) +
  geom_point(size = 2.2, alpha = .50, show.legend = FALSE) +
  geom_line(data = line_data, aes(linetype = program), linewidth = 1.25) +
  scale_color_manual(values = arm_colors) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  scale_x_continuous(breaks = sort(unique(cci_scores))) +
  coord_cartesian(ylim = distance_limits) +
  labs(x = "Baseline CCI (points)",
       y = "Walking distance (m)", color = NULL, linetype = NULL) +
  outcome_theme +
  theme(legend.position = "top", legend.key.width = grid::unit(1.2, "cm"),
        plot.margin = margin(4, 17, 10, 10))
ggsave(file.path(assets, "program_comorbidity_interaction.png"), fit_plot,
       width = 7.25, height = 4.6, dpi = 230, bg = color_white)

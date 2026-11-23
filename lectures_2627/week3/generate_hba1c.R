library(lmerTest)
library(emmeans)
library(ggplot2)
library(jsonlite)
source("slides-common/R/common_fig.R")
assets <- "lectures_2627/week3/assets"

# One record per patient, with program status constant within a practice.
set.seed(262706)
programs <- c("Usual care", "Structured follow-up")
practices <- data.frame(
  practice = 1:30, program = sample(rep(programs, each = 15)),
  n = sample(30:65, 30), intercept = rnorm(30, sd = 0.25),
  slope = rnorm(30, sd = 0.20)
)
patients <- practices[rep(seq_len(nrow(practices)), practices$n), ]
n <- nrow(patients)
patients$patient <- seq_len(n)
patients$baseline <- round(6.5 + 4 * rbeta(n, 2.1, 2.7), 1)
baseline_c <- patients$baseline - 8
structured <- patients$program == programs[2]
patients$month6 <- with(patients, round(baseline - 0.55 + intercept -
  0.23 * structured + (-0.25 - 0.35 * structured + slope) * baseline_c +
  rnorm(n, sd = 0.48), 1))
patients$change <- round(patients$month6 - patients$baseline, 1)
patients <- patients[c("patient", "practice", "program", "baseline", "month6", "change")]
write.csv(patients, file.path(assets, "hba1c_patients.csv"), row.names = FALSE)

patients$baseline_c <- patients$baseline - 8
patients$practice <- factor(patients$practice)
patients$program <- factor(patients$program, levels = programs)
intercept <- lmer(change ~ baseline_c + program + (1 | practice), data = patients)
slope <- lmer(change ~ baseline_c + program + (baseline_c | practice), data = patients)
full <- update(slope, . ~ . + baseline_c:program)
reduced <- update(intercept, . ~ . + baseline_c:program)
lrt <- anova(reduced, full, refit = FALSE)
stopifnot(!isSingular(full), !isSingular(slope), nobs(reduced) == nobs(full))

means <- emmeans(full, ~ program | baseline_c, at = list(baseline_c = 0:1),
                 lmer.df = "satterthwaite")
mean_table <- as.data.frame(means)
contrasts <- as.data.frame(summary(pairs(means, reverse = TRUE), infer = c(TRUE, TRUE)))
interaction_term <- coef(summary(full))["baseline_c:programStructured follow-up", ]
stopifnot(isTRUE(all.equal(diff(contrasts$estimate), unname(interaction_term[1]))))

# Select by practice size alone. Use the same patient points and coordinates in every panel.
ranked <- practices[order(-practices$n, practices$practice), ]
shown <- do.call(rbind, lapply(programs, function(p) head(ranked[ranked$program == p, ], 3)))
shown$label <- paste0("Practice ", shown$practice, " · ", shown$program)
points <- patients[patients$practice %in% shown$practice, ]
points$panel <- factor(shown$label[match(points$practice, shown$practice)], levels = shown$label)
grid <- unique(points[c("practice", "program", "panel")])
grid <- merge(grid, data.frame(baseline = c(7, 10)), by = NULL)
grid$baseline_c <- grid$baseline - 8
colors <- setNames(c(color_primary, "#AB611E"), programs)
common <- list(
  geom_hline(yintercept = 0, colour = color_muted, linewidth = .38),
  geom_vline(xintercept = 8, colour = color_muted, linetype = "dashed", linewidth = .38),
  scale_color_manual(values = colors, guide = "none"),
  scale_x_continuous(breaks = 7:10), scale_y_continuous(breaks = seq(-4, 2)),
  coord_cartesian(xlim = range(points$baseline), ylim = range(points$change)),
  labs(x = "HbA1c at diagnosis (%)", y = "Six-month change\n(percentage points)"),
  theme_minimal(base_size = 15, base_family = "Liberation Sans"),
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", color = color_text),
        axis.title = element_text(color = color_text), panel.spacing = unit(0.8, "lines"))
)
base <- ggplot(points, aes(baseline, change, colour = program)) + common +
  geom_point(alpha = .65, size = 1.6) + facet_wrap(~ panel, ncol = 3)
save_figure <- function(plot, name) {
  ggsave(file.path(assets, paste0("hba1c_", name, ".png")), plot, width = 12,
         height = 4.45, units = "in", dpi = 170, bg = "white")
}
eda <- base + geom_smooth(aes(group = practice), method = "lm", formula = y ~ x,
                          se = FALSE, linewidth = 1.15)
save_figure(eda, "eda")
for (name in c("intercept", "slope")) {
  model <- if (name == "intercept") intercept else slope
  grid$change <- predict(model, newdata = grid)
  save_figure(base + geom_line(data = grid, aes(group = practice), linewidth = 1.25), name)
}

# Compare population-average changes at common baseline values for the two programs.
average <- expand.grid(baseline = 7:10, program = factor(programs, levels = programs))
average$baseline_c <- average$baseline - 8
average$change <- predict(full, newdata = average, re.form = NA)
comparison_points <- transform(mean_table, baseline = baseline_c + 8, change = emmean)
stopifnot(isTRUE(all.equal(comparison_points$change,
                           as.numeric(predict(full, newdata = comparison_points, re.form = NA)))))
program_plot <- ggplot(average, aes(baseline, change, colour = program)) +
  geom_hline(yintercept = 0, colour = color_muted, linewidth = .45) +
  geom_line(linewidth = 1.6) +
  geom_point(data = comparison_points, size = 3) +
  scale_color_manual(values = colors, name = NULL) +
  scale_x_continuous(breaks = 7:10, expand = expansion(mult = .04)) +
  scale_y_continuous(breaks = seq(-2, 0, .5), expand = expansion(mult = c(.10, .08))) +
  labs(x = "HbA1c at diagnosis (%)", y = "Expected six-month change in HbA1c
(percentage points)") +
  theme_minimal(base_size = 15, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), axis.title = element_text(color = color_text),
        axis.title.x = element_text(margin = margin(t = 7)), legend.position = "top",
        legend.text = element_text(size = 13), legend.margin = margin(0, 0, 0, 0))
ggsave(file.path(assets, "hba1c_interaction.png"), program_plot,
       width = 6.0, height = 4.8, units = "in", dpi = 170, bg = "white")

slope_sd <- function(model) unname(attr(VarCorr(model)$practice, "stddev")["baseline_c"])
source("lectures_2627/week3/slope_distribution.R")
save_slope_distribution(file.path(assets, "hba1c_practice_slopes.png"),
                        fixef(slope)[["baseline_c"]], slope_sd(slope), "Practice slope for baseline HbA1c")
comparison <- data.frame(
  baseline = contrasts$baseline_c + 8, usual = mean_table$emmean[mean_table$program == programs[1]],
  structured = mean_table$emmean[mean_table$program == programs[2]], difference = contrasts$estimate,
  lower = contrasts$lower.CL, upper = contrasts$upper.CL, p = contrasts$p.value
)
# Match the coefficient tests and intervals to the Satterthwaite output used in class.
fixed_results <- function(model) {
  tab <- coef(summary(model))
  half_width <- qt(.975, tab[, "df"]) * tab[, "Std. Error"]
  data.frame(row.names = NULL, term = rownames(tab), estimate = tab[, "Estimate"], se = tab[, "Std. Error"],
             lower = tab[, "Estimate"] - half_width, upper = tab[, "Estimate"] + half_width,
             p = tab[, "Pr(>|t|)"])
}
practice_slopes <- coef(slope)$practice
write_json(list(
  patients = n, practices = nrow(practices), displayed = shown[c("practice", "program", "n")],
  common_slope = fixef(intercept)[["baseline_c"]], mean_slope = fixef(slope)[["baseline_c"]],
  slope_sd = slope_sd(slope), full_slope_sd = slope_sd(full),
  shown_slopes = practice_slopes[as.character(shown$practice), "baseline_c"],
  interaction = unname(interaction_term["Estimate"]), interaction_p = unname(interaction_term["Pr(>|t|)"]),
  additive_fixed = fixed_results(slope), interaction_fixed = fixed_results(full),
  intercept_sd = unname(attr(VarCorr(slope)$practice, "stddev")[1]),
  residual_sd = sigma(slope), slope_correlation = unname(cov2cor(VarCorr(slope)$practice)[1, 2]),
  full_intercept_sd = unname(attr(VarCorr(full)$practice, "stddev")[1]),
  full_residual_sd = sigma(full), full_correlation = unname(cov2cor(VarCorr(full)$practice)[1, 2]),
  means = comparison, loglik = lrt$logLik, parameters = lrt$npar,
  chisq = lrt$Chisq[2], df = lrt$Df[2], lrt_p = lrt[["Pr(>Chisq)"]][2]
), file.path(assets, "hba1c_results.json"), pretty = TRUE, auto_unbox = TRUE, digits = NA)

# Typeset the model equations and distributions using the same LaTeX/PNG format as Week 2.
equations <- c(
  intercept = r"(Y_{ij}=(\beta_0+c_j)+\beta_1 B_{ij}+\beta_2 P_j+\varepsilon_{ij})",
  slope = r"(Y_{ij}=(\beta_0+c_j)+(\beta_1+d_j) B_{ij}+\beta_2 P_j+\varepsilon_{ij})",
  practice = r"(\displaystyle \begin{pmatrix}c_j\\d_j\end{pmatrix}\overset{\mathrm{iid}}{\sim}\mathcal{N}_2\!\left(\begin{pmatrix}0\\0\end{pmatrix},\begin{pmatrix}\sigma_c^2 & \sigma_{cd}\\ \sigma_{cd} & \sigma_d^2\end{pmatrix}\right))",
  errors = r"(\displaystyle \varepsilon_{ij}\overset{\mathrm{iid}}{\sim}\mathcal{N}(0,\sigma^2))",
  interaction = r"(Y_{ij}=(\beta_0+c_j)+(\beta_1+\beta_3 P_j+d_j) B_{ij}+\beta_2 P_j+\varepsilon_{ij})"
)
for (name in names(equations)) {
  tex <- file.path(tempdir(), paste0(name, ".tex"))
  writeLines(c(r"(\documentclass[border=1pt]{standalone})", r"(\usepackage{amsmath})",
               r"(\begin{document})", paste0("$", equations[[name]], "$"), r"(\end{document})"), tex)
  status <- system2("pdflatex", c("-interaction=batchmode", "-halt-on-error",
                                  paste0("-output-directory=", tempdir()), tex), stdout = FALSE)
  stopifnot(status == 0)
  status <- system2("pdftocairo", c("-png", "-transp", "-singlefile", "-r", "600",
                    file.path(tempdir(), paste0(name, ".pdf")),
                    file.path(assets, paste0("hba1c_eq_", name))))
  stopifnot(status == 0)
}

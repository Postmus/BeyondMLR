# Reproduce the simulated crossed case study from the 2025/26 Week 2 lecture.
suppressPackageStartupMessages({ library(lme4); library(ggplot2); library(jsonlite) })
args <- commandArgs(FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)])))
source(file.path(here, "../../slides-common/R/common_fig.R"))
out <- file.path(here, "assets/radiologists")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(123)
dat <- expand.grid(patient = 1:10, radiologist = 1:5)
patient_effect <- 3 * rnorm(10)
radiologist_effect <- 2 * rnorm(5)
dat$measurement <- 50 + patient_effect[dat$patient] +
  radiologist_effect[dat$radiologist] + rnorm(nrow(dat), 0, 1)
dat$patient <- factor(dat$patient)
dat$radiologist <- factor(dat$radiologist)
stopifnot(nrow(dat) == 50, all(table(dat$patient, dat$radiologist) == 1))
write.csv(dat, file.path(out, "radiologist_measurements.csv"), row.names = FALSE)
fit <- lmer(measurement ~ 1 + (1 | patient) + (1 | radiologist), data = dat,
            REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
stopifnot(!isSingular(fit), is.null(fit@optinfo$conv$lme4$messages))
v <- as.data.frame(VarCorr(fit))
patient_var <- v$vcov[v$grp == "patient"]
radiologist_var <- v$vcov[v$grp == "radiologist"]
residual_var <- sigma(fit)^2
total <- patient_var + radiologist_var + residual_var
results <- list(seed = 123, n_patients = 10, n_radiologists = 5, n = 50,
  model = "measurement ~ 1 + (1 | patient) + (1 | radiologist)", method = "REML",
  mean = unname(fixef(fit)[1]),
  variance_components = list(patient = patient_var, radiologist = radiologist_var,
                             residual = residual_var, total = total),
  icc_absolute_agreement_single = patient_var / total,
  versions = list(R = R.version.string, lme4 = as.character(packageVersion("lme4"))))
write_json(results, file.path(out, "model_results.json"), pretty = TRUE, auto_unbox = TRUE, digits = 12)
writeLines(trimws(capture.output(summary(fit)), which = "right"), file.path(out, "model_summary.txt"))

p <- ggplot(dat, aes(patient, measurement, group = radiologist,
                     colour = radiologist, linetype = radiologist)) +
  geom_line(linewidth = .85) + geom_point(size = 2.0) +
  scale_colour_manual(values = c(color_primary, color_accent, color_secondary, "#855C85", "#287D86")) +
  labs(x = "Patient", y = "Tumor diameter (mm)", colour = "Radiologist", linetype = "Radiologist") +
  theme_minimal(base_size = 21, base_family = "Liberation Sans") +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = color_grid),
        axis.text = element_text(colour = color_text), text = element_text(colour = color_text),
        legend.position = "top", legend.key.width = grid::unit(.55, "in"),
        plot.margin = margin(12, 18, 10, 10))
ggsave(file.path(out, "measurements.png"), p, width = 12.21, height = 5.98, dpi = 240, bg = "white")
ggsave(file.path(out, "measurements.pdf"), p, width = 12.21, height = 5.98, device = cairo_pdf, bg = "white")
print(results)

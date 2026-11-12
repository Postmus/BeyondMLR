# Conceptual population sampling: true means only, without patient noise.
library(grid)
args <- commandArgs(FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)])))
source(file.path(here, "../../slides-common/R/common_fig.R"))
out <- file.path(here, "assets/random-center-samples")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20261111)
# Two independent samples of four centers from the same population.
# Within each row reuse the intercept draws across model illustrations.
cj <- rnorm(8, 0, 4)
dj <- 4 * (.5 * cj / 4 + sqrt(1 - .5^2) * rnorm(8))
means <- data.frame(sample = rep(c("First sample", "Second sample"), each = 4),
                    center = paste("Hospital", LETTERS[1:8]),
                    control = 2 + cj,
                    intercept_only_cpap = 5.1 + cj,
                    random_slope_cpap = 5.1 + cj + dj,
                    intercept_deviation = cj, treatment_deviation = dj)
write.csv(means, file.path(out, "true_center_means.csv"), row.names = FALSE)
stopifnot(all(abs(means$intercept_only_cpap - means$control - 3.1) < 1e-10),
          !anyDuplicated(means$center),
          abs(mean(dj[1:4]) - mean(dj[5:8])) > 1e-10)

y_breaks <- pretty(range(means$control, means$intercept_only_cpap, means$random_slope_cpap), n = 4)
y_limits <- range(y_breaks)

W <- 12.21
H <- 4.58 # Full figure slot, after caption and takeaway.
draw <- function() {
  grid.newpage()
  label <- function(s, x, y, size = 18, col = color_text, face = "plain", rot = 0, just = "centre") {
    grid.text(s, unit(x, "in"), unit(y, "in"), rot = rot, just = just,
              gp = gpar(fontfamily = "Liberation Sans", fontsize = size, col = col, fontface = face))
  }
  line <- function(x, y, col = color_grid, width = 1) {
    grid.lines(unit(x, "in"), unit(y, "in"), gp = gpar(col = col, lwd = width))
  }
  label("Random intercepts", 3.62, 4.36, 20, face = "bold")
  label("Random intercepts and slopes", 9.39, 4.36, 20, face = "bold")
  for (k in 1:2) {
    lx <- c(4.50, 6.50)[k]
    col <- c(color_primary, color_accent)[k]
    line(c(lx, lx + .30), c(3.98, 3.98), col, 2.6)
    label(c("Control", "CPAP")[k], lx + .40, 3.98, 17, just = "left")
  }
  for (row in 1:2) {
    ids <- if (row == 1) 1:4 else 5:8
    bottom <- if (row == 1) 2.50 else .48
    top <- bottom + 1.22
    label(c("First sample", "Second sample")[row], .26, (bottom + top) / 2,
          19, face = "bold", rot = 90)
    for (column in 1:2) {
      left <- c(1.13, 6.90)[column]
      right <- left + 4.88
      xpos <- seq(left + .32, right - .32, length.out = 4)
      ypos <- function(value) bottom + (value - y_limits[1]) / diff(y_limits) * (top - bottom)
      for (value in y_breaks) {
        line(c(left, right), rep(ypos(value), 2), width = .65)
        label(as.character(value), left - .12, ypos(value), 16, color_muted, just = "right")
      }
      values <- list(means$control[ids],
                     if (column == 1) means$intercept_only_cpap[ids] else means$random_slope_cpap[ids])
      for (k in 1:2) {
        col <- c(color_primary, color_accent)[k]
        line(xpos, ypos(values[[k]]), col, 2.6)
        grid.points(unit(xpos, "in"), unit(ypos(values[[k]]), "in"),
                    pch = 16, size = unit(.095, "in"), gp = gpar(col = col))
      }
      for (j in 1:4) label(means$center[ids[j]], xpos[j], bottom - .19, 16)
    }
  }
}
png(file.path(out, "two-center-samples.png"), width = W, height = H,
    units = "in", res = 240, bg = "white")
draw()
dev.off()
cairo_pdf(file.path(out, "two-center-samples.pdf"), width = W, height = H)
draw()
dev.off()
cat("Random-slope sample-average effects:", tapply(3.1 + dj, means$sample, mean), "mmHg\n")

# Covariance illustration: mirrored deviations give equal spreads and opposite
# covariance signs. These are true center values, not estimated random effects.
library(ggplot2)
set.seed(20261111)
baseline_deviation <- rnorm(40, 0, 4)
effect_deviation <- 3 * (.75 * baseline_deviation / 4 + sqrt(1 - .75^2) * rnorm(40))
covariance_data <- rbind(
  data.frame(covariance = "Positive", center = 1:40,
             control_mean = 2 + baseline_deviation, effect = 3.1 + effect_deviation),
  data.frame(covariance = "Negative", center = 1:40,
             control_mean = 2 + baseline_deviation, effect = 3.1 - effect_deviation)
)
write.csv(covariance_data, file.path(out, "covariance_true_values.csv"), row.names = FALSE)
positive_cov <- cov(2 + baseline_deviation, 3.1 + effect_deviation)
negative_cov <- cov(2 + baseline_deviation, 3.1 - effect_deviation)
stopifnot(positive_cov > 0, negative_cov < 0,
          abs(positive_cov + negative_cov) < 1e-10,
          abs(var(3.1 + effect_deviation) - var(3.1 - effect_deviation)) < 1e-10)
x_radius <- ceiling(max(abs(baseline_deviation)) / 5) * 5
y_radius <- ceiling(max(abs(effect_deviation)) / 5) * 5
plots <- lapply(c("Positive", "Negative"), function(sign) {
  ggplot(subset(covariance_data, covariance == sign), aes(control_mean, effect)) +
    geom_point(color = color_primary, size = 2.5, alpha = .85) +
    scale_x_continuous(limits = 2 + c(-x_radius, x_radius),
                       breaks = seq(-15, 20, 5)) +
    scale_y_continuous(limits = 3.1 + c(-y_radius, y_radius),
                       breaks = seq(-10, 15, 5)) +
    labs(title = paste(sign, "covariance"),
         subtitle = paste0("Higher control means tend to accompany\n",
                           if (sign == "Positive") "larger treatment effects." else "smaller treatment effects."),
         x = "True control mean (mmHg)", y = "True CPAP effect (mmHg)") +
    theme_minimal(base_size = 18, base_family = "Liberation Sans") +
    theme(plot.title = element_text(size = 20, face = "bold", color = color_primary),
          plot.subtitle = element_text(size = 17, margin = margin(b = 10)),
          axis.title = element_text(size = 18), axis.text = element_text(size = 16),
          panel.grid.minor = element_blank(),
          plot.margin = margin(10, 15, 10, 8))
})
draw_covariance <- function() {
  grid.newpage()
  for (k in 1:2) print(plots[[k]], newpage = FALSE,
    vp = viewport(x = c(.25, .75)[k], y = .5, width = .5, height = 1))
}
png(file.path(out, "covariance-examples.png"), width = W, height = H,
    units = "in", res = 240, bg = "white")
draw_covariance()
dev.off()
cairo_pdf(file.path(out, "covariance-examples.pdf"), width = W, height = H)
draw_covariance()
dev.off()

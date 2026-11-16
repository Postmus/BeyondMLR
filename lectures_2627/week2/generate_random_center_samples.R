# Conceptual population sampling: true means only, without patient noise.
library(grid)
args <- commandArgs(FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)])))
source(file.path(here, "../../slides-common/R/common_fig.R"))
out <- file.path(here, "assets/random-center-samples")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(20261111)
# Two independent samples from one bivariate Normal distribution.
# Hospital labels A-D are local to each sample, so the sample-label pair is unique.
cj <- rnorm(8, 0, 4)
dj <- 4 * (.5 * cj / 4 + sqrt(1 - .5^2) * rnorm(8))
means <- data.frame(sample = rep(c("First sample", "Second sample"), each = 4),
                    center = rep(paste("Hospital", LETTERS[1:4]), 2),
                    control = 2 + cj,
                    random_slope_cpap = 5.1 + cj + dj,
                    intercept_deviation = cj, treatment_deviation = dj)
write.csv(means, file.path(out, "true_center_means.csv"), row.names = FALSE)
stopifnot(!anyDuplicated(means[c("sample", "center")]),
          all(abs(means$control - (2 + cj)) < 1e-10),
          all(abs(means$random_slope_cpap - means$control - (3.1 + dj)) < 1e-10))

y_breaks <- seq(-10, 20, 10)
y_limits <- c(-15, 20)
W <- 12.21
H <- 4.58 # Covariance figure slot.
sample_H <- 4.35 # Population-and-samples slot, with a taller callout.
draw <- function() {
  grid.newpage()
  label <- function(s, x, y, size = 18, col = color_text, face = "plain", rot = 0, just = "centre") {
    grid.text(s, unit(x, "in"), unit(y, "in"), rot = rot, just = just,
              gp = gpar(fontfamily = "Liberation Sans", fontsize = size, col = col, fontface = face))
  }
  line <- function(x, y, col = color_grid, width = 1, arrow = NULL) {
    grid.lines(unit(x, "in"), unit(y, "in"), arrow = arrow,
               gp = gpar(col = col, lwd = width))
  }
  label("Population distribution", 2.65, 4.10, 20, face = "bold")
  # Nested equal-density contours of the distribution used for both samples.
  left <- 1.15; right <- 4.55; bottom <- .88; top <- 3.73
  px <- function(x) left + (x + 12) / 24 * (right - left)
  py <- function(y) bottom + (y + 12) / 24 * (top - bottom)
  theta <- seq(0, 2 * pi, length.out = 241)
  for (k in 1:3) {
    radius <- sqrt(qchisq(c(.95, .75, .40)[k], df = 2))
    x <- 4 * radius * cos(theta)
    y <- 4 * radius * (.5 * cos(theta) + sqrt(1 - .5^2) * sin(theta))
    grid.polygon(unit(px(x), "in"), unit(py(y), "in"),
      gp = gpar(fill = c(color_panel_cool, color_secondary_light, color_secondary)[k],
                col = color_primary, lwd = .8))
  }
  line(c(left, right), c(py(0), py(0)), color_muted, .7)
  line(c(px(0), px(0)), c(bottom, top), color_muted, .7)
  line(c(left, right), c(bottom, bottom), color_muted)
  line(c(left, left), c(bottom, top), color_muted)
  for (value in c(-10, 0, 10)) {
    label(as.character(value), px(value), bottom - .17, 16, color_muted)
    label(as.character(value), left - .14, py(value), 16, color_muted, just = "right")
  }
  label(expression(paste("Intercept deviation ", c[j], " (mmHg)")),
        (left + right) / 2, .30, 18)
  label(expression(paste("Treatment-effect deviation ", d[j])),
        .30, (bottom + top) / 2, 18, rot = 90)

  # The same population supplies a fresh set of effect pairs for each sample.
  for (destination in c(3.0, 1.1)) {
    line(c(4.75, 5.35, 6.0), c(2.30, destination, destination),
         color_muted, 1.6, arrow = arrow(length = unit(.12, "in"), type = "closed"))
  }
  for (k in 1:2) {
    lx <- c(7.30, 9.25)[k]
    col <- c(color_primary, color_accent)[k]
    line(c(lx, lx + .30), c(4.10, 4.10), col, 2.6)
    label(c("Control", "CPAP")[k], lx + .40, 4.10, 18, just = "left")
  }
  for (row in 1:2) {
    ids <- if (row == 1) 1:4 else 5:8
    bottom <- if (row == 1) 2.47 else .48
    top <- bottom + 1.06
    left <- 6.55; right <- 11.80
    label(c("First sample", "Second sample")[row], (left + right) / 2,
          top + .24, 19, face = "bold")
    xpos <- seq(left + .32, right - .32, length.out = 4)
    ypos <- function(value) bottom + (value - y_limits[1]) / diff(y_limits) * (top - bottom)
    for (value in y_breaks) {
      line(c(left, right), rep(ypos(value), 2), width = .65)
      label(as.character(value), left - .12, ypos(value), 16, color_muted, just = "right")
    }
    values <- list(means$control[ids], means$random_slope_cpap[ids])
    for (k in 1:2) {
      col <- c(color_primary, color_accent)[k]
      line(xpos, ypos(values[[k]]), col, 2.6)
      grid.points(unit(xpos, "in"), unit(ypos(values[[k]]), "in"),
                  pch = 16, size = unit(.095, "in"), gp = gpar(col = col))
    }
    for (j in 1:4) label(means$center[ids[j]], xpos[j], bottom - .20, 16)
  }
}
png(file.path(out, "two-center-samples.png"), width = W, height = sample_H,
    units = "in", res = 240, bg = "white")
draw()
dev.off()
cairo_pdf(file.path(out, "two-center-samples.pdf"), width = W, height = sample_H)
draw()
dev.off()

# Allow focused regeneration without changing the separate covariance figure.
if ("--samples-only" %in% commandArgs(TRUE)) quit(save = "no")

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

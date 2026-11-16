# Worked Assignment 1 figures, fitted to the original student dataset.
args <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)])))
source(file.path(here, "../../slides-common/R/common_fig.R"))
library(grid)
out <- file.path(here, "assets/assignment1-recap")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
input <- file.path(here, "../../labs_2627/assignments/week1/downloads/protein_yield.csv")
d <- read.csv(input)
d$donor <- factor(d$donor)
d$protocol <- factor(d$protocol, levels = c("Standard", "Modified"))
stopifnot(nrow(d) == 80, all(table(d$donor, d$protocol) == 4))
model <- nlme::lme(protein_yield ~ protocol, random = ~ 1 | donor,
                  data = d, method = "REML")
means <- aggregate(protein_yield ~ donor + protocol, d, mean)
means$fitted <- as.numeric(predict(model, newdata = means, level = 1))
coef <- summary(model)$tTable["protocolModified", ]
v <- as.numeric(nlme::VarCorr(model)[, "Variance"])
ci <- coef["Value"] + c(-1, 1) * qt(.975, coef["DF"]) * coef["Std.Error"]
results <- list(
  data_md5 = unname(tools::md5sum(input)),
  fitting = "Random-intercept REML via nlme; protocol t interval with 69 df.",
  intercept = unname(nlme::fixef(model)[1]),
  modified_mean = unname(sum(nlme::fixef(model))),
  estimate = unname(coef["Value"]), se = unname(coef["Std.Error"]),
  df = unname(coef["DF"]), lower_95 = ci[1], upper_95 = ci[2],
  donor_variance = v[1], residual_variance = v[2],
  donor_sd = sqrt(v[1]), residual_sd = sqrt(v[2]), icc = v[1] / sum(v)
)
reference <- jsonlite::read_json(file.path(here, "assets/allocation-imbalance/model_results.json"), simplifyVector = TRUE)
r <- subset(reference$results, design == "Assignment 1 balanced" & model == "Random donor effects")
stopifnot(abs(results$estimate - r$estimate) < 1e-6,
          abs(results$se - r$se) < 1e-6,
          max(abs(ci - c(r$lower_95, r$upper_95))) < 1e-6,
          abs(v[1] - r$donor_variance) < 1e-3)
standard <- subset(means, protocol == "Standard")
modified <- subset(means, protocol == "Modified")
stopifnot(max(abs(modified$fitted - standard$fitted - results$estimate)) < 1e-8)
write.csv(means, file.path(out, "observed_and_fitted_means.csv"), row.names = FALSE)
jsonlite::write_json(results, file.path(out, "model_results.json"), pretty = TRUE, auto_unbox = TRUE, digits = 12)

# All dimensions are physical slide inches, keeping labels readable after embedding.
W <- 12.21
colors <- c(Standard = color_primary, Modified = color_accent)
label <- function(value, x, y, size = 19, col = color_text, just = "centre", rot = 0, face = "plain") {
  grid.text(value, x = unit(x, "in"), y = unit(y, "in"), just = just, rot = rot,
            gp = gpar(fontfamily = "Liberation Sans", fontsize = size, col = col, fontface = face))
}
line <- function(x, y, col = color_grid, lwd = 1, lty = 1) {
  grid.lines(unit(x, "in"), unit(y, "in"), gp = gpar(col = col, lwd = lwd, lty = lty))
}
dots <- function(x, y, col, pch = 16, size = .065) {
  grid.points(unit(x, "in"), unit(y, "in"), pch = pch, size = unit(size, "in"),
              gp = gpar(col = col, fill = col))
}
save_figure <- function(name, width, height, draw) {
  png(file.path(out, paste0(name, ".png")), width = width, height = height,
      units = "in", res = 180, type = "cairo", bg = color_bg)
  grid.newpage(); draw(); dev.off()
}
draw_means <- function(H, fitted = FALSE) {
  left <- 1.02; right <- W - .30; bottom <- 1.03; top <- H - .68
  px <- function(x) left + (x - .65) / 9.7 * (right - left)
  py <- function(y) bottom + (y - 15) / 80 * (top - bottom)
  for (y in if (fitted) seq(20, 80, 20) else seq(20, 90, 10)) {
    line(c(left, right), rep(py(y), 2))
    label(y, left - .15, py(y), size = 18, just = "right")
  }
  for (j in 1:10) label(j, px(j), bottom - .20, size = 18)
  label("Donor", (left + right) / 2, .53, size = 20)
  label("Protein yield (µg/mg)", .22, (bottom + top) / 2, size = 20, rot = 90)
  for (k in 1:2) {
    x <- c(4.5, 7.0)[k]; col <- colors[k]
    line(c(x - .40, x - .05), rep(H - .22, 2), col, 2.5)
    label(names(colors)[k], x + .07, H - .22, size = 19, col = col, just = "left")
  }
  if (!fitted) {
    set.seed(20261116)
    for (protocol in names(colors)) {
      dd <- d[d$protocol == protocol, ]
      line(c(left, right), rep(py(mean(dd$protein_yield)), 2), colors[protocol], 1.6, 2)
      dots(px(as.integer(dd$donor) + runif(nrow(dd), -.14, .14)), py(dd$protein_yield),
           adjustcolor(colors[protocol], alpha.f = .42), size = .072)
    }
  } else {
    for (protocol in names(colors)) {
      mm <- means[means$protocol == protocol, ]
      line(px(as.integer(mm$donor)), py(mm$fitted), colors[protocol], 2.5)
    }
  }
  for (protocol in names(colors)) {
    mm <- means[means$protocol == protocol, ]
    dots(px(as.integer(mm$donor)), py(mm$protein_yield), colors[protocol], pch = 18, size = .14)
  }
  if (fitted) {
    # The low-yield donor leaves clear space to label the common fitted gap.
    x <- px(6)
    ys <- py(c(standard$fitted[6], modified$fitted[6]))
    line(c(x, x), ys, color_text, 1.5)
    for (y in ys) line(c(x - .065, x + .065), c(y, y), color_text, 1.5)
    line(c(x + .07, x + .43), rep(mean(ys), 2), color_muted, 1)
    label(sprintf("Common estimated\nprotocol difference: %.2f µg/mg", results$estimate),
          x + .52, mean(ys), size = 18, just = "left")
  }
  label(if (fitted) "Diamonds: observed donor–protocol means     Lines: fitted means including donor random intercepts"
        else "Small points: aliquots     Diamonds: donor–protocol means     Dashed lines: overall protocol means",
        W / 2, .18, size = 16, col = color_muted)
}
save_figure("eda", W, 5.98, function() draw_means(5.98))
save_figure("random-intercept-means", W, 3.09, function() draw_means(3.09, fitted = TRUE))

draw_diagnostic <- function(qq = FALSE) {
  w <- 5.965; h <- 3.0
  left <- .80; right <- w - .14; bottom <- .65; top <- h - .50
  e <- as.numeric(resid(model))
  if (qq) {
    x <- qnorm(ppoints(length(e))); y <- sort(e)
    xlim <- c(-2.6, 2.6); ylim <- c(-20, 20)
    xticks <- -2:2
  } else {
    x <- as.numeric(fitted(model)); y <- e
    xlim <- c(30, 85); ylim <- c(-20, 20)
    xticks <- seq(30, 80, 10)
  }
  px <- function(z) left + (z - xlim[1]) / diff(xlim) * (right - left)
  py <- function(z) bottom + (z - ylim[1]) / diff(ylim) * (top - bottom)
  for (y0 in c(-20, -10, 0, 10, 20)) {
    line(c(left, right), rep(py(y0), 2)); label(y0, left - .10, py(y0), 16, just = "right")
  }
  for (x0 in xticks) label(x0, px(x0), bottom - .15, 16)
  if (qq) {
    q <- quantile(e, c(.25, .75)); slope <- diff(q) / diff(qnorm(c(.25, .75)))
    intercept <- q[1] - slope * qnorm(.25)
    endpoints <- pmax(xlim[1], pmin(xlim[2], (ylim - intercept) / slope))
    line(px(endpoints), py(intercept + slope * endpoints), color_accent, 1.6)
  } else line(c(left, right), rep(py(0), 2), color_muted, 1.4, 2)
  dots(px(x), py(y), color_primary, pch = 1, size = .07)
  label(if (qq) "Normal Q–Q plot" else "Residuals versus fitted values", w / 2, h - .23, 19, face = "bold")
  label(if (qq) "Theoretical Normal quantiles" else "Conditional fitted yield (µg/mg)",
        (left + right) / 2, .15, 17)
  label("Residual (µg/mg)", .20, (bottom + top) / 2, 17, rot = 90)
}
save_figure("residuals-vs-fitted", 5.965, 3.0, function() draw_diagnostic())
save_figure("normal-qq", 5.965, 3.0, function() draw_diagnostic(TRUE))
cat("Generated Assignment 1 means, REML results and four figures.\n")

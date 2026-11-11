# Known means, not simulated observations: the paired introductory 2 x 2 examples.
args <- commandArgs(trailingOnly = FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
here <- dirname(script)
source(file.path(here, "../../slides-common/R/common_fig.R"))
library(grid)
out <- file.path(here, "assets/additive-example")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

W <- 12.21
H <- 3.65
draw <- function(interaction = FALSE) {
  grid.newpage()
  # Coordinates use the slide's physical inches, so figure type sizes survive embedding.
  label <- function(value, x, y, size = 19, col = color_text, face = "plain", just = "centre") {
    grid.text(value, x = unit(x, "in"), y = unit(y, "in"), just = just,
      gp = gpar(fontfamily = "Liberation Sans", fontsize = size, col = col, fontface = face))
  }
  segment <- function(x0, y0, x1, y1, col = color_grid, width = 1, lty = 1) {
    grid.lines(unit(c(x0, x1), "in"), unit(c(y0, y1), "in"),
      gp = gpar(col = col, lwd = width, lty = lty))
  }
  # Left: the four means, with equally weighted row and column averages.
  tx <- .04
  widths <- c(1.05, 1.55, 1.55, 1.50)
  centers <- tx + cumsum(widths) - widths / 2
  top <- 3.35
  heights <- c(.60, .66, .66, .72)
  rows <- list(
    c("Center", "Standard", "Enhanced", "Row\naverage"),
    list("A", expression(mu[0*A] == 350),
      if (interaction) expression(mu[1*A] == 360) else expression(mu[1*A] == 370),
      if (interaction) "355" else "360"),
    list("B", expression(mu[0*B] == 390),
      if (interaction) expression(mu[1*B] == 420) else expression(mu[1*B] == 410),
      if (interaction) "405" else "400"),
    list("Column\naverage", expression(beta[0] == 370), "390", "380")
  )
  for (r in seq_along(rows)) {
    cy <- top - sum(heights[seq_len(r)]) + heights[r] / 2
    for (k in seq_along(widths)) {
      fill <- if (r == 1) color_primary else if (r == 4 || k == 4) color_panel_cool else color_white
      grid.rect(x = unit(centers[k], "in"), y = unit(cy, "in"),
        width = unit(widths[k], "in"), height = unit(heights[r], "in"),
        gp = gpar(fill = fill, col = color_grid, lwd = .8))
      label(rows[[r]][[k]], centers[k], cy,
        size = if (r == 1 || (r == 4 && k == 1)) 16 else 19,
        col = if (r == 1) color_white else color_text,
        face = if (r == 1 || k == 1 || k == 4) "bold" else "plain")
    }
  }
  label(expression(mu[tj]*": expected outcome for treatment "*t*" in center "*j),
    tx + sum(widths) / 2, .34, size = 16)

  # Right: center on x, treatment in color. Repeated brackets are the focal annotations.
  label("Expected walking distance (m)", 6.52, 3.48, size = 18, just = "left")
  for (k in 1:2) {
    lx <- c(6.65, 9.05)[k]
    col <- c(color_primary, color_accent)[k]
    segment(lx, 3.14, lx + .35, 3.14, col, 2.6)
    label(c("Standard", "Enhanced")[k], lx + .46, 3.14, size = 17, just = "left")
  }
  if (interaction) label(expression("Average treatment effect: "*beta[1]*" = 20 m"),
    9.12, 2.80, size = 17)
  # Reserve a full label width outside each bracket; share coordinates across examples.
  x <- c(8.25, 10.40)
  y <- function(value) .80 + (value - 340) / 80 * 1.75
  for (value in seq(350, 410, 20)) {
    segment(6.70, y(value), 11.86, y(value), color_grid, .7)
    label(as.character(value), 6.55, y(value), size = 16, col = color_muted, just = "right")
  }
  means <- list(c(350, 390), if (interaction) c(360, 420) else c(370, 410))
  for (k in 1:2) {
    col <- c(color_primary, color_accent)[k]
    segment(x[1], y(means[[k]][1]), x[2], y(means[[k]][2]), col, 2.8)
    grid.points(unit(x, "in"), unit(y(means[[k]]), "in"), pch = 21, size = unit(.12, "in"),
      gp = gpar(fill = col, col = col))
  }
  for (j in 1:2) {
    bx <- x[j] + if (j == 1) -.18 else .18
    low <- y(means[[1]][j]); high <- y(means[[2]][j])
    segment(bx, low, bx, high, color_text, 1.5)
    segment(bx - .06, low, bx + .06, low, color_text, 1.5)
    segment(bx - .06, high, bx + .06, high, color_text, 1.5)
    gap_label <- if (!interaction) expression(beta[1] == 20) else if (j == 1)
      expression(beta[1] + d[A] == 10) else expression(beta[1] + d[B] == 30)
    label(gap_label, bx + if (j == 1) -.14 else .14,
      (low + high) / 2, size = 18, just = if (j == 1) "right" else "left")
    label(c("Center A", "Center B")[j], x[j], .45, size = 18)
    if (interaction) label(if (j == 1) expression(d[A]*" = -10 m") else expression(d[B]*" = +10 m"),
      x[j], .13, size = 17)
  }
}
for (interaction in c(FALSE, TRUE)) {
  stem <- if (interaction) "interaction-means" else "additive-means"
  png(file.path(out, paste0(stem, ".png")), width = W, height = H, units = "in", res = 220, type = "cairo", bg = color_bg)
  draw(interaction); invisible(dev.off())
  svg(file.path(out, paste0(stem, ".svg")), width = W, height = H, bg = color_bg)
  draw(interaction); invisible(dev.off())
}
cat("Generated both known-means examples.\n")

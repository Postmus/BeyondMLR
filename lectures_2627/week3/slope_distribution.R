save_slope_distribution <- function(path, average, sd_points, axis_label) {
  ticks <- average + c(-1, 0, 1) * sd_points
  x <- sort(unique(c(seq(average - 3 * sd_points, average + 3 * sd_points, length.out = 301), ticks)))
  curve <- data.frame(x = x, density = dnorm(x, average, sd_points))
  middle <- subset(curve, x >= ticks[1] & x <= ticks[3])
  range_plot <- ggplot(curve, aes(x, density)) +
    geom_area(data = middle, fill = color_secondary_light) +
    geom_line(linewidth = 1.3, colour = color_primary) +
    geom_segment(data = data.frame(x = ticks, density = dnorm(ticks, average, sd_points)),
                 aes(xend = x, yend = 0), colour = color_muted, linetype = "dashed") +
    annotate("label", x = average, y = 0.37 * dnorm(average, average, sd_points), label = "68%", size = 7, fontface = "bold",
             fill = color_secondary_light, colour = color_primary, linewidth = 0) +
    scale_x_continuous(breaks = ticks, labels = sprintf("%.2f", ticks)) +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    labs(x = axis_label, y = NULL) +
    theme_minimal(base_size = 18) +
    theme(text = element_text(colour = color_text), axis.text = element_text(colour = color_text),
          panel.grid = element_blank(), axis.text.y = element_blank(),
          axis.line.x = element_line(colour = color_muted))
  ggsave(path, range_plot, width = 6.3, height = 3.35, dpi = 160, bg = "white")
}

library(ggplot2)
library(lme4)
library(jsonlite)
source("slides-common/R/common_fig.R")

assets <- "lectures_2627/week3/assets"
pain <- read.csv("labs_2627/assignments/week2/downloads/postoperative_pain.csv")
pain$protocol <- factor(pain$protocol, levels = c("Standard", "Enhanced"))
pain$hospital <- factor(pain$hospital)

means <- aggregate(pain_score ~ hospital + protocol, pain, mean)
means$number <- as.integer(sub("Hospital ", "", means$hospital))
by_hospital <- reshape(means[c("number", "protocol", "pain_score")],
                       idvar = "number", timevar = "protocol", direction = "wide")
by_hospital$reduction <- by_hospital$pain_score.Standard - by_hospital$pain_score.Enhanced
examples <- by_hospital[c(which.min(by_hospital$reduction), which.max(by_hospital$reduction)), ]
examples$midpoint <- (examples$pain_score.Standard + examples$pain_score.Enhanced) / 2

plot_theme <- theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = color_grid),
        text = element_text(colour = color_text), axis.text = element_text(colour = color_text),
        plot.background = element_rect(fill = "white", colour = NA))
interaction_plot <- ggplot(means, aes(number, pain_score, colour = protocol, shape = protocol)) +
  geom_line(linewidth = 1.3) + geom_point(size = 3.5) +
  geom_segment(data = examples, inherit.aes = FALSE, colour = color_muted, linetype = "dashed",
               aes(x = number, xend = number, y = pain_score.Enhanced, yend = pain_score.Standard)) +
  geom_label(data = examples, inherit.aes = FALSE,
             aes(x = number + 0.14, y = midpoint, label = sprintf("%.2f", reduction)),
             size = 5.5, hjust = 0, fontface = "bold", colour = color_text, linewidth = 0) +
  scale_colour_manual(values = c(Standard = color_primary, Enhanced = "#B66526")) +
  scale_shape_manual(values = c(Standard = 16, Enhanced = 17)) +
  scale_x_continuous(breaks = by_hospital$number) +
  scale_y_continuous(breaks = 2:6) +
  labs(x = "Hospital", y = "Mean pain score at 24 hours", colour = NULL, shape = NULL) +
  plot_theme + theme(legend.position = "top", legend.margin = margin(0, 0, 0, 0))
ggsave(file.path(assets, "assignment2_interaction.png"), interaction_plot,
       width = 12, height = 4.45, dpi = 160)

varying <- lmer(pain_score ~ protocol + (1 + protocol | hospital), data = pain)
common <- lmer(pain_score ~ protocol + (1 | hospital), data = pain)
effect <- function(model) {
  row <- coef(summary(model))["protocolEnhanced", ]
  list(estimate = unname(row["Estimate"]), se = unname(row["Std. Error"]))
}
slope_sd <- attr(VarCorr(varying)$hospital, "stddev")[["protocolEnhanced"]]
write_json(list(varying = effect(varying), common = effect(common), slope_sd = slope_sd),
           file.path(assets, "assignment2_results.json"), auto_unbox = TRUE, pretty = TRUE, digits = 8)

# Use the rounded teaching values throughout the one-SD illustration.
average <- round(-fixef(varying)[["protocolEnhanced"]], 2)
sd_points <- round(slope_sd, 2)
source("lectures_2627/week3/slope_distribution.R")
save_slope_distribution(file.path(assets, "assignment2_hospital_effects.png"),
                        average, sd_points, "Mean pain reduction (points)")

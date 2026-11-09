library(ggplot2)
source('../../slides-common/R/common_fig.R')

# Conceptual locations and common spread, not fitted trial estimates.
means <- c(Standard = -0.8, Enhanced = 0.8)
curves <- expand.grid(distance = seq(-3.5, 3.5, length.out = 500),
                      treatment = names(means))
curves$density <- dnorm(curves$distance, means[curves$treatment], sd = 0.85)
curves$treatment <- factor(curves$treatment, levels = names(means))
locations <- data.frame(treatment = names(means), location = unname(means))
p <- ggplot(curves, aes(distance, density, color = treatment)) +
  geom_line(linewidth = 1.3) +
  geom_vline(data = locations, aes(xintercept = location, color = treatment),
             linetype = 'dashed', linewidth = .8) +
  scale_color_manual(values = c(Standard = color_primary, Enhanced = color_accent),
                     breaks = names(means)) +
  scale_x_continuous(breaks = unname(means),
                     labels = c(expression(beta[0]), expression(beta[0] + beta[1]))) +
  labs(x = 'Walking distance', y = 'Density', color = 'Treatment') +
  theme_minimal(base_size = 19) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 19), legend.position = 'bottom',
        plot.margin = margin(8, 18, 5, 10))
ggsave('assets/outcome-distributions-wide.png', p, width = 10, height = 3.7,
       dpi = 240, bg = 'white')

library(ggplot2)
source('../../slides-common/R/common_fig.R')

# Population distributions with the same mean (4), variance (4) and SD (2).
# Normal versus Gamma(shape = 4, scale = 1): symmetry versus right skewness.
y <- seq(-2, 12, length.out = 1401)
densities <- rbind(
  data.frame(y, density = dnorm(y, mean = 4, sd = 2), panel = 'Normal'),
  data.frame(y, density = dgamma(y, shape = 4, scale = 1), panel = 'Right-skewed')
)
densities$panel <- factor(densities$panel, levels = c('Normal', 'Right-skewed'))
recap_theme <- theme_minimal(base_size = 17, base_family = 'Liberation Sans') +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = color_grid, linewidth = .3),
        axis.text = element_text(color = color_muted),
        axis.title = element_text(color = color_primary),
        strip.text = element_text(color = color_primary, face = 'bold', size = 18),
        panel.spacing = grid::unit(.35, 'in'),
        plot.margin = margin(8, 12, 6, 6), legend.position = 'none')
p_density <- ggplot(densities, aes(y, density, color = panel, fill = panel)) +
  geom_area(alpha = .12, position = 'identity') +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 4, linetype = 'dashed', color = color_muted, linewidth = .7) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c(color_primary, color_accent)) +
  scale_fill_manual(values = c(color_primary, color_accent)) +
  scale_x_continuous(limits = c(-2, 12), breaks = c(0, 4, 8, 12)) +
  scale_y_continuous(limits = c(0, .24), breaks = c(0, .1, .2), expand = expansion(mult = c(0, .02))) +
  labs(x = 'Y', y = 'Density') + recap_theme
ggsave('assets/recap-distributions.png', p_density, width = 7.2, height = 3.35, dpi = 300, bg = 'white')

# Genuine independent draws; do not force the sample covariance to be zero.
# Population means and variances match in the two examples. Sample summaries
# fluctuate. The second population covariance is rho * SD(Y1) * SD(Y2) = 3.
set.seed(20261109)
n <- 260
z1 <- rnorm(n)
z2 <- rnorm(n)
rho <- .75
pairs <- rbind(
  data.frame(y1 = 4 + 2*z1, y2 = 4 + 2*z2, panel = 'Independent'),
  data.frame(y1 = 4 + 2*z1, y2 = 4 + 2*(rho*z1 + sqrt(1-rho^2)*z2), panel = 'Positive covariance')
)
pairs$panel <- factor(pairs$panel, levels = c('Independent', 'Positive covariance'))
stopifnot(all(pairs$y1 > -3 & pairs$y1 < 11), all(pairs$y2 > -3 & pairs$y2 < 11))
p_pairs <- ggplot(pairs, aes(y1, y2, color = panel)) +
  geom_hline(yintercept = 4, linetype = 'dashed', color = color_muted, linewidth = .7) +
  geom_vline(xintercept = 4, linetype = 'dashed', color = color_muted, linewidth = .7) +
  geom_point(size = 1.65, alpha = .65) +
  facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c(color_primary, color_accent)) +
  scale_x_continuous(limits = c(-3, 11), breaks = c(0, 4, 8)) +
  scale_y_continuous(limits = c(-3, 11), breaks = c(0, 4, 8)) +
  coord_fixed() + labs(x = expression(Y[1]), y = expression(Y[2])) + recap_theme
ggsave('assets/recap-covariance.png', p_pairs, width = 7.2, height = 3.5, dpi = 300, bg = 'white')

# Check the population moments used in the labels, independently of samples.
for (density in list(function(x) dnorm(x, 4, 2), function(x) dgamma(x, 4, scale = 1))) {
  stopifnot(abs(integrate(function(x) x*density(x), -Inf, Inf)$value - 4) < 1e-6,
            abs(integrate(function(x) (x-4)^2*density(x), -Inf, Inf)$value - 4) < 1e-6)
}
cat('Recap figures generated: mean 4, variance 4, SD 2; population covariances 0 and 3.\n')

# Full-width exploratory plot of the existing teaching data.
# Jitter moves points horizontally only; all observed distances are preserved.
source('../../slides-common/R/common_fig.R')
library(ggplot2)
study <- read.csv('assets/rehabilitation_data.csv')
centers <- sort(unique(study$center))
treatments <- c('Standard', 'Enhanced')
study$center <- factor(study$center, levels=centers)
study$treatment <- factor(study$treatment, levels=treatments)
allocation <- table(study$center, study$treatment)
stopifnot(nrow(study) == 120, length(centers) == 6, all(allocation == 10))

means <- aggregate(distance ~ center + treatment, study, mean)
overall <- aggregate(distance ~ treatment, study, mean)
# Balance makes each pooled treatment mean equal to the equally weighted
# average of its six center means, matching the following model interpretation.
center_averages <- aggregate(distance ~ treatment, means, mean)
stopifnot(isTRUE(all.equal(overall$distance, center_averages$distance)))
positions <- function(data) {
  as.integer(data$center) + ifelse(data$treatment == 'Standard', -.16, .16)
}
study$x <- positions(study)
means$x <- positions(means)
palette <- c(Standard=color_primary, Enhanced=color_accent)

figure <- ggplot(study, aes(x=x, y=distance, color=treatment)) +
  geom_hline(data=overall, aes(yintercept=distance, color=treatment),
             linetype='dashed', linewidth=.8, show.legend=FALSE) +
  geom_point(position=position_jitter(width=.085, height=0, seed=20261104),
             size=1.7, alpha=.32) +
  geom_point(data=means, aes(fill=treatment), shape=23, color='white',
             size=4.7, stroke=.8, show.legend=FALSE) +
  scale_color_manual(values=palette, breaks=treatments) +
  scale_fill_manual(values=palette, guide='none') +
  scale_x_continuous(breaks=seq_along(centers),
                     labels=sub('Center ', '', centers), limits=c(.5, 6.5),
                     expand=expansion(mult=0)) +
  scale_y_continuous(expand=expansion(mult=.06)) +
  labs(x='Center', y='Six-minute walk distance (m)', color=NULL) +
  guides(color=guide_legend(override.aes=list(size=3.5, alpha=1))) +
  theme_minimal(base_family='sans', base_size=20) +
  theme(
    plot.background=element_rect(fill='white', color=NA),
    panel.grid.major.x=element_blank(), panel.grid.minor=element_blank(),
    panel.grid.major.y=element_line(color=color_grid, linewidth=.35),
    axis.text=element_text(color=color_text, size=18),
    axis.title=element_text(color=color_primary, size=20),
    axis.title.y=element_text(margin=margin(r=12)),
    axis.title.x=element_text(margin=margin(t=8)),
    legend.position='top', legend.text=element_text(color=color_text, size=19),
    legend.margin=margin(b=4), plot.margin=margin(8, 18, 8, 8)
  )
ggsave('assets/center-summary.png', figure, width=12.05, height=5.25,
       units='in', dpi=300, bg='white')
print(means[c('center', 'treatment', 'distance')], row.names=FALSE)
print(overall, row.names=FALSE)

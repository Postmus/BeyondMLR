library(ggplot2)
source('../../slides-common/R/common_fig.R')

# A schematic joint distribution, not rehabilitation observations.
# Symmetric pairs make both plotted means exactly zero.
u <- c(.35, .65, .9, 1.1, 1.45, 1.65, 1.85, 2.05, .4, 1.4)
v <- c(.7, .3, 1.2, .8, 1.1, 1.85, 1.5, 2.2, -.35, 1.65)
points <- data.frame(u=c(u,-u),v=c(v,-v))
p <- ggplot(points,aes(u,v)) +
  geom_hline(yintercept=0,linetype='dashed',color=color_muted,linewidth=.7) +
  geom_vline(xintercept=0,linetype='dashed',color=color_muted,linewidth=.7) +
  geom_point(color=color_primary,size=3.4,alpha=.85) +
  scale_x_continuous(breaks=0,labels='E[U]',limits=c(-2.6,2.6)) +
  scale_y_continuous(breaks=0,labels='E[V]',limits=c(-2.6,2.6)) +
  labs(x='U',y='V') +
  theme_minimal(base_size=21,base_family='Liberation Sans') +
  theme(panel.grid=element_blank(),axis.text=element_text(color=color_primary,size=20),
    axis.title=element_text(color=color_primary,size=22),
    plot.margin=margin(12,18,6,8))
ggsave('assets/covariance-recap.png',p,width=6.0,height=3.55,dpi=220,bg='white')

# Two schematic collections of repeated error pairs. Orthogonalize and scale
# the draws so both panels have sample means 0 and variances 1 exactly.
# The first has zero covariance; the second has covariance 0.75.
set.seed(411)
z1 <- as.numeric(scale(rnorm(160)))
z2 <- as.numeric(scale(resid(lm(rnorm(160) ~ z1))))
pair_sets <- list(zero=data.frame(u=z1,v=z2),
                  positive=data.frame(u=z1,v=.75*z1+sqrt(1-.75^2)*z2))
stopifnot(abs(cov(pair_sets$zero$u,pair_sets$zero$v)) < 1e-12,
          abs(cov(pair_sets$positive$u,pair_sets$positive$v)-.75) < 1e-12)
for (name in names(pair_sets)) {
  d <- pair_sets[[name]]
  stopifnot(abs(mean(d$u)) < 1e-12,abs(mean(d$v)) < 1e-12,
            abs(var(d$u)-1) < 1e-12,abs(var(d$v)-1) < 1e-12)
  p_pair <- ggplot(d,aes(u,v)) +
    geom_hline(yintercept=0,linetype='dashed',color=color_muted,linewidth=.65) +
    geom_vline(xintercept=0,linetype='dashed',color=color_muted,linewidth=.65) +
    geom_point(color=color_primary,size=2.4,alpha=.7) +
    scale_x_continuous(breaks=c(-2,0,2),limits=c(-3.6,3.6)) +
    scale_y_continuous(breaks=c(-2,0,2),limits=c(-3.6,3.6)) +
    labs(x=expression('Error '*u[1*j]),y=expression('Error '*u[2*j])) +
    theme_minimal(base_size=19,base_family='Liberation Sans') +
    theme(panel.grid=element_blank(),axis.text=element_text(color=color_primary,size=17),
          axis.title=element_text(color=color_primary,size=19),
          plot.margin=margin(5,10,5,5))
  ggsave(paste0('assets/covariance-',name,'.png'),p_pair,width=6.0,height=3.0,dpi=250,bg='white')
}

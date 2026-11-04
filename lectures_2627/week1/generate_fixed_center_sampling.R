library(ggplot2)
library(jsonlite)
source('../../slides-common/R/common_fig.R')

# One clinical setting throughout Part 2: 120 patients, six fixed centers.
# This is lecture asset generation, independent of the deferred live simulator.
# Every figure and calibration check below uses the same full simulated trials.
set.seed(20261103)
n_repetitions <- 10000L
center_effects <- c(A=-17, B=16, C=8, D=-29, E=27, F=-5)
positions <- data.frame(
  center=rep(names(center_effects), each=20L),
  patient=rep(seq_len(20L), 6L),
  treatment=rep(rep(0:1, each=10L), 6L))
positions$center_effect <- unname(center_effects[positions$center])
n <- nrow(positions)
epsilon <- matrix(rnorm(n_repetitions * n, sd=24), nrow=n_repetitions, byrow=TRUE)
errors <- sweep(epsilon, 2, positions$center_effect, '+')
treatment_component <- 360 + 10 * positions$treatment
outcomes <- sweep(errors, 2, treatment_component, '+')

# Evaluate the two analyses on the same trials. In this balanced design their
# treatment coefficients coincide; their residual variances and SEs differ.
standard_mean <- rowMeans(outcomes[, positions$treatment == 0])
enhanced_mean <- rowMeans(outcomes[, positions$treatment == 1])
estimates <- enhanced_mean - standard_mean
sse_pooled <- rowSums(outcomes^2) - 60 * (standard_mean^2 + enhanced_mean^2)
center_means <- sapply(names(center_effects), function(j)
  rowMeans(outcomes[, positions$center == j]))
sse_fixed <- sse_pooled - 20 * rowSums((center_means - rowMeans(outcomes))^2)
se_pooled <- sqrt(sse_pooled / (n - 2) * (1/60 + 1/60))
se_fixed <- sqrt(sse_fixed / (n - 7) * (1/60 + 1/60))

# Validate the closed-form calculations against lm, plus the generating moments.
for (k in seq_len(20L)) {
  d <- transform(positions, outcome=outcomes[k,], center=factor(center))
  pooled <- summary(lm(outcome ~ treatment, d))$coefficients['treatment',]
  fixed <- summary(lm(outcome ~ treatment + center, d))$coefficients['treatment',]
  stopifnot(abs(pooled[1] - estimates[k]) < 1e-9,
            abs(fixed[1] - estimates[k]) < 1e-9,
            abs(pooled[2] - se_pooled[k]) < 1e-8,
            abs(fixed[2] - se_fixed[k]) < 1e-8)
}
stopifnot(sum(center_effects) == 0, n == 120,
          all(table(positions$center, positions$treatment) == 10),
          max(abs(colMeans(errors) - positions$center_effect)) < 1.2,
          max(abs(apply(errors, 2, var) / 24^2 - 1)) < .07,
          max(abs(cov(errors) - cov(epsilon))) < 1e-9,
          abs(mean(estimates) - 10) < .15,
          abs(sd(estimates) - 24 * sqrt(1/60 + 1/60)) < .12,
          abs(mean(se_fixed) - sd(estimates)) < .12,
          mean(se_pooled) > 1.2 * sd(estimates))

# Follow one designated standard-care patient in each of the six centers.
# Do not pool patients from different centers when illustrating independence.
selected <- which(positions$patient == 1)
labels <- sprintf('Center %s: mean %+d m', names(center_effects), center_effects)
samples <- do.call(rbind, lapply(seq_along(selected), function(k) data.frame(
  center=factor(labels[k], levels=labels), error=errors[, selected[k]],
  expected_error=unname(center_effects[k]))))
reference <- unique(samples[,c('center','expected_error')])
p <- ggplot(samples, aes(error)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=6, boundary=0,
                 fill=color_primary, color='white', linewidth=.25, alpha=.8) +
  geom_vline(xintercept=0, color=color_muted, linewidth=.55) +
  geom_vline(data=reference, aes(xintercept=expected_error),
             linetype='dashed', color=color_accent, linewidth=.85) +
  facet_wrap(~center, ncol=3) +
  scale_x_continuous(breaks=c(-80,-40,0,40,80)) +
  scale_y_continuous(breaks=c(0,.01,.02), expand=expansion(mult=c(0,.02))) +
  coord_cartesian(xlim=c(-110,110), ylim=c(0,.020)) +
  labs(x='Treatment-only error (m)', y='Density') +
  theme_minimal(base_size=20) + theme(
    panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
    axis.text=element_text(size=18,color=color_text),
    axis.title=element_text(size=20,color=color_primary),
    strip.text=element_text(size=20,face='bold',color=color_primary),
    panel.spacing=grid::unit(.6,'lines'), plot.margin=margin(8,12,8,8))
ggsave('assets/fixed-center-errors-10000.png',p,
       width=12.05,height=4.3,dpi=220,bg='white')

# Two specific positions in A (one per treatment), then A versus B.
first <- 1L
paired <- c(11L,21L)
error_pair_plot <- function(second, title, y_label) {
  ggplot(data.frame(first=errors[,first], second=errors[,second]), aes(first,second)) +
    geom_point(color=color_primary,size=.7,alpha=.12) +
    geom_vline(xintercept=positions$center_effect[first],
               linetype='dashed',color=color_accent,linewidth=.7) +
    geom_hline(yintercept=positions$center_effect[second],
               linetype='dashed',color=color_accent,linewidth=.7) +
    scale_x_continuous(breaks=c(-80,-40,0,40,80)) +
    scale_y_continuous(breaks=c(-80,-40,0,40,80)) +
    coord_fixed(xlim=c(-110,110),ylim=c(-110,110)) +
    labs(title=title, subtitle='Model covariance: 0',
         x='Center A / standard: error (m)', y=y_label) +
    theme_minimal(base_size=20) + theme(
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(color=color_grid,linewidth=.25),
      plot.title=element_text(size=22,face='bold',color=color_primary),
      plot.subtitle=element_text(size=19,color=color_muted),
      axis.text=element_text(size=18,color=color_text),
      axis.title=element_text(size=20,color=color_primary),
      plot.margin=margin(8,12,8,8))
}
p_same <- error_pair_plot(paired[1], 'Same center: A and A', 'Center A / enhanced: error (m)')
p_different <- error_pair_plot(paired[2], 'Different centers: A and B', 'Center B / standard: error (m)')
png('assets/fixed-center-error-pairs.png',width=12.05,height=4.3,units='in',res=220,bg='white')
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(1,2)))
print(p_same,vp=grid::viewport(layout.pos.row=1,layout.pos.col=1))
print(p_different,vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()
stopifnot(all(abs(cor(errors[,c(first,paired)])[1,-1]) < .05))

result <- list(
  seed=20261103, repetitions=n_repetitions, total=n, centers=6,
  patients_per_center=20, patients_per_arm_per_center=10,
  baseline=360, treatment_effect=10, individual_error_sd=24,
  center_effects=as.list(center_effects),
  error_definition='Known generating error u = c + epsilon, not fitted residuals.',
  histogram_positions=as.list(selected), pair_positions=list(first=first, second=as.list(paired)),
  positions=lapply(seq_len(n),function(k) list(
    center=positions$center[k], patient=positions$patient[k], treatment=positions$treatment[k],
    expected_error=positions$center_effect[k], empirical_mean=mean(errors[,k]),
    empirical_variance=var(errors[,k]))),
  theoretical_pair_covariance=0,
  empirical_pair_correlations=unname(cor(errors[,c(first,paired)])[1,-1]),
  treatment=list(mean_estimate=mean(estimates), bias=mean(estimates)-10,
    empirical_se=sd(estimates), theoretical_se=24*sqrt(1/60+1/60),
    mean_estimated_se_pooled=mean(se_pooled), mean_estimated_se_fixed=mean(se_fixed)))
write_json(result,'assets/fixed-center-sampling.json',auto_unbox=TRUE,pretty=TRUE,digits=8)
cat('Generated 10,000 full 120-patient trials; both figures and SE checks use the same draws.\n')
print(result$treatment)

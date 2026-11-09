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

# The first two patient positions in A and B, followed through the same trials.
# These are generating errors, not fitted residuals or pooled center samples.
selected <- which(positions$center %in% c('A', 'B') & positions$patient <= 2L)
stopifnot(identical(selected, c(1L, 2L, 21L, 22L)))
labels <- sprintf('Center %s / patient %d: mean %+d m',
                  positions$center[selected], positions$patient[selected],
                  positions$center_effect[selected])
samples <- do.call(rbind, lapply(seq_along(selected), function(k) data.frame(
  panel=factor(labels[k], levels=labels), error=errors[, selected[k]],
  expected_error=positions$center_effect[selected[k]])))
reference <- unique(samples[,c('panel','expected_error')])
p <- ggplot(samples, aes(error)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=6, boundary=0,
                 fill=color_primary, color='white', linewidth=.25, alpha=.8) +
  geom_vline(xintercept=0, color=color_muted, linewidth=.45) +
  geom_vline(data=reference, aes(xintercept=expected_error),
             linetype='dashed', color=color_accent, linewidth=.85) +
  facet_wrap(~panel, ncol=2) +
  scale_x_continuous(breaks=c(-80,-40,0,40,80)) +
  scale_y_continuous(breaks=c(0,.01,.02), expand=expansion(mult=c(0,.02))) +
  coord_cartesian(xlim=c(-110,110), ylim=c(0,.020)) +
  labs(x='Treatment-only error (m)', y='Density') +
  theme_minimal(base_size=18) + theme(
    panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
    axis.text=element_text(size=16,color=color_text),
    axis.title=element_text(size=18,color=color_primary),
    strip.text=element_text(size=18,face='bold',color=color_primary),
    panel.spacing=grid::unit(.7,'lines'), plot.margin=margin(8,12,8,8))
ggsave('assets/fixed-center-errors-10000.png',p,
       width=12.05,height=4.18,dpi=240,bg='white')

# Top row: two within-center pairs. Bottom row: two between-center pairs.
# Preserve simulation-row alignment in every pair; no sample decorrelation.
pair_first <- c(1L,21L,1L,2L)
pair_second <- c(2L,22L,21L,22L)
pair_titles <- c('Same center: A1 / A2', 'Same center: B1 / B2',
                 'Different centers: A1 / B1', 'Different centers: A2 / B2')
patient_label <- function(k) paste0(positions$center[k],positions$patient[k])
error_pair_plot <- function(k) {
  first <- pair_first[k]; second <- pair_second[k]
  ggplot(data.frame(first=errors[,first], second=errors[,second]), aes(first,second)) +
    geom_point(color=color_primary,size=.45,alpha=.10) +
    geom_vline(xintercept=positions$center_effect[first],
               linetype='dashed',color=color_accent,linewidth=.65) +
    geom_hline(yintercept=positions$center_effect[second],
               linetype='dashed',color=color_accent,linewidth=.65) +
    scale_x_continuous(breaks=c(-80,0,80)) +
    scale_y_continuous(breaks=c(-80,0,80)) +
    coord_fixed(xlim=c(-110,110),ylim=c(-110,110)) +
    labs(title=pair_titles[k], x=paste(patient_label(first),'error (m)'),
         y=paste(patient_label(second),'error (m)')) +
    theme_minimal(base_size=16) + theme(
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(color=color_grid,linewidth=.25),
      plot.title=element_text(size=16,face='bold',color=color_primary,hjust=.5),
      axis.text=element_text(size=14,color=color_text),
      axis.title=element_text(size=16,color=color_primary),
      plot.margin=margin(5,10,5,10))
}
png('assets/fixed-center-error-pairs.png',width=9.3,height=4.52,units='in',res=240,bg='white')
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,2)))
for (k in 1:4) print(error_pair_plot(k),vp=grid::viewport(
  layout.pos.row=(k-1L)%/%2L+1L,layout.pos.col=(k-1L)%%2L+1L))
dev.off()
pair_correlations <- mapply(function(i,j) cor(errors[,i],errors[,j]),pair_first,pair_second)
pair_covariances <- mapply(function(i,j) cov(errors[,i],errors[,j]),pair_first,pair_second)
stopifnot(nrow(samples) == 4L*n_repetitions,
          all(abs(pair_correlations) < .05),
          all(abs(colMeans(errors[,selected])-positions$center_effect[selected]) < 1.2),
          all(abs(apply(errors[,selected],2,sd)-24) < .6))

# Pool all 120 patient positions across the same 10,000 studies. Each center
# contributes 20 positions, so its Normal density has mixture weight 1/6.
pooled_errors <- as.vector(errors)
weights <- as.numeric(table(positions$center)) / n
names(weights) <- names(center_effects)
curve_x <- seq(-150, 150, length.out=1501)
components <- do.call(rbind, lapply(names(center_effects), function(j) data.frame(
  error=curve_x, density=weights[j]*dnorm(curve_x, center_effects[j], 24), center=j)))
mixture_density <- function(x) vapply(x, function(t)
  sum(weights*dnorm(t, center_effects, 24)), numeric(1))
mixture <- data.frame(error=curve_x, density=mixture_density(curve_x))
mixture_mean <- sum(weights*center_effects)
component_areas <- vapply(names(center_effects), function(j)
  integrate(function(x) weights[j]*dnorm(x, center_effects[j],24), -Inf,Inf)$value, numeric(1))
stopifnot(length(pooled_errors)==1200000L,
          all(abs(weights-1/6)<1e-12), abs(mixture_mean)<1e-12,
          max(abs(component_areas-1/6))<1e-8,
          abs(integrate(mixture_density,-Inf,Inf)$value-1)<1e-8,
          abs(integrate(function(x) x*mixture_density(x),-Inf,Inf)$value)<1e-8,
          abs(mean(pooled_errors)-mixture_mean)<.15)
curve_colors <- c(Mixture=color_primary, A='#8FB2C4', B='#C8A3C4',
                  C='#A8BD8B', D='#E1B07B', E='#B0B1D4', F='#80BEB5')
p_pool <- ggplot(data.frame(error=pooled_errors), aes(error)) +
  geom_histogram(aes(y=after_stat(density)), binwidth=4, boundary=0,
                 fill='#B8C2BF', alpha=.65, color='white', linewidth=.2) +
  geom_line(data=components, aes(error,density,color=center,group=center),
            inherit.aes=FALSE, linewidth=.75) +
  geom_line(data=mixture, aes(error,density,color='Mixture'),
            inherit.aes=FALSE, linewidth=1.35) +
  geom_vline(xintercept=0, linetype='dashed',color=color_muted,linewidth=.7) +
  scale_color_manual(values=curve_colors, breaks=names(curve_colors),
                     labels=c('Mixture', paste('Center',names(center_effects))), name=NULL) +
  guides(color=guide_legend(nrow=1)) +
  scale_x_continuous(breaks=seq(-120,120,40)) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  coord_cartesian(xlim=c(-140,140)) +
  labs(x='Treatment-only error (m)',y='Density',
       subtitle='Histogram: 1,200,000 simulated errors; thin curves: center contributions, each weighted 1/6') +
  theme_minimal(base_size=18) + theme(
    panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    plot.subtitle=element_text(size=17,color=color_muted,margin=margin(b=8)),
    axis.text=element_text(size=16,color=color_text),
    axis.title=element_text(size=18,color=color_primary),
    legend.position='bottom',legend.text=element_text(size=16),
    legend.key.width=grid::unit(.4,'in'),plot.margin=margin(8,12,5,8))
ggsave('assets/fixed-center-pooled-errors.png',p_pool,
       width=12.05,height=4.19,dpi=240,bg='white')

result <- list(
  seed=20261103, repetitions=n_repetitions, total=n, centers=6,
  patients_per_center=20, patients_per_arm_per_center=10,
  baseline=360, treatment_effect=10, individual_error_sd=24,
  center_effects=as.list(center_effects),
  error_definition='Known generating error u = c + epsilon, not fitted residuals.',
  histogram_positions=as.list(selected), pair_positions=lapply(1:4, function(k) list(first=pair_first[k],second=pair_second[k],label=pair_titles[k])),
  positions=lapply(seq_len(n),function(k) list(
    center=positions$center[k], patient=positions$patient[k], treatment=positions$treatment[k],
    expected_error=positions$center_effect[k], empirical_mean=mean(errors[,k]),
    empirical_variance=var(errors[,k]))),
  pooled_error_distribution=list(observations=length(pooled_errors),
    center_weights=as.list(weights),component_areas=as.list(component_areas),
    theoretical_mean=mixture_mean,empirical_mean=mean(pooled_errors)),
  theoretical_pair_covariance=0,
  empirical_pair_correlations=unname(pair_correlations),
  empirical_pair_covariances=unname(pair_covariances),
  treatment=list(mean_estimate=mean(estimates), bias=mean(estimates)-10,
    empirical_se=sd(estimates), theoretical_se=24*sqrt(1/60+1/60),
    mean_estimated_se_pooled=mean(se_pooled), mean_estimated_se_fixed=mean(se_fixed)))
write_json(result,'assets/fixed-center-sampling.json',auto_unbox=TRUE,pretty=TRUE,digits=8)
cat('Generated 10,000 full 120-patient trials; all three error figures and SE checks use the same draws.\n')
print(result$treatment)

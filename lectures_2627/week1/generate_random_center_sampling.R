library(ggplot2)
library(jsonlite)
source('../../slides-common/R/common_fig.R')

# Teaching experiment: draw new center effects and patients in each repetition.
# The known treatment-only component is used to compute errors in every draw.
set.seed(20261104)
n_repetitions <- 10000L
positions <- data.frame(
  position=c('Center 1 / standard', 'Center 1 / enhanced',
             'Center 2 / standard', 'Center 2 / enhanced'),
  center=c(1L, 1L, 2L, 2L), treatment=c(0L, 1L, 0L, 1L),
  center_effect=c(0, 0, 0, 0))
center_draws <- matrix(rnorm(n_repetitions * 2L, mean=0, sd=10), ncol=2)
shared <- center_draws[,positions$center]
epsilon <- matrix(rnorm(n_repetitions * 4L, mean=0, sd=5), ncol=4)
errors <- shared + epsilon
treatment_component <- 100 + 5 * positions$treatment
outcomes <- sweep(errors, 2, treatment_component, '+')
stopifnot(max(abs(outcomes - sweep(errors, 2, treatment_component, '+'))) < 1e-10)
# Validate marginal moments, shared-center covariance and balanced contrasts.
stopifnot(all(abs(colMeans(errors)) < .4))
stopifnot(all(abs(apply(errors, 2, var) - 125) < 6))
expected_cov <- 100 * outer(positions$center, positions$center, '==') + diag(25,4)
stopifnot(max(abs(cov(errors) - expected_cov)) < 6)
stopifnot(identical(shared[,1], shared[,2]), identical(shared[,3], shared[,4]))
contrast <- (outcomes[,2] + outcomes[,4] - outcomes[,1] - outcomes[,3]) / 2
noise_contrast <- (epsilon[,2] + epsilon[,4] - epsilon[,1] - epsilon[,3]) / 2
stopifnot(max(abs(contrast - 5 - noise_contrast)) < 1e-10)
# Treatment-only OLS has two observations per group and two residual df.
residuals <- outcomes
residuals[,c(1,3)] <- outcomes[,c(1,3)] - rowMeans(outcomes[,c(1,3)])
residuals[,c(2,4)] <- outcomes[,c(2,4)] - rowMeans(outcomes[,c(2,4)])
reported_se <- sqrt(rowSums(residuals^2) / 2)
stopifnot(abs(mean(contrast) - 5) < .2, abs(var(contrast) - 25) < 1)
stopifnot(abs(mean(reported_se^2) - 125) < 6)
# Cross-check the vectorized formulas against actual OLS fits.
for (i in 1:10) {
  fit <- lm(outcomes[i,] ~ positions$treatment)
  stopifnot(abs(coef(fit)[2] - contrast[i]) < 1e-10)
  stopifnot(abs(coef(summary(fit))[2,2] - reported_se[i]) < 1e-10)
}

samples <- do.call(rbind, lapply(seq_len(4), function(k) data.frame(
  repetition=seq_len(n_repetitions), position=positions$position[k],
  center=factor(positions$center[k], levels=1:2,
                labels=c('Center 1', 'Center 2')),
  treatment=factor(positions$treatment[k], levels=0:1,
                   labels=c('Standard care', 'Enhanced care')),
  error=errors[,k], expected_error=positions$center_effect[k])))

stages <- c(20L, n_repetitions)
breaks <- seq(-100,100,by=4)
max_density <- max(vapply(stages, function(n) max(vapply(1:4, function(k)
  max(hist(errors[seq_len(n),k],breaks=breaks,plot=FALSE)$density), numeric(1))), numeric(1)))
y_top <- ceiling((max_density + .005) / .02) * .02

for (n in stages) {
  current <- samples[samples$repetition <= n,]
  reference <- unique(current[,c('center','treatment','expected_error')])
  p <- ggplot(current,aes(error,fill=center)) +
    geom_histogram(aes(y=after_stat(density)),breaks=breaks,
                   color='white',linewidth=.25,alpha=.78) +
    geom_vline(data=reference,aes(xintercept=expected_error),
               linetype='dashed',color=color_text,linewidth=.8) +
    facet_grid(treatment~center) +
    scale_fill_manual(values=c(color_primary,color_accent),guide='none') +
    scale_x_continuous(breaks=c(-40,-20,0,20,40)) +
    scale_y_continuous(breaks=c(0,.02,.04,.06),expand=expansion(mult=c(0,.02))) +
    coord_cartesian(xlim=c(-45,45),ylim=c(0,y_top)) +
    labs(x='Error relative to the treatment-only component',y='Density') +
    theme_minimal(base_size=18) + theme(
      panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
      axis.text=element_text(size=16,color=color_text),
      axis.title=element_text(size=18,color=color_primary),
      strip.text=element_text(size=18,face='bold',color=color_primary),
      strip.text.y=element_text(angle=0),
      panel.spacing=grid::unit(.65,'lines'),
      plot.margin=margin(8,12,8,8))
  ggsave(sprintf('assets/random-center-errors-%d.png',n),p,
         width=12.05,height=5.05,dpi=220,bg='white')
}

# i indexes patients within center j: columns 1, 2 and 3 are u11, u21 and u12.
# Pair errors from the same repetition, retaining the original simulated draws.
error_pair_plot <- function(second, title, y_label) {
  ggplot(data.frame(first=errors[,1],second=errors[,second]),aes(first,second)) +
    geom_point(color=color_primary,size=.7,alpha=.12) +
    geom_vline(xintercept=0,linetype='dashed',color=color_accent,linewidth=.7) +
    geom_hline(yintercept=positions$center_effect[second],linetype='dashed',
               color=color_accent,linewidth=.7) +
    scale_x_continuous(breaks=c(-40,-20,0,20,40)) +
    scale_y_continuous(breaks=c(-40,-20,0,20,40)) +
    coord_fixed(xlim=c(-45,45),ylim=c(-45,45)) +
    labs(title=title,subtitle=if(second==2) 'Model covariance: 100' else 'Model covariance: 0',
         x=expression('Error '*u[11]),y=y_label) +
    theme_minimal(base_size=18) + theme(
      panel.grid.minor=element_blank(),panel.grid.major=element_line(color=color_grid,linewidth=.25),
      plot.title=element_text(size=20,face='bold',color=color_primary),
      plot.subtitle=element_text(size=17,color=color_muted),
      axis.text=element_text(size=16,color=color_text),
      axis.title=element_text(size=18,color=color_primary),
      plot.margin=margin(8,12,8,8))
}
p_same <- error_pair_plot(2,'Same center',expression('Error '*u[21]))
p_different <- error_pair_plot(3,'Different centers',expression('Error '*u[12]))
png('assets/random-center-error-pairs.png',width=12.05,height=5.05,units='in',res=220,bg='white')
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(1,2)))
print(p_same,vp=grid::viewport(layout.pos.row=1,layout.pos.col=1))
print(p_different,vp=grid::viewport(layout.pos.row=1,layout.pos.col=2))
dev.off()

result <- list(
  seed=20261104, repetitions=n_repetitions, baseline=100, treatment_effect=5,
  individual_error_sd=5, center_effect_sd=10, stages=stages,
  positions=lapply(1:4,function(k) list(
    center=positions$center[k], treatment=if(positions$treatment[k]==0) 'Standard' else 'Enhanced',
    center_effect=shared[1,k], individual_error=epsilon[1,k],
    treatment_component=treatment_component[k],
    first_error=errors[1,k],first_outcome=outcomes[1,k],
    expected_error=positions$center_effect[k], empirical_mean=mean(errors[,k]),
    empirical_variance=var(errors[,k]))),
  pooled_expected_error=mean(positions$center_effect),
  theoretical_within_center_covariance=100, theoretical_between_center_covariance=0,
  theoretical_error_variance=125, theoretical_treatment_variance=25,
  empirical_treatment_mean=mean(contrast), empirical_treatment_sd=sd(contrast),
  mean_reported_se=mean(reported_se), mean_reported_se_squared=mean(reported_se^2), empirical_covariances=unname(cov(errors)),
  empirical_correlations=unname(cor(errors)), plot_y_max=y_top)
write_json(result,'assets/random-center-sampling.json',auto_unbox=TRUE,pretty=TRUE,digits=8)
cat('Generated random-center four-position simulation: 20 and 10000 repetitions, plus paired-error scatterplots.\n')
print(data.frame(positions,empirical_mean=colMeans(errors),empirical_variance=apply(errors,2,var)))

cat('Treatment mean:',mean(contrast),'SD:',sd(contrast),'Mean estimated SE:',mean(reported_se),'Mean SE squared:',mean(reported_se^2),'\n')

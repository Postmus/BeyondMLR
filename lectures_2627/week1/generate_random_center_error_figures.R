library(ggplot2)
library(jsonlite)
source('../../slides-common/R/common_fig.R')

# Match the six-center, 120-patient design in Part 2. Redraw all six center
# effects in each study; within a study, each center shares its single draw.
set.seed(20261109)
n_repetitions <- 10000L
center_sd <- 20
individual_sd <- 24
positions <- data.frame(center=rep(LETTERS[1:6], each=20L),
                        patient=rep(seq_len(20L), 6L),
                        treatment=rep(rep(0:1, each=10L), 6L))
center_draws <- matrix(rnorm(n_repetitions*6L, sd=center_sd), ncol=6L)
shared <- center_draws[,match(positions$center, LETTERS[1:6])]
epsilon <- matrix(rnorm(n_repetitions*nrow(positions), sd=individual_sd),
                  nrow=n_repetitions)
errors <- shared + epsilon
selected <- c(1L,2L,21L,22L)
expected_variance <- center_sd^2 + individual_sd^2
pair_first <- c(1L,21L,1L,2L)
pair_second <- c(2L,22L,21L,22L)
pair_titles <- c('Same center: A1 / A2', 'Same center: B1 / B2',
                 'Different centers: A1 / B1', 'Different centers: A2 / B2')
pair_covariances <- mapply(function(i,j) cov(errors[,i],errors[,j]),pair_first,pair_second)
pair_correlations <- mapply(function(i,j) cor(errors[,i],errors[,j]),pair_first,pair_second)
expected_covariances <- c(center_sd^2,center_sd^2,0,0)
expected_correlations <- expected_covariances / expected_variance

# Validate genuine independent draws, without centering or decorrelating samples.
stopifnot(identical(shared[,1],shared[,2]), identical(shared[,21],shared[,22]),
          !identical(shared[,1],shared[,21]),
          all(table(positions$center,positions$treatment)==10),
          max(abs(colMeans(errors))) < 5*sqrt(expected_variance/n_repetitions),
          max(abs(apply(errors,2,var)/expected_variance - 1)) < .06,
          max(abs(pair_covariances-expected_covariances)) < 40,
          max(abs(pair_correlations-expected_correlations)) < .04)

labels <- sprintf('Center %s / patient %d: mean 0 m',
                  positions$center[selected],positions$patient[selected])
samples <- do.call(rbind,lapply(seq_along(selected),function(k) data.frame(
  panel=factor(labels[k],levels=labels),error=errors[,selected[k]])))
p <- ggplot(samples,aes(error)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=6,boundary=0,
                 fill=color_primary,color='white',linewidth=.25,alpha=.8) +
  geom_vline(xintercept=0,linetype='dashed',color=color_accent,linewidth=.85) +
  facet_wrap(~panel,ncol=2) +
  scale_x_continuous(breaks=c(-120,-60,0,60,120)) +
  scale_y_continuous(breaks=c(0,.005,.010,.015),expand=expansion(mult=c(0,.02))) +
  coord_cartesian(xlim=c(-140,140),ylim=c(0,.015)) +
  labs(x='Combined error (m)',y='Density',
       subtitle='10,000 studies; center-effect SD = 20 m; individual-error SD = 24 m') +
  theme_minimal(base_size=18) + theme(
    panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    plot.subtitle=element_text(size=17,color=color_muted,margin=margin(b=8)),
    axis.text=element_text(size=16,color=color_text),
    axis.title=element_text(size=18,color=color_primary),
    strip.text=element_text(size=18,face='bold',color=color_primary),
    panel.spacing=grid::unit(.7,'lines'),plot.margin=margin(8,12,8,8))
ggsave('assets/random-center-combined-errors.png',p,
       width=12.05,height=4.52,dpi=240,bg='white')

# Identical patient pairings to slide 21; pair values from the same study row.
patient_label <- function(k) paste0(positions$center[k],positions$patient[k])
pair_plot <- function(k) {
  first <- pair_first[k]; second <- pair_second[k]
  ggplot(data.frame(first=errors[,first],second=errors[,second]),aes(first,second)) +
    geom_point(color=color_primary,size=.45,alpha=.10) +
    geom_vline(xintercept=0,linetype='dashed',color=color_accent,linewidth=.65) +
    geom_hline(yintercept=0,linetype='dashed',color=color_accent,linewidth=.65) +
    scale_x_continuous(breaks=c(-120,0,120)) +
    scale_y_continuous(breaks=c(-120,0,120)) +
    coord_fixed(xlim=c(-140,140),ylim=c(-140,140)) +
    labs(title=pair_titles[k],x=paste(patient_label(first),'error (m)'),
         y=paste(patient_label(second),'error (m)')) +
    theme_minimal(base_size=16) + theme(
      panel.grid.minor=element_blank(),
      panel.grid.major=element_line(color=color_grid,linewidth=.25),
      plot.title=element_text(size=16,face='bold',color=color_primary,hjust=.5),
      axis.text=element_text(size=14,color=color_text),
      axis.title=element_text(size=16,color=color_primary),
      plot.margin=margin(5,10,5,10))
}
png('assets/random-center-combined-error-pairs.png',width=9.3,height=4.52,units='in',res=240,bg='white')
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,2)))
for (k in 1:4) print(pair_plot(k),vp=grid::viewport(
  layout.pos.row=(k-1L)%/%2L+1L,layout.pos.col=(k-1L)%%2L+1L))
dev.off()

result <- list(seed=20261109,repetitions=n_repetitions,centers=6,patients_per_center=20,
  center_effect_sd=center_sd,individual_error_sd=individual_sd,
  sampling='Draw new independent center effects and individual errors in every study; share each center effect within that study.',
  error_definition='Known generating combined error c + epsilon, not a fitted residual.',
  theoretical_mean=0,theoretical_variance=expected_variance,
  positions=lapply(selected,function(k) list(center=positions$center[k],
    patient=positions$patient[k],empirical_mean=mean(errors[,k]),empirical_variance=var(errors[,k]))),
  pairs=lapply(1:4,function(k) list(label=pair_titles[k],first=pair_first[k],second=pair_second[k],
    theoretical_covariance=expected_covariances[k],empirical_covariance=pair_covariances[k],
    theoretical_correlation=expected_correlations[k],empirical_correlation=pair_correlations[k])))
write_json(result,'assets/random-center-combined-errors.json',auto_unbox=TRUE,pretty=TRUE,digits=8)
cat('Generated both random-center error figures from the same 10,000 studies; moment and covariance checks passed.\n')
print(data.frame(pair=pair_titles,covariance=pair_covariances,correlation=pair_correlations))

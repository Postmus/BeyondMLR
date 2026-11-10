library(ggplot2)
library(nlme)
library(jsonlite)
source('../../slides-common/R/common_fig.R')
dir.create('assets',showWarnings=FALSE)

# Continue the Week 1 clinical case under a different randomization scheme.
# Outcomes are generated under each center's new assignment. The patient
# deviations, center contributions and common 24 m effect are retained.
blocked <- read.csv('../week1/assets/rehabilitation_data.csv')
centers <- paste('Center',LETTERS[1:6])
center_effects <- c(-29,-17,-5,8,16,27)
blocked$center <- factor(blocked$center,levels=centers)
patient_deviation <- blocked$distance -
  (360 + center_effects[as.integer(blocked$center)] + 24*(blocked$treatment=='Enhanced'))
set.seed(729)
allocation <- sample(rep(c('Standard','Enhanced'),each=length(centers)/2))
cluster_dat <- blocked
cluster_dat$treatment <- factor(allocation[as.integer(blocked$center)],
                                levels=c('Standard','Enhanced'))
cluster_dat$distance <- round(360 + center_effects[as.integer(blocked$center)] +
  24*(cluster_dat$treatment=='Enhanced') + patient_deviation,1)
write.csv(cluster_dat,'assets/rehabilitation_cluster_data.csv',row.names=FALSE)

ordinary <- lm(distance~treatment,data=cluster_dat)
mixed <- lme(distance~treatment,random=~1|center,data=cluster_dat,method='REML')
ci_ordinary <- unname(confint(ordinary)['treatmentEnhanced',])
ci_mixed <- unname(intervals(mixed,which='fixed')$fixed['treatmentEnhanced',c('lower','upper')])
test <- summary(mixed)$tTable['treatmentEnhanced',]
vc <- VarCorr(mixed)
statistics <- list(centers=length(centers),
  cluster=list(centers_per_arm=length(centers)/2,
    patients_per_center=nrow(cluster_dat)/length(centers),total=nrow(cluster_dat),
    allocation=data.frame(center=centers,treatment=allocation),
    ordinary=list(estimate=unname(coef(ordinary)['treatmentEnhanced']),
      se=unname(summary(ordinary)$coefficients['treatmentEnhanced','Std. Error']),
      lower=ci_ordinary[1],upper=ci_ordinary[2],residual_sd=unname(sigma(ordinary))),
    random=list(estimate=unname(fixef(mixed)['treatmentEnhanced']),
      se=unname(test['Std.Error']),df=unname(test['DF']),
      t=unname(test['t-value']),p=unname(test['p-value']),
      lower=ci_mixed[1],upper=ci_mixed[2],
      center_var=as.numeric(vc[1,'Variance']),patient_var=as.numeric(vc[2,'Variance']))))
write_json(statistics,'assets/model_results.json',auto_unbox=TRUE,digits=10,pretty=TRUE)

p <- ggplot(cluster_dat,aes(center,distance,color=treatment)) +
  geom_point(position=position_jitter(width=.13,height=0,seed=19),alpha=.42,size=1.8) +
  stat_summary(fun=mean,geom='point',shape=18,size=4.8) +
  scale_color_manual(values=c(Standard=color_primary,Enhanced=color_accent),drop=FALSE) +
  labs(x=NULL,y='6-minute walk distance (m)',color='Assigned program') +
  theme_minimal(base_size=16) +
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
    legend.position='bottom',axis.text.x=element_text(size=12),
    plot.margin=margin(10,16,10,10))
ggsave('assets/cluster-centers.png',p,width=9.2,height=4.5,dpi=190,bg='white')

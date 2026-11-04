library(ggplot2)
library(nlme)
source('../../slides-common/R/common_fig.R')
set.seed(158)
centers <- paste('Center', LETTERS[1:6])
# Use the established center-label permutation for the revised teaching trial.
# Original A–F cohorts are now D, A, F, C, B, E; display levels remain alphabetical.
generation_centers <- centers[c(4, 1, 6, 3, 2, 5)]
center_effects <- setNames(c(-29, -17, -5, 8, 16, 27), generation_centers)
patients_per_arm <- 10
treatment_effect <- 10
dat <- do.call(rbind, lapply(seq_along(centers), function(j) {
  treatment <- sample(rep(c('Standard','Enhanced'), each=patients_per_arm))
  data.frame(center=generation_centers[j], treatment=treatment,
    distance=round(360 + center_effects[[generation_centers[j]]] + treatment_effect*(treatment=='Enhanced') + rnorm(2*patients_per_arm,0,24), 1))
}))
dat$center <- factor(dat$center, levels=centers)
dat$treatment <- factor(dat$treatment, levels=c('Standard','Enhanced'))
write.csv(dat,'assets/rehabilitation_data.csv',row.names=FALSE)
# Retain Center D for the auxiliary single-center teaching figures.
single <- subset(dat, center==generation_centers[1])
base <- theme_minimal(base_size=16)+theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(face='bold'),legend.position='none',plot.margin=margin(10,16,10,10))
cols <- c(Standard=color_primary,Enhanced=color_accent)
p1 <- ggplot(single,aes(treatment,distance,fill=treatment))+geom_boxplot(width=.48,alpha=.5,outlier.shape=NA)+geom_jitter(aes(color=treatment),width=.14,height=0,size=2.1,alpha=.65)+scale_fill_manual(values=cols)+scale_color_manual(values=cols)+labs(x=NULL,y='6-minute walk distance (m)')+base
ggsave('assets/single-center.png',p1,width=7.7,height=4.5,dpi=170,bg='white')
means <- aggregate(distance~center+treatment,dat,mean)
p2 <- ggplot(dat,aes(center,distance,color=treatment))+geom_point(position=position_jitterdodge(jitter.width=.2,dodge.width=.55,seed=9),alpha=.44,size=1.8)+stat_summary(aes(group=treatment),fun=mean,geom='point',shape=18,size=4.5,position=position_dodge(width=.55))+scale_color_manual(values=cols)+labs(x=NULL,y='6-minute walk distance (m)',color='Treatment')+base+theme(legend.position='bottom',axis.text.x=element_text(size=12))
ggsave('assets/centers.png',p2,width=9.2,height=4.5,dpi=170,bg='white')
p3 <- ggplot(means,aes(distance,center,group=center))+geom_line(color='#A5B7B7',linewidth=1)+geom_point(aes(color=treatment),size=4)+scale_color_manual(values=cols)+labs(y=NULL,x='Mean walk distance (m)',color='Treatment')+base+theme(legend.position='bottom')
ggsave('assets/center-means.png',p3,width=9.2,height=4.5,dpi=170,bg='white')
m0<-lm(distance~treatment,data=dat);m1<-lm(distance~treatment+center,data=dat)
m2<-lme(distance~treatment,random=~1|center,data=dat,method='REML')
vc<-VarCorr(m2); singlemod<-lm(distance~treatment,data=single)
png('assets/diagnostics.png',width=1600,height=700,res=180,bg='white')
par(mfrow=c(1,2),mar=c(4.2,4.3,3.0,1),cex=1.1,col.axis='#36565A',col.lab='#173B44',col.main='#0C5964')
plot(singlemod,which=1,col='#157C80',pch=19,cex=.8,id.n=0)
plot(singlemod,which=2,col='#157C80',pch=19,cex=.8,id.n=0)
dev.off()
# Individual diagnostic figures match the two stacked right-column slide slots.
diagnostics <- data.frame(fitted=fitted(singlemod), residual=resid(singlemod),
                          standardized=rstandard(singlemod))
diagnostic_theme <- theme_minimal(base_size=16) + theme(
  panel.grid.minor=element_blank(), panel.grid.major.x=element_blank(),
  plot.title=element_text(size=17,face='bold',color=color_primary),
  axis.title=element_text(size=15,color=color_primary),
  axis.text=element_text(size=13,color=color_primary),
  plot.margin=margin(7,12,5,6))
p_residual <- ggplot(diagnostics,aes(fitted,residual)) +
  geom_hline(yintercept=0,linetype='dashed',color='#9EADAA') +
  geom_point(color=color_primary,alpha=.65,size=2) +
  scale_x_continuous(breaks=sort(unique(diagnostics$fitted)),
                     labels=function(x) sprintf('%.1f',x),expand=expansion(mult=.25)) +
  labs(title='Residuals vs fitted values',x='Fitted walk distance (m)',y='Residual (m)') + diagnostic_theme
p_qq <- ggplot(diagnostics,aes(sample=standardized)) +
  stat_qq_line(color=color_accent,linewidth=.8) +
  stat_qq(color=color_primary,alpha=.75,size=1.8) +
  labs(title='Normal Q-Q plot',x='Theoretical quantiles',y='Std. residual') + diagnostic_theme
ggsave('assets/residuals-vs-fitted.png',p_residual,width=5.80,height=2.38,dpi=220,bg='white')
ggsave('assets/normal-qq.png',p_qq,width=5.80,height=2.38,dpi=220,bg='white')
output<-c(paste('single means',paste(round(tapply(single$distance,single$treatment,mean),1),collapse=' ')),paste('single contrast',round(coef(singlemod)[2],2),'se',round(summary(singlemod)$coefficients[2,2],2)),paste('pooled',round(coef(m0)[2],2),'se',round(summary(m0)$coefficients[2,2],2),'resid_sd',round(sigma(m0),2)),paste('fixed',round(coef(m1)[2],2),'se',round(summary(m1)$coefficients[2,2],2),'resid_sd',round(sigma(m1),2)),paste('random',round(fixef(m2)[2],2),'se',round(summary(m2)$tTable[2,2],2),'center_var',round(as.numeric(vc[1,'Variance']),2),'resid_var',round(as.numeric(vc[2,'Variance']),2)))
writeLines(output,'assets/model_results.txt')
# Standalone figures use the shared teaching palette and match the slide visual slots.
# Conceptual distributions: arbitrary means, not fitted estimates from the trial.
concept_means <- c(Standard=-0.8, Enhanced=0.8)
curve_data <- expand.grid(distance=seq(-3.5,3.5,length.out=500),treatment=names(concept_means))
curve_data$density <- dnorm(curve_data$distance,mean=concept_means[curve_data$treatment],sd=0.85)
p4 <- ggplot(curve_data,aes(distance,density,color=treatment))+geom_line(linewidth=1.3)+geom_vline(data=data.frame(treatment=names(concept_means),m=as.numeric(concept_means)),aes(xintercept=m,color=treatment),linetype='dashed',linewidth=.8)+scale_color_manual(values=cols)+scale_x_continuous(breaks=unname(concept_means),labels=c(expression(beta[0]),expression(beta[0]+beta[1])))+labs(title='Conceptual distributions',x='Walk distance',y='Density',color='Treatment')+base+theme(legend.position='bottom',axis.title=element_text(size=17),axis.text.x=element_text(size=16),axis.text.y=element_blank(),axis.ticks.y=element_blank())
ggsave('assets/conditional-distributions.png',p4,width=6,height=5.2,dpi=200,bg='white')
resids <- data.frame(residual=c(resid(m0),resid(m1)),model=factor(rep(c('Treatment only','Treatment + center'),each=nrow(dat)),levels=c('Treatment only','Treatment + center')))
max_bin <- max(hist(resid(m0),breaks=seq(-240,240,12),plot=FALSE)$counts, hist(resid(m1),breaks=seq(-240,240,12),plot=FALSE)$counts)
lab <- data.frame(model=levels(resids$model),x=c(-95,-95),y=rep(max_bin*1.14,2),text=c(sprintf('Residual SD = %.1f m',sigma(m0)),sprintf('Residual SD = %.1f m',sigma(m1))))
p5 <- ggplot(resids,aes(residual))+geom_histogram(binwidth=12,boundary=0,fill=color_secondary,color='white')+facet_wrap(~model,ncol=2)+geom_text(data=lab,aes(x=x,y=y,label=text),inherit.aes=FALSE,hjust=0,color=color_primary,size=4.7)+labs(x='Model residual (m)',y='Patients')+base+theme(strip.text=element_text(size=15,face='bold',color=color_primary),axis.title=element_text(size=15),axis.text=element_text(size=12))
ggsave('assets/residual-variation.png',p5,width=10.8,height=4.5,dpi=200,bg='white')

# Export the statistics used by the presentation so the slides follow the data.
model_stats <- function(model) {
  estimate <- unname(coef(model)['treatmentEnhanced'])
  ci <- unname(confint(model)['treatmentEnhanced', ])
  list(estimate=estimate,se=unname(summary(model)$coefficients['treatmentEnhanced','Std. Error']),
       lower=ci[1],upper=ci[2], residual_sd=unname(sigma(model)))
}
single_ci <- unname(confint(singlemod)['treatmentEnhanced', ])
intercept_ci <- unname(confint(singlemod)['(Intercept)', ])
treatment_test <- summary(singlemod)$coefficients['treatmentEnhanced', ]
re_ci <- unname(intervals(m2,which='fixed')$fixed['treatmentEnhanced',c('lower','upper')])
vc_center <- as.numeric(vc[1,'Variance']); vc_patient <- as.numeric(vc[2,'Variance'])
statistics <- list(
  patients_per_arm=patients_per_arm, centers=length(centers), total=nrow(dat),
  generating_treatment_effect=treatment_effect,
  single=list(standard_mean=mean(single$distance[single$treatment=='Standard']),
    enhanced_mean=mean(single$distance[single$treatment=='Enhanced']),
    standard_sd=sd(single$distance[single$treatment=='Standard']),
    enhanced_sd=sd(single$distance[single$treatment=='Enhanced']),
    estimate=unname(coef(singlemod)['treatmentEnhanced']), lower=single_ci[1],upper=single_ci[2],
    t=unname(treatment_test['t value']),p=unname(treatment_test['Pr(>|t|)']),
    df=unname(df.residual(singlemod)),
    intercept=list(lower=intercept_ci[1],upper=intercept_ci[2],
      p=unname(summary(singlemod)$coefficients['(Intercept)','Pr(>|t|)'])),
    residual_sd=unname(sigma(singlemod)),residual_var=unname(sigma(singlemod)^2)),
  pooled=model_stats(m0),fixed=model_stats(m1),
  random=list(estimate=unname(fixef(m2)['treatmentEnhanced']),
    se=unname(summary(m2)$tTable['treatmentEnhanced','Std.Error']), lower=re_ci[1],upper=re_ci[2],
    center_var=vc_center,patient_var=vc_patient,icc=vc_center/(vc_center+vc_patient)))
jsonlite::write_json(statistics,'assets/model_results.json',auto_unbox=TRUE,digits=10,pretty=TRUE)

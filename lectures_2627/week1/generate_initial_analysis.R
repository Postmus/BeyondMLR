library(ggplot2)
library(jsonlite)
source('../../slides-common/R/common_fig.R')

# Analyze the existing full trial. Keep its data and all later-slide assets intact.
study <- read.csv('assets/rehabilitation_data.csv')
study$treatment <- factor(study$treatment,levels=c('Standard','Enhanced'))
model <- lm(distance ~ treatment,data=study)
coefficients <- summary(model)$coefficients
intervals <- confint(model)
term <- function(name) list(
  estimate=unname(coefficients[name,'Estimate']),
  se=unname(coefficients[name,'Std. Error']),
  lower=unname(intervals[name,1]),upper=unname(intervals[name,2]),
  p=unname(coefficients[name,'Pr(>|t|)']))
results <- list(n=nrow(study),centers=length(unique(study$center)),
  patients_per_arm=unname(table(study$treatment)['Standard']),
  intercept=term('(Intercept)'),treatment=term('treatmentEnhanced'),
  residual_sd=unname(sigma(model)),residual_var=unname(sigma(model)^2))
write_json(results,'assets/initial-analysis-results.json',auto_unbox=TRUE,digits=10,pretty=TRUE)

cols <- c(Standard=color_primary,Enhanced=color_accent)
p <- ggplot(study,aes(treatment,distance,fill=treatment)) +
  geom_boxplot(width=.48,alpha=.4) +
  scale_fill_manual(values=cols) +
  labs(x=NULL,y='6-minute walk distance (m)') +
  theme_minimal(base_size=16) +
  theme(panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
        legend.position='none',plot.margin=margin(10,16,10,10))
ggsave('assets/pooled-outcomes.png',p,width=7.7,height=4.5,dpi=220,bg='white')

diagnostics <- data.frame(fitted=fitted(model),residual=resid(model),standardized=rstandard(model))
diagnostic_theme <- theme_minimal(base_size=16) + theme(
  panel.grid.minor=element_blank(),panel.grid.major.x=element_blank(),
  plot.title=element_text(size=17,face='bold',color=color_primary),
  axis.title=element_text(size=15,color=color_primary),
  axis.text=element_text(size=13,color=color_primary),
  plot.margin=margin(7,12,5,6))
p_residual <- ggplot(diagnostics,aes(fitted,residual)) +
  geom_hline(yintercept=0,linetype='dashed',color='#9EADAA') +
  geom_point(color=color_primary,alpha=.38,size=1.6) +
  scale_x_continuous(breaks=sort(unique(diagnostics$fitted)),
                    labels=function(x) sprintf('%.1f',x),expand=expansion(mult=.25)) +
  labs(title='Residuals vs fitted values',x='Fitted walk distance (m)',y='Residual (m)') + diagnostic_theme
p_qq <- ggplot(diagnostics,aes(sample=standardized)) +
  stat_qq_line(color=color_accent,linewidth=.8) +
  stat_qq(color=color_primary,alpha=.6,size=1.5) +
  labs(title='Normal Q-Q plot',x='Theoretical quantiles',y='Std. residual') + diagnostic_theme
ggsave('assets/pooled-residuals-vs-fitted.png',p_residual,width=5.80,height=2.38,dpi=220,bg='white')
ggsave('assets/pooled-normal-qq.png',p_qq,width=5.80,height=2.38,dpi=220,bg='white')
print(results)

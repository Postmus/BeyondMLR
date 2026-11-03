# Observed center means for the transition from diagnostics to study design.
# Read the existing teaching data; no observations are regenerated.
source('../../slides-common/R/common_fig.R')
study <- read.csv('assets/rehabilitation_data.csv')
centers <- sort(unique(study$center))
treatments <- c('Standard', 'Enhanced')
allocation <- table(factor(study$center, levels=centers),
                    factor(study$treatment, levels=treatments))
stopifnot(nrow(study) == 480, length(centers) == 6, all(allocation == 40))
means <- with(study, tapply(distance, list(center, treatment), mean))
means <- means[centers, treatments]
overall <- tapply(study$distance, study$center, mean)[centers]
stopifnot(isTRUE(all.equal(as.numeric(overall), as.numeric(rowMeans(means)))))

png('assets/center-summary.png', width=1800, height=1800, res=300, bg='white')
par(family='sans', mar=c(5.5, 3.3, 2.1, .8), mgp=c(2.8, .75, 0),
    col.axis=color_text, col.lab=color_primary, fg=color_primary, las=1,
    cex.axis=1.3, cex.lab=1.35)
limits <- range(pretty(range(means), n=5))
plot(NA, xlim=limits+c(-3, 3), ylim=c(6.4, .6), xaxs='i', yaxs='i',
     xlab='', ylab='', axes=FALSE)
ticks <- pretty(limits, n=5)
abline(v=ticks, col=color_grid, lwd=1)
axis(1, at=ticks, tick=FALSE, col=NA)
axis(2, at=seq_along(centers), labels=sub('Center ', '', centers),
     tick=FALSE, col=NA)
mtext('Mean walk distance (m)', side=1, line=2, cex=1.35, col=color_primary)
mtext('Center', side=2, line=1.7, las=0, cex=1.35, col=color_primary)
title('Observed center means', cex.main=1.45, col.main=color_primary)
points(means[, 'Standard'], seq_along(centers), pch=16,
       col=color_primary, cex=1.55)
points(overall, seq_along(centers), pch=18, col='black', cex=1.6)
points(means[, 'Enhanced'], seq_along(centers), pch=16,
       col=color_accent, cex=1.55)
legend('bottom', inset=c(0, -.225), xpd=NA, horiz=TRUE, bty='n',
       legend=c('Standard', 'Overall', 'Enhanced'), pch=c(16, 18, 16),
       col=c(color_primary, 'black', color_accent), pt.cex=1.4, cex=1.08,
       x.intersp=.6, y.intersp=1)
invisible(dev.off())
print(data.frame(center=centers, standard=means[, 'Standard'],
                 overall=overall, enhanced=means[, 'Enhanced']), row.names=FALSE)

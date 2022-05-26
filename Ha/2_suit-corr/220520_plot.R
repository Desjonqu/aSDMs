rm(list=ls())
setwd("./data")
library(lme4)

load("breadth_Ha.Rdata")
load(file='model_boot_ID.Rdata')
load("Ha_monthly.Rdata")
xdata <- dat
xdata$z.temp <- scale(xdata$tmin)
xdata$z.prec <- scale(xdata$prec)
xdata$z.prec2 <- scale(xdata$prec2)
xdata$z.ph.ch <- scale(xdata$ph.ch)
xdata$calling.freq <- xdata$calling.freq/xdata$n

mean.zphot <- mean(xdata$z.ph.ch[which(xdata$calling.freq!=0)])#mean of the ph.ch period when Hyla is calling
mean.ztemp <- mean(xdata$z.temp[which(xdata$calling.freq!=0)])#mean of the ph.ch period when Hyla is calling
mean.zprec <- mean(xdata$z.prec[which(xdata$calling.freq!=0)])#mean of the ph.ch period when Hyla is calling

mean.prec <- which(abs(mean.zprec-boot.res$ci.predicted$z.prec)==min(abs(mean.zprec-boot.res$ci.predicted$z.prec))&abs(mean.zphot-boot.res$ci.predicted$z.ph.ch)==min(abs(mean.zphot-boot.res$ci.predicted$z.ph.ch)))

mean.temp <- which(abs(mean.ztemp-boot.res$ci.predicted$z.temp)==min(abs(mean.ztemp-boot.res$ci.predicted$z.temp))&abs(mean.zphot-boot.res$ci.predicted$z.ph.ch)==min(abs(mean.zphot-boot.res$ci.predicted$z.ph.ch)))

mean.prectemp <- which(abs(mean.zprec-boot.res$ci.predicted$z.prec)==min(abs(mean.zprec-boot.res$ci.predicted$z.prec))&abs(mean.ztemp-boot.res$ci.predicted$z.temp)==min(abs(mean.ztemp-boot.res$ci.predicted$z.temp)))

means <- c(mean.prec, mean.temp, mean.prectemp)

bitmap(file = 'Ha_Fig3_model-envvariables.jpg', res = 700, width = 3400, height = 2200, units = 'px')
par(mfrow=c(2,3), mar=c(4.1, 4.1, 3, 0.8), oma=c(0,0.5,0,0), lwd=0.5)
col=ifelse(xdata$pop=='cold', yes='blue', no='orange')
pch=21
ylim=range(boot.res$ci.predicted$fitted[means], boot.res$ci.predicted$lower.cl[means], boot.res$ci.predicted$upper.cl[means], xdata$calling.freq)

plot(boot.res$ci.predicted$z.temp[mean.prec], boot.res$ci.predicted$fitted[mean.prec], type='l', ylim=ylim, xaxt='n', yaxt='n', xlab="monthly minimum temperature (°C)", ylab="calling probability", cex.lab=1.6, cex.axis=1.2)
at=seq(-1.5, 1.5, 0.5)
labels=round(at*sd(xdata$tmin)+mean(xdata$tmin), 1)
axis(1, at = at, labels = labels, cex.axis=1.2, lwd=0.3)
axis(2, cex.axis=1.2, lwd=0.3)
lines(boot.res$ci.predicted$z.temp[mean.prec], boot.res$ci.predicted$upper.cl[mean.prec], lty=2, col="darkgrey")
lines(boot.res$ci.predicted$z.temp[mean.prec], boot.res$ci.predicted$lower.cl[mean.prec], lty=2, col="darkgrey")
points(xdata$z.temp, xdata$calling.freq, col='black', bg=col, lwd=0.3, pch=pch)

plot(boot.res$ci.predicted$z.ph.ch[mean.prectemp], boot.res$ci.predicted$fitted[mean.prectemp], type='l', ylim=ylim, xaxt='n', yaxt='n', xlab="monthly change in photoperiod (min.)", ylab='', cex.lab=1.6, cex.axis=1.2, main='Regression model', cex.main=2)
at=seq(-1.5, 1.5, 0.5)
labels=round((at*sd(xdata$ph.ch)+mean(xdata$ph.ch))*60, 1)
axis(1, at = at, labels = labels, cex.axis=1.2, lwd=0.3)
axis(2, cex.axis=1.2, lwd=0.3)
lines(boot.res$ci.predicted$z.ph.ch[mean.prectemp], boot.res$ci.predicted$upper.cl[mean.prectemp], lty=2, col="darkgrey")
lines(boot.res$ci.predicted$z.ph.ch[mean.prectemp], boot.res$ci.predicted$lower.cl[mean.prectemp], lty=2, col="darkgrey")
points(xdata$z.ph.ch, xdata$calling.freq, col='black', bg=col, lwd=0.3, pch=pch)

plot(boot.res$ci.predicted$z.prec[mean.temp], boot.res$ci.predicted$fitted[mean.temp], type='l', ylim=ylim, xaxt='n', yaxt='n', xlab="monthly cumulated precipitation (mm)", ylab="", cex.lab=1.6, cex.axis=1.2)
at=seq(-1.5, 2, 0.5)
labels=round(at*sd(xdata$prec)+mean(xdata$prec), 1)
axis(1, at = at, labels = labels, cex.axis=1.2, lwd=0.3)
axis(2, cex.axis=1.2, lwd=0.3)
lines(boot.res$ci.predicted$z.prec[mean.temp], boot.res$ci.predicted$upper.cl[mean.temp], lty=2, col="darkgrey")
lines(boot.res$ci.predicted$z.prec[mean.temp], boot.res$ci.predicted$lower.cl[mean.temp], lty=2, col="darkgrey")
points(xdata$z.prec, xdata$calling.freq, col='black', bg=col, lwd=0.3, pch=pch)


pch=ifelse(xdata$calling.freq==0, yes=23, no=21)
pch=ifelse(xdata$prec<res$call_precmin, yes=4, no=pch)

plot(0,type='n',axes=FALSE,ann=FALSE)

plot(((xdata$pcmax+xdata$pcmin)/2)*60, (xdata$tmax+xdata$tmin)/2, ylim=range(c(xdata$tmax,xdata$tmin, res$call_tmax, res$call_tmin)), xlab='monthly change in photoperiod (min.)', ylab='monthly minimum temperature (°C)', main='Boundary model', col='black', bg=col, lwd=0.3, pch=pch, cex.lab=1.6, cex.axis=1.2, cex.main=2, xaxt='n', yaxt='n')
axis(1, cex.axis=1.2, lwd=0.3)
axis(2, cex.axis=1.2, lwd=0.3)
rect(xleft=res$call_phmin*60, ybottom=res$call_tmin, xright=res$call_phmax*60, ytop=res$call_tmax)

mtext(text = c("(a)", "(b)", "(c)", "(d)"), side = 1, at = c(-11, -3.2, 4.5, -3.2), line = c(-42, -42, -42, -18.2), font = 1)

par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
par(cex=1)
legend('topright', legend = c('cold population', 'hot population', 'calling', 'not calling', 'not calling (below prec.threshold)'), pt.bg=c('blue', 'orange', 'black', 'black', 'black'), col='black', pch=c(21,21,21,23,4), bty='n',inset=c(0.68,0.6), xpd = TRUE, pt.lwd=0.3)
dev.off()


rm(list=ls())

#Loading data
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
load('output-validation.RData')
load('Ha_monthly.Rdata')

#Correlation validation----
cor.pred <- cor.suit <- rep(NA, 1000)
pv.pred <- pv.suit <- rep(NA, 1000)

ind <- (1:1000)#[-which(is.na(rowSums(output$predcall))|rowSums(output$suitability)==0)]

for (i in ind){
  sha.suit <- shapiro.test(output$suitability[i,])
  sha.pred <- shapiro.test(output$predcall[i,])
  sha.obs <- shapiro.test(output$obscall[i,])
  meth.suit <- ifelse(sha.obs$p.value<0.05|sha.suit$p.value<0.05, yes='spearman', no='pearson')
  meth.pred <- ifelse(sha.obs$p.value<0.05|sha.pred$p.value<0.05, yes='spearman', no='pearson')
  c.suit <- cor.test(output$suitability[i,],output$obscall[i,], method=meth.suit)
  c.pred <- cor.test(output$predcall[i,],output$obscall[i,], method=meth.pred)
  cor.pred[i] <- c.pred$estimate
  cor.suit[i] <- c.suit$estimate
  pv.pred[i] <- c.pred$p.value
  pv.suit[i] <- c.suit$p.value
}
qu.pred <- quantile(cor.pred, probs=c(0.05,0.5,0.95), na.rm = TRUE)
length(which(cor.pred<0))
length(which(sort(pv.pred)<0.05))

qu.suit <- quantile(cor.suit, probs=c(0.05,0.5,0.95), na.rm = TRUE)
length(which(cor.suit<0))
length(which(sort(pv.suit)<0.05))

qu <- rbind(qu.pred, qu.suit)


bitmap(file = 'Ha_fig2_correlation_obs-pred.jpg', res = 700, width = 2000, height = 2000, units = 'px')
plot(1:2,qu[,2], pch=16, xlab='Models', ylab='Spearman correlation', ylim=c(-1,1), xaxt='n', xlim=c(.5, 2.5))
axis(side = 1, at = 1:2, labels = factor(c('Regression', 'Boundary')))
arrows(x0 = 1:2, y0 = qu[,1], y1 = qu[,3], length = 0)
abline(h=0, lty=2)
#abline(h=0.344, lty=2, col="red")#alpha=0.1
abline(h=0.407, lty=2, col="red")#alpha=0.05
dev.off()

bitmap(file = 'Ha_figS1_res_obs-pred.jpg', res = 700, width = 2000, height = 2000, units = 'px')
resid.corr <- output$predcall-output$obscall
x1 <- as.numeric(dat$month)+rep(c(0, 0.2), each=12)
lim <- max(abs(resid.corr), na.rm=TRUE)
ylim <- c(-lim, lim)
plot(x1,apply(resid.corr, 2, FUN=mean, na.rm=TRUE), col=rep(c("blue", 'orange'), each=12), ylim=ylim, xlab='Months', ylab='Residual calling frequency', pch=16)
arrows(x0=x1, y0=apply(resid.corr, 2, FUN=quantile, probs=0.9, na.rm=TRUE), y1=apply(resid.corr, 2, FUN=quantile, probs=0.10, na.rm=TRUE), length=0,col=rep(c("blue", 'orange'), each=12))
abline(h=0)
dev.off()


bitmap(file = 'Fig3.jpg', res = 700, width = 4000, height = 2000, units = 'px')
par(mfrow=c(1,2))
plot(1:2,qu[,2], pch=21, xlab='Models', ylab='Spearman correlation', ylim=c(-1,1), xaxt='n', xlim=c(.5, 2.5), cex.lab=1.5, cex.axis=1.2, bg='black')
axis(side = 1, at = 1:2, labels = factor(c('Regression', 'Boundary')))
arrows(x0 = 1:2, y0 = qu[,1], y1 = qu[,3], length = 0)
abline(h=0, lty=1)
#abline(h=0.344, lty=2, col="red")#alpha=0.1
abline(h=0.407, lty=2, col="red")#alpha=0.05

resid.corr <- output$predcall-output$obscall
x1 <- as.numeric(dat$month)+rep(c(0, 0.2), each=12)
lim <- max(abs(resid.corr), na.rm=TRUE)
ylim <- c(-lim, lim)
plot(x1,apply(resid.corr, 2, FUN=mean, na.rm=TRUE), col='black', bg=rep(c("blue", 'orange'), each=12), ylim=ylim, xlab='Months', ylab='Residuals', pch=21, cex.lab=1.5, cex.axis=1.2, cex=1.2, lwd=0.3)
arrows(x0=x1, y0=apply(resid.corr, 2, FUN=quantile, probs=0.9, na.rm=TRUE), y1=apply(resid.corr, 2, FUN=quantile, probs=0.10, na.rm=TRUE), length=0)
abline(h=0)
mtext(text = c('(a)', '(b)'),side = 1, line = c(-21, -21), at = c(-15.5, 0), cex=1.5)
legend('topright', legend = c('cold', 'hot'), pt.bg = c("blue", 'orange'), bty = "n", pch = 21, lty = 1, title='population', cex=1.2, col='black')
dev.off()

save.image(file="Ha_valtemp-assess.RData")

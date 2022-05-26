rm(list=ls())
setwd("./data")
library(lme4)
library(car)

#loading and formating data
load("Ha_monthly.Rdata")
xdata <- dat
xdata$z.temp <- scale(xdata$tmin)
xdata$z.prec <- scale(xdata$prec)
xdata$z.prec2 <- scale(xdata$prec2)
xdata$z.photo <- scale(xdata$photo)
xdata$z.ph.ch <- scale(xdata$ph.ch)
xdata$ID <- as.factor(1:length(xdata$pop))
xdata$pop.dum <- scale(as.numeric(as.factor(xdata$pop)))

#Model ----
contr <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))
freq <- cbind(xdata$calling.freq, xdata$n-xdata$calling.freq)

res <- glmer(freq~z.temp+I(z.temp^2)+z.prec+z.ph.ch+pop+(1|ID), data=xdata, family='binomial', control=contr)

#Assumptions-----


#Normality and independence of residuals

source('../../functions/diagnostic_fcns.r')
diagnostics.plot(res) # looks fine
overdisp.test(res)

##Colinearity
res.lm <- lm(freq[,1]~z.temp+z.prec+I(z.temp^2)+z.ph.ch+pop, data=xdata)
vif(res.lm)# all <4 => no issue

## Distribution of the random effects
ranef.diagn.plot(res) #Looks ok... 
summary(res)$varcor

#fit----

a <- exp(predict(res))
plot(rep(1:12, 2)+rep(c(0,0.1), each=12), a/(1+a)-xdata$calling.freq/xdata$n, pch=16, col=rep(c('blue', 'orange'), each=12))
abline(h=0)

#Inferences----

null <- glmer(freq~1+(1|ID), data=xdata, family='binomial', control=contr)
anova(res, null, "Chisq")
drop1(res, test='Chisq')


summary(res)
ranef(res)

summary(res)$coefficients

#Calculate confidence intervals----

res <- glmer(freq~z.temp+I(z.temp^2)+z.prec+z.ph.ch+pop.dum+(1|ID), data=xdata, family='binomial', control=contr)#, family='poisson'

source('../../functions/boot_glmm.r')
boot.res=boot.glmm.pred(model.res=res, excl.warnings=T, nboots=100, para=F, use=c('z.temp', 'z.prec', 'z.ph.ch'), level=0.95, resol=50)
save(boot.res, file='model_boot_ID.Rdata')

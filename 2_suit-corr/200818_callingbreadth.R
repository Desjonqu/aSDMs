rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
load("Ha_daily.Rdata")
source("../2_suit-corr/200312_breadthfunctions.R")


calling <- xdata$calling.freq
tmin <- xdata$tmin
tmax <- xdata$tmax
pop <- xdata$pop
prec <- xdata$prec
photo <- xdata$ph.ch
month <- xdata$month
res <- breadth(calling=calling, tmin=tmin, tmax=tmax, prec=prec, photo=photo, month=month, pop=pop, min.nb.days=30, mean.prec=FALSE)


save(res, file="breadth_Ha.Rdata")


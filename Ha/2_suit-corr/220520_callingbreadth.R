rm(list=ls())
setwd("./data")
load("Ha_daily.Rdata")
source("../2_suit-corr/breadthfunctions.R")

#input data
calling <- xdata$calling.freq
tmin <- xdata$tmin
tmax <- xdata$tmax
pop <- xdata$pop
prec <- xdata$prec
photo <- xdata$ph.ch
month <- xdata$month

#running the breadth function
res <- breadth(calling=calling, tmin=tmin, tmax=tmax, prec=prec, photo=photo, month=month, pop=pop, min.nb.days=30, mean.prec=FALSE)

#saving the results
save(res, file="breadth_Ha.Rdata")


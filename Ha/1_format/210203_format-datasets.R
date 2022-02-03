rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
library(geosphere)

#loading data----
climate <- read.csv("Ha_climat-daily.csv")
calling <- read.csv("Ha_act-hourly.csv", header=TRUE, sep=',')
coord <- read.csv("Ha_site_coordinates.csv", header=TRUE, sep=',')
coord$pop <- as.factor(coord$pop)

#check data
head(calling)
head(climate)

#Daily format----
##Format the values of each line defined by the date and replicate site
unique.daily <- unique(paste(calling$date, calling$pop))
dim.daily <- length(unique.daily)#dimension of the daily dataset => make general = instead of calling pop: "replicate"
date.daily <- substr(unique.daily, start = 1, stop=10)
pop.daily <- substr(unique.daily, start = 12, stop=nchar(unique.daily))
lines <- sort(which(paste(climate$date, climate$pop)%in%unique.daily))#assumption that data is order by date

##dataframe that will be filled
xdata <- data.frame(pop=pop.daily, date=date.daily, month=substr(date.daily, start=4, stop=5), year=substr(date.daily, start=7, stop=10), calling.freq=NA, n=NA, tmin=climate$tmin[lines], tmax=climate$tmax[lines], prec=climate$prec[lines], photo=NA)


##Calling frequency and number of recordings to derive this data
aggreg.levels <- paste(calling$pop, substr(calling$date, start=7, stop=10), substr(calling$date, start=4, stop=5), substr(calling$date, start=1, stop=2))
xdata$calling.freq <- aggregate(calling$Ha, by=list(aggreg.levels), FUN=sum, na.rm=TRUE)[,2]

rec <- ifelse(is.na(calling$Ha), yes=0, no=1)
xdata$n <- aggregate(rec, by=list(aggreg.levels), FUN=sum, na.rm=TRUE)[,2]

#photoperiod
date.f <- as.Date(x = xdata$date, format='%d/%m/%Y')
for (i in levels(coord$pop)){
  lastrep <- max(which(xdata$pop==i))
  dates <- c(date.f[xdata$pop==i],  date.f[lastrep]+1)
  coor <- coord$lat[coord$pop==i]
  xdata$photo[xdata$pop==i] <- daylength(coor, date.f[xdata$pop==i])
  xdata$ph.ch[xdata$pop==i] <- diff(daylength(coor, dates))
}

##check data
head(xdata)
str(xdata)
save(xdata, file="Ha_daily.Rdata")



#Monthly format----
source('../../functions/format-monthly.R')
data.aggr <- xdata[,c(5:11, 10, 10, 11, 11)]
names(data.aggr) <- c(names(xdata)[5:11], 'phmin', 'phmax', 'pcmin', 'pcmax')
FUNs <- c("sum", "sum", 'mean', 'mean', 'sum', 'mean', 'mean', 'min', 'max', 'min', 'max')
pop <- xdata$pop
date <- xdata$date

data.bef <- data.frame(prec2=climate$prec)
pop.bef <- climate$pop
date.bef <- climate$date
FUNs.bef <- 'sum'

dat <- monthlyformat(data.aggr=data.aggr, FUNs=FUNs, pop=pop, date=date, data.bef=data.bef, FUNs.bef=FUNs.bef, pop.bef=pop.bef, date.bef=date.bef, nchar.diff=3)

head(dat)
str(dat)
save(dat, file="Ha_monthly.Rdata")



rm(list=ls())
library(lme4)

#Loading data
setwd("./data")
load("Ha_daily.Rdata")
source("../2_suit-corr/breadthfunctions.R")
source('../../functions/format-monthly.R')
source('../../functions/boot_glmm.r')
contr <- glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=10000000))

data <- xdata
data$pop <- as.factor(data$pop)
climate <- read.csv("Ha_climat-daily.csv", header=TRUE)
climate$year <- substr(climate$date, start=7, stop=10)
climate$month <- substr(climate$date, start=4, stop=5)
iterations=1000#Number of validation iterations
day.val=8#number of days per month kept for validation (the rest for training)

#Prepare the output dataset
output <- list(valid.days=matrix(NA, ncol=24*8, nrow=iterations), suitability=matrix(NA, ncol=24, nrow=iterations), predcall=matrix(NA, ncol=24, nrow=iterations), predday=matrix(NA, ncol=24, nrow=iterations), obscall=matrix(NA, ncol=24, nrow=iterations), traincall=matrix(NA, ncol=24, nrow=iterations))

for (i in 1:iterations){
  #random choice of 8 days per  month for validation ----
  dim.monthly <- length(unique(paste(climate$pop,climate$year,climate$month)))
  number.days <- as.vector(table(paste(climate$year,climate$month), climate$pop))
  number.days <- number.days[number.days!=0]
  valid.days.bef <- rep(NA, dim.monthly*day.val)
  index <- c(0,cumsum(number.days)[-dim.monthly])
  for(j in 1:dim.monthly){
    valid.days.bef[((j-1)*day.val+1):(day.val+(j-1)*day.val)] <- sample(x=1:number.days[j], size=day.val, replace = FALSE)+index[j]
  }
  valid.days.bef <- sort(valid.days.bef)
  
  ind.sub <- rep(NA, (dim.monthly-length(levels(data$pop)))*day.val)
  first.months <- c()
  year.month <- as.numeric(paste(climate$year, climate$month, sep=''))
  ini <- 1
  end <- subs <- 0
  for (j in levels(data$pop)){
    a <- which(levels(data$pop)==j)
    first.month <- min(year.month[climate$pop==j])
    first.months <- c(first.months, which(paste(climate$pop, year.month)==paste(j, first.month)))
    end <- end+(length(unique(year.month[climate$pop==j]))-1)*day.val
    subs <- subs+number.days[a+(ini-1)/day.val]
    ind.sub[ini:end] <- subs
    ini <- end+1
  }

  valid.days <- valid.days.bef[!valid.days.bef%in%first.months]-ind.sub
  output$valid.days[i,] <- valid.days
  
  #building the two separate datasets----
  training <- data[-valid.days,]
  validate <- data[valid.days,]

  training.bef <- climate[-valid.days.bef,]
  validate.bef <- climate[valid.days.bef,]
  
  #observed monthly calling frequency----
  
  data.aggr <- validate[,c(5:11, 10, 10, 11, 11)]
  names(data.aggr) <- c(names(validate)[5:11], 'phmin', 'phmax', 'pcmin', 'pcmax')
  FUNs <- c("sum", "sum", 'mean', 'mean', 'mean', 'mean', 'mean', 'min', 'max', 'min', 'max')
  pop <- validate$pop
  date <- validate$date
  
  data.bef <- data.frame(prec2=validate.bef$prec)
  FUNs.bef <- 'mean'
  pop.bef <- validate.bef$pop
  date.bef <- validate.bef$date
  
  valid <- monthlyformat(data.aggr=data.aggr, FUNs=FUNs, pop=pop, date=date, data.bef=data.bef, FUNs.bef=FUNs.bef, pop.bef=pop.bef, date.bef=date.bef, nchar.diff=3)
  
  valid$n[valid$n==0] <- 24*day.val
  output$obscall[i,] <- valid$calling.freq/valid$n
  if(is.na(sum(output$obscall[i,]))){
    print(i)
    next
  }
    
  #Breadth approach----
  #computes the thermal and precipitation extremes
  calling <- training$calling.freq
  tmin <- training$tmin
  tmax <- training$tmax
  replic <- training$pop
  prec <- training$prec
  photo <- training$ph.ch
  month <- training$month
  res <- breadth(calling, tmin, tmax, prec, photo, month, replic, min.nb.days=22, mean.prec=TRUE)

  ##Computes the suitability
  #for temperature and photoperiod
  for (j in 1:length(valid$calling.freq)){
  tmin <- valid$tmin[j]
  tmax <- valid$tmax[j]
  prec <- valid$prec[j]
  phmin <- valid$pcmin[j]
  phmax <- valid$pcmax[j]
  month <- valid$month[j]
  output$suitability[i,j] <- suit(tmin, tmax, prec, phmin, phmax, res)
  }
  
  #Regression model----
  ##Convert into monthly data
  data.aggr <- training[,c(5:11, 10, 10, 11, 11)]
  names(data.aggr) <- c(names(training)[5:11], 'phmin', 'phmax', "pcmin", "pcmax")
  FUNs <- c("sum", "sum", 'mean', 'mean', 'mean', 'mean', 'mean', 'min', 'max', 'min', 'max')
  pop <- training$pop
  date <- training$date
  
  data.bef <- data.frame(prec2=training.bef$prec)
  FUNs.bef <- 'mean'
  pop.bef <- training.bef$pop
  date.bef <- training.bef$date
  
  train <- monthlyformat(data.aggr=data.aggr, FUNs=FUNs, pop=pop, date=date, data.bef=data.bef, FUNs.bef=FUNs.bef, pop.bef=pop.bef, date.bef=date.bef, nchar.diff=3)
  
  ##monthly model
  freq <- cbind(train$calling.freq, train$n-train$calling.freq)
  train$z.temp <- scale(train$tmin)
  train$z.prec <- scale(train$prec)
  train$z.prec2 <- scale(train$prec2)
  train$z.phot <- scale(train$photo)
  train$z.ph.ch <- scale(train$ph.ch)
  train$ID <- as.factor(1:length(train$pop))
  pop.dum <- scale(as.numeric(as.factor(train$pop)))
  
  res <- try(glmer(freq~z.prec+z.temp+I(z.temp^2)+z.ph.ch+pop.dum+(1|ID), data=train, family='binomial', control=contr), TRUE)
  
  if(isTRUE(class(res)=="try-error")) {
    print(i)
    geterrmessage()
    next
    } else { 
    boot.res=boot.glmm.pred(model.res=res, excl.warnings=F, nboots=1, para=F, use=c('z.temp', 'z.prec', 'z.ph.ch'), level=0.95, resol=50)
    
    ##Predictions for the validation base
    ##ATTENTION Prendre les valeurs de temp et prec originales non centrées!!!!
    ori.tmin <- boot.res$ci.predicted$z.temp*sd(train$tmin)+mean(train$tmin)
    ori.prec <- boot.res$ci.predicted$z.prec*sd(train$prec)+mean(train$prec)
    ori.phot <- boot.res$ci.predicted$z.ph.ch*sd(train$ph.ch)+mean(train$ph.ch)
    fitted <- boot.res$ci.predicted$fitted
    for (k in 1:24){output$predcall[i,k] <- fitted[which(abs(valid$tmin[k]-ori.tmin)==min(abs(valid$tmin[k]-ori.tmin))&abs(valid$prec[k]-ori.prec)==min(abs(valid$prec[k]-ori.prec))&abs(valid$ph.ch[k]-ori.phot)==min(abs(valid$ph.ch[k]-ori.phot)))[1]]}
    }

  print(paste(i,'/1000', sep=""))
}

save(output, file='output-validation.RData')

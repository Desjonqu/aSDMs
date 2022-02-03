#Author: Camille Desjonquères - 2020
#Function to format the data at the monthly scale from daily data

monthlyformat <- function(data.aggr, FUNs=NULL, pop, date, data.bef=NULL, FUNs.bef, pop.bef=NULL, date.bef, nchar.diff=3){
  #nchar.diff => number of characters necessary to distinguish the names of the populations (here 3 is sufficient)
  
  #format the grouping factors
  month.year <- substr(date, start=4, stop=10)
  unique.monthly <- unique(paste(month.year, pop))
  dim.monthly <- length(unique.monthly)
  pop.monthly <- substr(unique.monthly, start = 9, stop=max(nchar(unique.monthly)))
  aggreg.levels <- paste(pop, substr(date, start=7, stop=10), substr(date, start=4, stop=5))
    
  #Create the dataframe
  xdata <- data.frame(matrix(NA, nrow = dim.monthly, ncol=dim(data.aggr)[2]+dim(data.bef)[2]+3))
  names(xdata) <- c('pop', 'month', 'year', names(data.aggr), names(data.bef))
  xdata$pop=pop.monthly
  xdata$month=substr(unique.monthly, start=1, stop=2)
  xdata$year=substr(unique.monthly, start=4, stop=8)
  
  for (i in names(data.aggr)){
    col <- which(names(data.aggr)==i)
    if (FUNs[col]=='mean'){
     xdata[,i] <- aggregate(data.aggr[,i], by=list(aggreg.levels), FUN=mean, na.rm=TRUE)[,2]
    }
    if (FUNs[col]=='sum'){
      xdata[,i] <- aggregate(data.aggr[,i], by=list(aggreg.levels), FUN=sum, na.rm=TRUE)[,2]
    }
    if (FUNs[col]=='max'){
      xdata[,i] <- aggregate(data.aggr[,i], by=list(aggreg.levels), FUN=max, na.rm=TRUE)[,2]
    }
    if (FUNs[col]=='min'){
      xdata[,i] <- aggregate(data.aggr[,i], by=list(aggreg.levels), FUN=min, na.rm=TRUE)[,2]
    }
  }
  
  #To format and sum data from two months
  
  
  for (i in names(data.bef)){
    col <- which(names(xdata)==i)
    col.bef <- which(names(data.bef)==i)
    if (FUNs.bef[col.bef]=='mean'){
      monthly.bef <- aggregate(data.bef, by=list(paste(pop.bef, substr(date.bef, start=9,stop=10), substr(date.bef, start=4,stop=5))), FUN=mean, na.rm=TRUE)
    }
    if (FUNs.bef[col.bef]=='sum'){
      monthly.bef <- aggregate(data.bef, by=list(paste(pop.bef, substr(date.bef, start=9,stop=10), substr(date.bef, start=4,stop=5))), FUN=sum, na.rm=TRUE)
    }
    if (FUNs.bef[col.bef]=='max'){
      monthly.bef <- aggregate(data.bef, by=list(paste(pop.bef, substr(date.bef, start=9,stop=10), substr(date.bef, start=4,stop=5))), FUN=max, na.rm=TRUE)
    }
    if (FUNs.bef[col.bef]=='min'){
      monthly.bef <- aggregate(data.bef, by=list(paste(pop.bef, substr(date.bef, start=9,stop=10), substr(date.bef, start=4,stop=5))), FUN=min, na.rm=TRUE)
    }
    for (k in levels(pop.bef)){#climate should start one month before the calling data for this
      a <- which(substr(monthly.bef$Group.1, start=1, stop=nchar.diff)==substr(k, start=1, stop=nchar.diff))
      which.bef <- a[1:length(a)-1]
      which.now <- a[2:length(a)]
      xdata[xdata$pop==k, col] <- monthly.bef[which.bef,2]+monthly.bef[which.now,2]
    }
  }
  return(xdata)
}
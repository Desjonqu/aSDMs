#Breadth function----
##computes the thermal, photoperiod and precipitation extremes based on daily data over at least one year and preferably 2 sites representing climatic and latitudinal extremes
##Returns call_tmin, call_tmax, call_precmin, call_phmin, call_phmax

breadth <- function(calling, tmin, tmax, prec, photo, month, pop, min.nb.days=30, mean.prec=FALSE){
  night_call <- which(calling>0)
  if(length(night_call)<min.nb.days){print('Not enough calling data to find the proper extremes')}
  
  call_tmin <- mean(sort(tmin[night_call])[1:min.nb.days], na.rm=TRUE)
  call_tmax <- mean(sort(tmax[night_call], decreasing=T)[1:min.nb.days], na.rm=TRUE)
  call_phmin <- mean(sort(photo[night_call])[1:min.nb.days], na.rm=TRUE)
  #min(photo[night_call])
  call_phmax <- mean(sort(photo[night_call], decreasing=T)[1:min.nb.days],na.rm=TRUE)
  #max(photo[night_call]
  
  monthcall <- aggregate(calling, by=list(paste(pop, month)), FUN=sum)[,2]
  if(mean.prec){
  cumprec <- aggregate(prec, by=list(paste(pop, month)), FUN=mean, na.rm=TRUE)[,2]
  } else {cumprec <- aggregate(prec, by=list(paste(pop, month)), FUN=sum, na.rm=TRUE)[,2]}
  month_call <- which(monthcall>0)
  call_precmin <- min(cumprec[month_call])
  breadth <- data.frame(call_tmin, call_tmax, call_precmin, call_phmin, call_phmax)
  return(breadth)
}

#suitability function----
##computes the thermal and diel suitability for calling based on monthly data and breadth values
##Returns suit, the predicted suitability value between 0 and 1 that represent the percentage of overlap between current conditions and the breadth
suit <- function(tmin, tmax, prec, phmin, phmax, breadth){
  #precipitation
  if(breadth$call_precmin>prec){
    suit=0
  } else {
  
  #temperature
  nooverlapmax= ifelse(breadth$call_tmax>tmax,breadth$call_tmax-tmax,0)
  nooverlapmin= ifelse(breadth$call_tmin<tmin,tmin-breadth$call_tmin,0)
  s= ifelse(breadth$call_tmax>tmin & breadth$call_tmin<tmax, breadth$call_tmax-breadth$call_tmin-nooverlapmax-nooverlapmin, 0)
  tvar=tmax-tmin
  suit_t= s/tvar
  
  #photoperiod
  nooverlapmax= ifelse(breadth$call_phmax>phmax,breadth$call_phmax-phmax,0)
  nooverlapmin= ifelse(breadth$call_phmin<phmin,phmin-breadth$call_phmin,0)
  s= ifelse(breadth$call_phmax>phmin & breadth$call_phmin<phmax, breadth$call_phmax-breadth$call_phmin-nooverlapmax-nooverlapmin, 0)
  phvar=phmax-phmin
  suit_ph=s/phvar
  
  suit=suit_ph*suit_t
  } 
  return(suit)
}
#Author: Camille Desjonquères 11/03/2020
#Function to format aemet raw data
aemetformat <- function(tmin, tmax, prec, file, month, year){
  tmin_vect <- as.vector(t(tmin[,-(1:2)]))/10
  tmax_vect <- as.vector(t(tmax[,-(1:2)]))/10
  prec_vect <- as.vector(t(prec[,-(1:2)]))/10
  
  date <- paste(rep(formatC(1:31, digits = 1, flag='0'), dim(tmin)[1]), rep(formatC(month, digits=1, flag='0'), each=31), rep(year, each=31), sep='/')
  
  xdata <- data.frame(date=date, tmax=tmax_vect, tmin=tmin_vect, prec=prec_vect)
  
  a <- which(is.na(as.Date(date, format = '%d/%m/%Y')))
  
  xdata <- xdata[-a,]
  
  write.csv(xdata,file=file, row.names=FALSE)
  return(xdata)
}

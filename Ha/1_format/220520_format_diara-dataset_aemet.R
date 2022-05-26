rm(list=ls())
setwd("./data")
source('../../functions/format-aemet.R')#loading function to format aemet weather files

#Loading weather files
tmin <- read.csv("aemet_tmin_genosto_Ha-cold.csv", header = TRUE, sep=";")
tmax <- read.csv("aemet_tmax_genosto_Ha-cold.csv", header = TRUE, sep=';')
prec <- read.csv("aemet_prec_genosto_Ha-cold.csv", header = TRUE)
file <- 'aemet_genostoso.csv'
xdata.hot <- read.csv("ogimet-temp-2007-08571-Portalegre.csv", header=TRUE)

#creating month and year vectors
month=tmin$MES
year=tmin$year

#formatting data for the cold population
xdata.cold <- aemetformat(tmin=tmin, tmax=tmax, prec=prec, file=file, month=month, year=year)


xdata.cold$pop <- 'cold'
xdata.hot$pop <- 'hot'
head(xdata.cold)
head(xdata.hot)
xdata <- rbind(xdata.cold, xdata.hot)
write.csv(xdata,"Ha_climat-daily.csv", row.names = FALSE)

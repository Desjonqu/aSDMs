rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
source('../../functions/format-aemet.R')

tmin <- read.csv("aemet_tmin_genosto_Ha-cold.csv", header = TRUE, sep=";")
tmax <- read.csv("aemet_tmax_genosto_Ha-cold.csv", header = TRUE, sep=';')
prec <- read.csv("aemet_prec_genosto_Ha-cold.csv", header = TRUE)
file <- 'aemet_genostoso.csv'
xdata.hot <- read.csv("ogimet-temp-2007-08571-Portalegre.csv", header=TRUE)

month=tmin$MES
year=tmin$year

xdata.cold <- aemetformat(tmin=tmin, tmax=tmax, prec=prec, file=file, month=month, year=year)


xdata.cold$pop <- 'cold'
xdata.hot$pop <- 'hot'
head(xdata.cold)
head(xdata.hot)
xdata <- rbind(xdata.cold, xdata.hot)
write.csv(xdata,"Ha_climat-daily.csv", row.names = FALSE)

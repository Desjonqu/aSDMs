rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
# require(raster)
# require(sp)
# require(ggplot2)
# require(lattice)
# require(readr)
# require(maps)
# require(rgeos)
# require(maptools)

load(file='model_boot_ID.Rdata')
load("Ha_monthly.Rdata")
load('../../spatial_dat.Rdata')
load("breadth_Ha.Rdata")
source("../2_suit-corr/200312_breadthfunctions.R")

for (i in 1:2){
# 1=current, 2=future
# extracting climate rasterbricks from list for time loop
if (i==1){
  t <- current
} else {
  t <- future
}

spain_tmin <- t[[1]]
spain_tmax <- t[[2]]
spain_prec <- t[[3]]

# convert climate databases from class rasterbrick to class raterstack to facilitate working by month
spain_tmin <- stack(spain_tmin)
spain_tmax <- stack(spain_tmax)
spain_prec <- stack(spain_prec)
spain_prec2 <- spain_prec+spain_prec[[c(2:12, 1)]]

# I create a map for results: spain_reg, and give 0 values for all cells
spain_reg <- spain_bou <- spain_tmin
spain_reg[] <- spain_bou[] <- 0

prec.pred <- sd(dat$prec)*boot.res$ci.predicted$z.prec+mean(dat$prec)
temp.pred <- sd(dat$tmin)*boot.res$ci.predicted$z.temp+mean(dat$tmin)
phot.pred <- sd(dat$ph.ch)*boot.res$ci.predicted$z.ph.ch+mean(dat$ph.ch)

idx <- which(!is.na(spain_prec[[1]][]))
for (j in 1:12){
  print(j)
  for (k in idx){
    tmin <- spain_tmin[[j]][k]
    tmax <- spain_tmax[[j]][k]
    prec <- spain_prec[[j]][k]
    phmin <- spain_pcmin_c[[j]][k]
    phmax <- spain_pcmax_c[[j]][k]
    spain_bou[[j]][k] <- suit(tmin, tmax, prec, phmin, phmax, res)
    
    a <- abs(tmin-temp.pred)
    b <- abs(prec-prec.pred)
    c <- abs(spain_phch_c[[j]][k]-phot.pred)
    spain_reg[[j]][k] <- boot.res$ci.predicted$fitted[which(a==min(a)&b==min(b)&c==min(c))]
    }
}
spain_reg <- mask(spain_reg, mask=iberia)
spain_bou <- mask(spain_bou, mask=iberia)

names(spain_reg) <- names(spain_bou) <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

reg_mean <- calc(spain_reg, fun = mean)
bou_mean <- calc(spain_bou, fun = mean)
cf <- ifelse(i==1, yes="c", no="f")
writeRaster(reg_mean, filename=paste("reg_mean", cf, ".asc", sep=""), datatype='ascii', overwrite=TRUE)
writeRaster(bou_mean, filename=paste("bou_mean", cf, ".asc", sep=""), datatype='ascii', overwrite=TRUE)
outfile <- save.image(paste("spain_reg", cf, ".Rdata", sep=""))
}

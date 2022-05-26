rm(list=ls())
setwd("./data")
require(raster)

#Load data
load('../../spatial_dat.Rdata')
load('spain_regc.Rdata')
spain_regc <- spain_reg
spain_bouc <- spain_bou
load('spain_regf.Rdata')
spain_regf <- spain_reg
spain_bouf <- spain_bou

UTM_Ib_sp <- read.table("UTM_Ha.csv", sep="\t", dec=".", header=T)
UTM_iberia <- read.table("../../UTM_Geo_grad_reducido.txt", sep="\t", dec=",", header=T)
coord.pres <- UTM_iberia[which(UTM_iberia$UTMCODE%in%UTM_Ib_sp$UTM),4:3]

#initializing dataframe
results_month_spdist <- data.frame(time=numeric(24), month=numeric(24),mean_call=numeric(24),sd_call=numeric(24),min_call=numeric(24),max_call=numeric(24),mean_suit=numeric(24),sd_suit=numeric(24),min_suit=numeric(24),max_suit=numeric(24)) 
results_month_spdist[,1] <- rep(c('current','future'), each=12)
results_month_spdist[,2] <- as.factor(rep(1:12, 2))

spain_regc_month <- spain_regf_month <- spain_bouc_month <- spain_bouf_month <- vector("list",12)

#regression model

for(i in 1:12){
  spain_regc_month[[i]] <- raster::extract(x=spain_regc[[i]], y=coord.pres, method='simple', buffer=5000, fun=mean, na.rm=T) 
  spain_regf_month[[i]] <- raster::extract(x=spain_regf[[i]], y=coord.pres, method='simple', buffer=5000, fun=mean, na.rm=T)
  
  results_month_spdist[i,3] <- mean(spain_regc_month[[i]], na.rm=T)
  results_month_spdist[i,4] <- sd(spain_regc_month[[i]], na.rm=T)
  results_month_spdist[i,5] <- min(spain_regc_month[[i]], na.rm=T)
  results_month_spdist[i,6] <- max(spain_regc_month[[i]], na.rm=T)
  
  results_month_spdist[i+12,3] <- mean(spain_regf_month[[i]], na.rm=T)
  results_month_spdist[i+12,4] <- sd(spain_regf_month[[i]], na.rm=T)
  results_month_spdist[i+12,5] <- min(spain_regf_month[[i]], na.rm=T)
  results_month_spdist[i+12,6] <- max(spain_regf_month[[i]], na.rm=T)
}


#Boundary model
for(i in 1:12){
  spain_bouc_month[[i]] <- raster::extract(x=spain_bouc[[i]], y=coord.pres, method='simple', buffer=5000, fun=mean, na.rm=T) 
  spain_bouf_month[[i]] <- raster::extract(x=spain_bouf[[i]], y=coord.pres, method='simple', buffer=5000, fun=mean, na.rm=T)
  
  results_month_spdist[i,7] <- mean(spain_bouc_month[[i]], na.rm=T)
  results_month_spdist[i,8] <- sd(spain_bouc_month[[i]], na.rm=T)
  results_month_spdist[i,9] <- min(spain_bouc_month[[i]], na.rm=T)
  results_month_spdist[i,10] <- max(spain_bouc_month[[i]], na.rm=T)
  
  results_month_spdist[i+12,7] <- mean(spain_bouf_month[[i]], na.rm=T)
  results_month_spdist[i+12,8] <- sd(spain_bouf_month[[i]], na.rm=T)
  results_month_spdist[i+12,9] <- min(spain_bouf_month[[i]], na.rm=T)
  results_month_spdist[i+12,10] <- max(spain_bouf_month[[i]], na.rm=T)
}

save(results_month_spdist, file='Ha_clockdata.Rdata')

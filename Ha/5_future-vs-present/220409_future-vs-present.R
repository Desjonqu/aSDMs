rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
require(raster)
library(tidyr)
require(rgeos)
require(maptools)
# require(sp)
library(ggplot2)
# require(lattice)
# require(readr)
# require(maps)
# library(ROCR)
# library(mapview)
# library(dismo)
# library(RColorBrewer)

inter <- readShapePoly("Ha_presence-polygon.shp")
load('Ha_clockdata.Rdata')
load('Ha_monthly.Rdata')
load('../../spatial_dat.Rdata')
load('spain_regc.Rdata')
spain_regc <- spain_reg
spain_bouc <- spain_bou
load('spain_regf.Rdata')
spain_regf <- spain_reg
spain_bouf <- spain_bou

spain_regs <- spain_regf-spain_regc
spain_bous <- spain_bouf-spain_bouc
names(spain_regs) <- names(spain_bous) <- names(spain_regc)

UTM_Ib_sp <- read.table("UTM_Ha.csv", sep="\t", dec=".", header=T)
UTM_iberia <- read.table("../../UTM_Geo_grad_reducido.txt", sep="\t", dec=",", header=T)
RES <- 400
inter <- fortify(inter)

#Regression model
reg_shift <- stack(spain_regs)
datos <- as.data.frame(reg_shift, xy = TRUE)

datosv <- gather(datos, c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), key="month", value = "suitability")
datosv$month <- factor(datosv$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
lim <- max(abs(datosv$suitability), na.rm=TRUE)

reg_mapplot <- ggplot() + geom_raster(data = datosv, aes(x=x, y=y, fill = suitability)) + coord_quickmap(xlim = c(-9, 4), ylim = c(36, 44)) + borders("world", size=0.2, colour = "lightgray") + theme(legend.title = element_text(size=7), axis.title = element_blank(), axis.text = element_text(size=7), strip.background = element_rect(fill="white")) + facet_wrap(~ month, nrow=3, ncol=4)+  geom_polygon(data=inter, aes(long, lat, group = group), colour = "black", fill = NA, size=0.25)+ scale_fill_gradient2(low="#a6611a", mid="#f5f5f5", high="#018571", midpoint=0, na.value="lightgray", name='shift in\ncalling\nprobability')+scale_x_continuous(breaks=c(-8,-6,-4,-2,0,2))

tiff("Ha_Fig4_regmod.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
reg_mapplot
dev.off()


#Boundary model
reg_shift <- stack(spain_bous)
datos <- as.data.frame(reg_shift, xy = TRUE)

datosv <- gather(datos, c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), key="month", value = "suitability")
datosv$month <- factor(datosv$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
lim <- max(abs(datosv$suitability), na.rm=TRUE)


bou_mapplot <- ggplot() + geom_raster(data = datosv, aes(x=x, y=y, fill = suitability)) + coord_quickmap(xlim = c(-9, 4), ylim = c(36, 44)) + borders("world", size=0.2, colour = "lightgray") + theme(legend.title = element_text(size=7),axis.title = element_blank(),axis.text = element_text(size=7), strip.background = element_rect(fill="white")) + facet_wrap(~ month, nrow=3, ncol=4)+  geom_polygon(data=inter, aes(long, lat, group = group), colour = "black", fill = NA, size=0.25)+ scale_fill_gradient2(low="#a6611a", mid="#f5f5f5", high="#018571", midpoint=0, na.value="lightgray", name='shift in\ncalling\nprobability')+scale_x_continuous(breaks=c(-8,-6,-4,-2,0,2))

tiff("Ha_Fig4_boumod.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
bou_mapplot
dev.off()

#Regression model
reg_c <- stack(spain_regc)
datos <- as.data.frame(reg_c, xy = TRUE)

datosv <- gather(datos, c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), key="month", value = "suitability")
datosv$month <- factor(datosv$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

reg_mapplot <- ggplot() + geom_raster(data = datosv, aes(x=x, y=y, fill = suitability)) + coord_quickmap(xlim = c(-9, 4), ylim = c(36, 44))+ scale_fill_viridis_c(option="D", name='calling\nprobability') + borders("world") + theme(legend.title = element_text(size=7),axis.title = element_blank()) + facet_wrap(~ month, nrow=3, ncol=4)+  geom_polygon(data=inter, aes(long, lat, group = group), colour = "black", fill = NA, size=0.25)


tiff("Ha_regmod-current.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
reg_mapplot
dev.off()


#Regression boundary model
reg_c <- stack(spain_bouc)
datos <- as.data.frame(reg_c, xy = TRUE)

datosv <- gather(datos, c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec), key="month", value = "suitability")
datosv$month <- factor(datosv$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

bou_mapplot <- ggplot() + geom_raster(data = datosv, aes(x=x, y=y, fill = suitability)) + coord_quickmap(xlim = c(-9, 4), ylim = c(36, 44))+ scale_fill_viridis_c(option="D", name='calling\nprobability') + borders("world") + theme(legend.title = element_text(size=7),axis.title = element_blank()) + facet_wrap(~ month, nrow=3, ncol=4)+  geom_polygon(data=inter, aes(long, lat, group = group), colour = "black", fill = NA, size=0.25)


tiff("Ha_boumod-current.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
bou_mapplot
dev.off()


#Polar plot regression model
col <- ifelse(dat$pop[which(dat$calling.freq!=0)]=='cold', yes='blue', no='orange')
mult <- ifelse(dat$pop[which(dat$calling.freq!=0)]=='cold', yes=1.05, no=1.1)
size <- 3*dat$calling.freq[which(dat$calling.freq!=0)]/max(dat$calling.freq[which(dat$calling.freq!=0)])
size[size<0.5] <- 0.5
realcalling <- data.frame(month=as.factor(as.numeric(dat$month[which(dat$calling.freq!=0)])), pop=dat$pop[which(dat$calling.freq!=0)], mean_call=max(c(results_month_spdist$max_call, results_month_spdist$mean_call+results_month_spdist$sd_call))*mult, mean_suit=max(c(results_month_spdist$max_suit, results_month_spdist$mean_suit+results_month_spdist$sd_suit))*mult, col=col)

clock_month_reg <- ggplot(results_month_spdist, aes(x=month, y=mean_call, fill=time, colour=time))+geom_polygon(aes(group=time, colour=time, fill=time), alpha=0.1, position=position_dodge(.1))+coord_polar(start = - pi/12)+geom_point(position=position_dodge(.1)) + geom_errorbar(aes(ymin=mean_call-sd_call, ymax=mean_call+sd_call), width=.2,position=position_dodge(.1)) +labs(y = "Calling suitability", x = "Month")+ theme_bw()+ geom_point(data = realcalling, fill=col, colour=col, size=size)
#geom_hline(yintercept=studysite, linetype='dashed', colour='black')
save(clock_month_reg, file='Ha_clock.RData')

tiff("Ha_Fig5_regmod.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
clock_month_reg
dev.off()



#Polar plot boundary model
clock_month_bou <- ggplot(results_month_spdist, aes(x=month, y=mean_suit, fill=time, colour=time))+geom_polygon(aes(group=time, colour=time, fill=time), alpha=0.1, position=position_dodge(.1))+coord_polar(start = - pi/12)+geom_point(position=position_dodge(.1)) + geom_errorbar(aes(ymin=mean_suit-sd_suit, ymax=mean_suit+sd_suit), width=.2,position=position_dodge(.1)) +labs(y = "Calling suitability", x = "Month")+ theme_bw()+ geom_point(data = realcalling, fill=col, colour=col, size=size)
#geom_hline(yintercept=studysite, linetype='dashed', colour='black')

tiff("Ha_Fig5_boumod.tif", width=16/2.54*RES, height=12/2.54*RES, res=RES, compression = "lzw")
clock_month_bou
dev.off()



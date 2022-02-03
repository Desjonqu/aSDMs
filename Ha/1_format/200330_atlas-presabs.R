rm(list=ls())
setwd("/Users/cdesjonq/Documents/post_docs/madrid/projet_brazil-FG/Iberian-peninsula/Ha/data")
require(rgeos)
require(raster)
library(rgdal)

load('../../spatial_dat.Rdata')

# UTMs Iberia
UTM_iberia <- read.table("../../UTM_Geo_grad_reducido.txt", sep="\t", dec=",", header=T)

# UTMs of spp distribution
# *Ha
UTM_Ib_sp <- read.table("UTM_Ha.csv", sep="\t", dec=".", header=T)

coord.pres <- UTM_iberia[which(UTM_iberia$UTMCODE%in%UTM_Ib_sp$UTM),4:3]
coord.abs <- UTM_iberia[which(!UTM_iberia$UTMCODE%in%UTM_Ib_sp$UTM),4:3]


spain_tmin <- current[[1]]
spain_tmin <- stack(spain_tmin)
coord.na <- calc(spain_tmin[[1]], is.na)
spain_tmin[]=NA
presabs <- spain_tmin[[1]]

cell <- cellFromXY(presabs, coord.pres)
cell.abs <- cellFromXY(presabs, coord.abs)
presabs[cell.abs] <- 0
presabs[cell] <- 1
presabs[coord.na==1] <- NA


#### Create polygon of the species distribution (BUFFER around the presence points)
UTM_sp <- SpatialPointsDataFrame(coords = coord.pres, data = coord.pres, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

UTM_sp_flat <- spTransform(UTM_sp, CRS("+init=epsg:2062"))
buf1 <- gBuffer(UTM_sp_flat, byid=TRUE, width=30000, capStyle='square')
buf2 <- gUnaryUnion(buf1)
buffer <- spTransform(buf2, CRS(" +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
inter <- intersect(iberia, buffer)
inter <- gUnaryUnion(inter)
inter <- as(inter, "SpatialPolygonsDataFrame")

writeRaster(presabs, filename="presabs.grd", datatype='INT4S', overwrite=TRUE)
writeOGR(inter, ".", "Ha_presence-polygon", driver="ESRI Shapefile")



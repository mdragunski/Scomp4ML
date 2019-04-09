############################################################################################################
####################################### READ DATA ##########################################################
############################################################################################################

rm(list=ls())
library(raster)
library(mapview)
#library(devtools)
#install_github("HannaMeyer/MSGtools")
library(MSGtools)
setwd("/home/marc/Uni-stuff/Master/Masterarbeit Marc/data")
MSGpath <- paste0(getwd(),"/MSGProj/2010/05/06/13/")
# read MSG SEVIRI channels vom 24.05.2010 um 12 Uhr
MSG <- getChannels(MSGpath,type="tif")
#read RADOLAN
RADOLAN <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/data/RadarProj/2010/05/06/13/201005061350_raa01_rw.tif")
# mask radolan so that it only contains clouded pixels
RADOLAN <- mask(RADOLAN,MSG[[1]])
#set NA value correctly
RADOLAN <- reclassify(RADOLAN,c(-999,-0.01,NA))
#see location of the data
mapview(MSG)
mapview(RADOLAN)
#######################
#für NA
#lin <- rasterToContour(is.na(RADOLAN))
lin <- rasterToContour(RADOLAN)

pol <- as(st_union(st_polygonize(st_as_sf(lin))), 'Spatial') # st_union to dissolve geometries

pts <- spsample(pol[1,], 2000, type = 'random')
pts3000 <- spsample(pol[1,], 3000, type = 'random')
pts1000 <- spsample(pol[1,], 1000, type = 'random')
pts500 <- spsample(pol[1,], 500, type = 'random')

plot(lin)
plot(pts, add = T, col = 'red')

###
g <- as(RADOLAN, 'SpatialGridDataFrame')
p <- as(RADOLAN, 'SpatialPixels')

ptsG <- spsample(g, 2000, type = 'random')
plot(g)
plot(ptsG, add = T, col = 'red')


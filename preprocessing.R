rm(list=ls())
library(raster)
library(mapview)
library(devtools)
install_github("HannaMeyer/MSGtools")
library(MSGtools)
#setwd("H:/Masterarbeit/data")
setwd("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data")
##########################################################################################################
#                                 Read SRTM data
#########################################################################################################
#SRTM37_02 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_37_02/srtm_37_02.tif")
#SRTM37_03 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_37_03/srtm_37_03.tif")

#SRTM38_02 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_38_02/srtm_38_02.tif")
#SRTM38_03 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_38_03/srtm_38_03.tif")
#SRTM39_02 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_39_02/srtm_39_02.tif")
#SRTM39_03 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_39_03/srtm_39_03.tif")

#SRTM40_02 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_40_02/srtm_40_02.tif")
#SRTM40_03 <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/srtm_40_03/srtm_40_03.tif")

#########################################################################################################
#                                 Create SRTM mosaic
#########################################################################################################
#SRTM_mosaic <- mosaic(SRTM37_02, SRTM37_03, SRTM38_02, SRTM38_03, SRTM39_02, SRTM39_03, SRTM40_02, SRTM40_03, fun=max) #max
#writeRaster(SRTM_mosaic, filename = "SRTM_mosaic_basic.tif", overwrite=TRUE)

#########################################################################################################
#                                 Read MSG & RADOLAN data
#########################################################################################################
MSGpath <- paste0(getwd(),"/MSGProj/2010/05/06/13/")
# read MSG SEVIRI channels vom 24.05.2010 um 12 Uhr
MSG <- getChannels(MSGpath,type="tif")
#read RADOLAN
RADOLAN <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/RadarProj/2010/05/06/13/201005061350_raa01_rw.tif")
#RADOLAN <- raster("H:/Masterarbeit/data/RadarProj/2010/05/06/13/201005061350_raa01_rw.tif")
# mask radolan so that it only contains clouded pixels
RADOLAN <- mask(RADOLAN,MSG[[1]])
#set NA value correctly
RADOLAN <- reclassify(RADOLAN,c(-999,-0.01,NA))
#see location of the data
#mapview(MSG)
#mapview(RADOLAN)

########################################################################################################
#                                 crop and reference SRTM data
########################################################################################################
#SRTM_basic <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/SRTM_mosaic_basic.tif")
#SRTM_basic_crs <- projectRaster(SRTM_basic, RADOLAN, filename = "SRTM_basic_crs.tif", overwrite = TRUE)
#crop <- crop(SRTM_basic_crs, RADOLAN)
#writeRaster(crop, filename = "SRTM_basic_crs_final.tif", overwrite=TRUE)
#SRTM_basic_crs_final <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/SRTM_basic_crs_final.tif")
#SRTM_final <- resample(crop, RADOLAN, method="bilinear")
#writeRaster(SRTM_final, filename = "SRTM_final.tif", overwrite=TRUE)

#######################################################################################################
#                                 create aspect raster
#######################################################################################################
#SRTM_final <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/SRTM_final.tif")
#aspect_layer <- terrain(SRTM_final, opt=c('aspect'))

#######################################################################################################
#                                 create raster brick product
#######################################################################################################
#c_r <- addLayer(MSG, RADOLAN, SRTM_final, aspect_layer)
#x <- init(c_r, 'x')#longitude 
#y <- init(c_r, 'y')#latitude
#c_r_x_y <- addLayer(c_r, x, y)
#names(c_r_x_y) <- c("VIS0.6", "VIS0.8", "NIR1.6", "IR3.9", "WV6.2", "WV7.3", "IR8.7", "IR9.7", "IR10.8", "IR12.0", "IR13.4", "RADOLAN", "SRTM", "ASPECT", "x", "y")
#writeRaster(c_r_x_y, filename = "merged_raster.grd", options="INTERLEAVE=BAND", overwrite=TRUE)
merged_raster <- stack("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/merged_raster.grd")
#merged_raster <- stack("H:/Masterarbeit/data/merged_raster.grd")
#######################################################################################################
#                                 create training sample
#######################################################################################################
#mask4SRTM <- mask(RADOLAN, aspect_layer)
#pointsFromHere <- rasterToPolygons(mask4SRTM)
#save(pointsFromHere, file = "/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/pointsFromHere.Rdata")
load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/pointsFromHere.Rdata")
#load("H:/Masterarbeit/data/pointsFromHere.Rdata")

#random point locations aus dem sp paket / Base case
set.seed(28)
points_random <- spsample(pointsFromHere, 1000, type = 'random')

extract_training_points_random <- extract(merged_raster, points_random)
training_set_random <- as.data.frame(extract_training_points_random)
save(training_set_random, file = "H:/Masterarbeit/data/model_data/training_set_random.Rdata")

#regular point locations
set.seed(28)
points_regular <- spsample(pointsFromHere, 1000, type = 'regular')

extract_training_points_regular <- extract(merged_raster, points_regular)
training_set_regular <- as.data.frame(extract_training_points_regular)
save(training_set_regular, file = "H:/Masterarbeit/data/model_data/training_set_regular.Rdata")

#clustered points 1000x10
set.seed(28)
points_clustered_1000_10 <- spsample(pointsFromHere, 1000, type = 'clustered', nclusters = 10)

extract_training_points_clustered_1000_10 <- extract(merged_raster, points_clustered_1000_10)
training_set_clustered_1000_10 <- as.data.frame(extract_training_points_clustered_1000_10)
save(training_set_clustered_1000_10, file = "H:/Masterarbeit/data/model_data/training_set_clustered_1000_10.Rdata")

#clustered points 1000x20 sieht sehr gut aus vermutlich nehmen als cluster beispiel.
set.seed(28)
points_clustered_1000_20 <- spsample(pointsFromHere, 1000, type = 'clustered', nclusters = 20)

extract_training_points_clustered_1000_20 <- extract(merged_raster, points_clustered_1000_20)
training_set_clustered_1000_20 <- as.data.frame(extract_training_points_clustered_1000_20)
save(training_set_clustered_1000_20, file = "H:/Masterarbeit/data/model_data/training_set_clustered_1000_20.Rdata")


#plot(RADOLAN)
###plot(gridlines(points_clustered), add = T, col = grey(.8))
#plot(points_clustered_1000_20, add = T, col = 'red')

######################################################################################################
#                                 Run models w/o spatial dependencies
######################################################################################################
install.packages("caret")
library(doParallel)
library(caret)
library(dplyr)
random <- select(training_set_random, -c(x,y))
regular <- select(training_set_regular, -c(x,y))
cluster_10 <- select(training_set_clustered_1000_10, -c(x,y))
cluster_20 <- select(training_set_clustered_1000_20, -c(x,y))

ncores <- 3
cl <- makeCluster(ncores)
registerDoParallel(ncores)
#getDoParWorkers() # Just checking, how many workers you have
ctrl = trainControl(method="cv", number = 10,returnResamp = "all",savePredictions = TRUE, allowParallel = TRUE)#, index = #createFolds()result)
tGrid = data.frame(mtry= c(2:5))

####base model (10-fold cv)
set.seed(28)
system.time(fit_rf_wo_spatial_cluster_20 <- train(RADOLAN ~ .,
                data = cluster_20,
                method = "rf",
                trControl = ctrl,
                tuneGrid = tGrid,
                ntree = 1000,
                metric = "RMSE",
                maximize = FALSE,
                importance = TRUE
)
)
stopCluster(cl)
save(fit_rf_wo_spatial_cluster_20, file = "/media/marc/ADATA HV100/Masterarbeit/data/results_wo_spatial/cluster_20.Rdata")
#load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_RFmodel.Rdata")

######
#base model (10-fold cv) with regular point locations
#####
cl <- makeCluster(ncores)
registerDoParallel(ncores)
set.seed(28)
system.time(fit_rf_regular <- train(RADOLAN ~ .,
                            data = training_set_regular,
                            method = "rf",
                            trControl = ctrl,
                            tuneGrid = tGrid,
                            ntree = 1000,
                            metric = "RMSE",
                            maximize = FALSE,
                            importance = TRUE
)
)
stopCluster(cl)
save(fit_rf_regular, file = "/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_regular_RFmodel.Rdata")
load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_regular_RFmodel.Rdata")


######
#base model (10-fold cv) with clustered_1000_10 point locations
#####
cl <- makeCluster(ncores)
registerDoParallel(ncores)
set.seed(28)
system.time(fit_rf_clustered_1000_10 <- train(RADOLAN ~ .,
                                            data = training_set_clustered_1000_10,
                                            method = "rf",
                                            trControl = ctrl,
                                            tuneGrid = tGrid,
                                            ntree = 1000,
                                            metric = "RMSE",
                                            maximize = FALSE,
                                            importance = TRUE
)
)
stopCluster(cl)
save(fit_rf_clustered_1000_10, file = "/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_1000_10_RFmodel.Rdata")
load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_1000_10_RFmodel.Rdata")
load("H:/Masterarbeit/data/basic_10cv_clustered_1000_10_RFmodel.Rdata")

######
#base model (10-fold cv) with clustered_1000_20 point locations
#####
cl <- makeCluster(ncores)
registerDoParallel(ncores)
set.seed(28)
system.time(fit_rf_clustered_1000_20 <- train(RADOLAN ~ .,
                                              data = training_set_clustered_1000_20,
                                              method = "rf",
                                              trControl = ctrl,
                                              tuneGrid = tGrid,
                                              ntree = 1000,
                                              metric = "RMSE",
                                              maximize = FALSE,
                                              importance = TRUE
)
)
stopCluster(cl)
save(fit_rf_clustered_1000_20, file = "/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_1000_20_RFmodel.Rdata")
load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_1000_20_RFmodel.Rdata")
load("H:/Masterarbeit/data/basic_10cv_clustered_1000_20_RFmodel.Rdata")

######
#base model (10-fold cv) with clustered_1000_20 point locations csample approach
#####
cl <- makeCluster(ncores)
registerDoParallel(ncores)
set.seed(28)
system.time(fit_rf_clustered_spwrap_1000_20 <- train(RADOLAN ~ .,
                                              data = training_set_clustered_spwrap_1000_20,
                                              method = "rf",
                                              trControl = ctrl,
                                              tuneGrid = tGrid,
                                              ntree = 1000,
                                              metric = "RMSE",
                                              maximize = FALSE,
                                              importance = TRUE
)
)
stopCluster(cl)
save(fit_rf_clustered_spwrap_1000_20, file = "/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_spwrap_1000_20_RFmodel.Rdata")
load("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/basic_10cv_clustered_spwrap_1000_20_RFmodel.Rdata")


###################################################################################################
#                       make predictions
###################################################################################################
pred_fit_rf <- predict(merged_raster, model=fit_rf, na.rm=TRUE)
pred_fit_rf_regular <- predict(merged_raster, model=fit_rf_regular, na.rm=TRUE)
pred_fit_rf_clustered_1000_10 <- predict(merged_raster, model=fit_rf_clustered_1000_10, na.rm=TRUE)
pred_fit_rf_clustered_1000_20 <- predict(merged_raster, model=fit_rf_clustered_1000_20, na.rm=TRUE)
pred_fit_rf_clustered_spwrap_1000_20 <- predict(merged_raster, model=fit_rf_clustered_spwrap_1000_20, na.rm=TRUE)

##show variance Importance # https://explained.ai/rf-importance/index.html
#plot(varImp(fit_rf,type=1,scale=F))

#viewMAP <-  mapview(RADOLAN)+mapview(pred_fit_rf)+mapview(pred_fit_rf_regular)+
#            mapview(pred_fit_rf_clustered_1000_10)

#mapviewspwrap <- mapview(RADOLAN)+mapview(pred_fit_rf_clustered_1000_20)+mapview(pred_fit_rf_clustered_spwrap_1000_20)


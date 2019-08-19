# Clear workspace
rm(list=ls(all=TRUE))

## SET WORKING DIRECTORY
setwd("H:/Masterarbeit/data/model_data")

install.packages("caret")

#load libraries
library(doParallel)
library(caret)
library(CAST)


#lapply(list.files(),load,.GlobalEnv)
number_of_files <- list.files()
#length(number_of_files)

#getDoParWorkers() # Just checking, how many workers you have
ctrl = trainControl(method="cv", number = 10,returnResamp = "all",savePredictions = TRUE, allowParallel = TRUE)#, index = #createFolds()result)
tGrid = data.frame(mtry= c(2:5))

####base model (10-fold cv)
run_rf <- function(ctrl, tGrid, data) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
  load(number_of_files[i],.GlobalEnv)
  
  train_data <-get(gsub(".Rdata", "", data))
  fit_rf <- train(RADOLAN ~ .,
                  data = train_data,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = tGrid,
                  ntree = 1000,
                  metric = "RMSE",
                  maximize = FALSE,
                  importance = TRUE
  )
  
  stopCluster(cl)

  save(fit_rf, file = paste("H:/Masterarbeit/data/results/base_case_", data,sep=""))

}
###RUN MODEL BASE CASE
system.time(for (i in 1:length(number_of_files)){

  run_rf(ctrl, tGrid, number_of_files[i])

  }
)


###RUN MODEL BASE CASE FFS
predictors <- c("VIS0.6", "VIS0.8", "NIR1.6", "IR3.9", "WV6.2", "WV7.3", "IR8.7", "IR9.7", "IR10.8", "IR12.0", "IR13.4", "SRTM", "ASPECT", "x", "y")
run_rf_ffs <- function(ctrl, tGrid, data) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
  load(number_of_files[i],.GlobalEnv)
  
  train_data <-get(gsub(".Rdata", "", data))
  
  fit_ffs_rf <- ffs(train_data[,predictors],
                    train_data$RADOLAN,
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = tGrid,
                    ntree = 1000,
                    metric = "RMSE",
                    maximize = FALSE,
                    importance = TRUE
  )
  stopCluster(cl)
  
  save(fit_ffs_rf, file = paste("H:/Masterarbeit/data/results/base_case_ffs_", data,sep=""))

}

system.time(for (i in 1:length(number_of_files)){
  
  run_rf_ffs(ctrl, tGrid, number_of_files[i])
  
}
)



###RUN MODEL BASE CASE RFE
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10, saveDetails = TRUE,returnResamp = "all", allowParallel = TRUE)
tGrid = data.frame(mtry= c(2:5))
# run the RFE algorithm
run_rf_rfe <- function(ctrl, tGrid, data) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
  load(number_of_files[i],.GlobalEnv)
  
  train_data <-get(gsub(".Rdata", "", data))
  
  fit_rfe_rf <- rfe(RADOLAN ~.,
                   data = train_data, 
                   sizes=c(1:15),
                   ntree=1000,
                   rfeControl=control)
  
  stopCluster(cl)
  
  save(fit_rfe_rf, file = paste("H:/Masterarbeit/data/results/base_case_rfe_", data,sep=""))
  
}

system.time(for (i in 1:length(number_of_files)){
  
  run_rf_rfe(ctrl, tGrid, number_of_files[i])
  
}
)

#Create SPATIAL BLOCKS
#read RADOLAN
#RADOLAN <- raster("/home/marc/Uni-stuff/Master/Masterarbeit Marc/Scomp4ML/data/RadarProj/2010/05/06/13/201005061350_raa01_rw.tif")
RADOLAN <- raster("H:/Masterarbeit/data/RadarProj/2010/05/06/13/201005061350_raa01_rw.tif")
# mask radolan so that it only contains clouded pixels
RADOLAN <- mask(RADOLAN,MSG[[1]])
#set NA value correctly
RADOLAN <- reclassify(RADOLAN,c(-999,-0.01,NA))

#install_github("rvalavi/blockCV")
library(blockCV)

run_rf_sb_random <- function(data, RADOLAN, typeof, row, col, k, sep) {
  
  set.seed(28)
  load(number_of_files[i],.GlobalEnv)
  
  train_data <-get(gsub(".Rdata", "", data))
  points <- SpatialPointsDataFrame(train_data[,c("x","y")], train_data, proj4string = crs(RADOLAN))
  
  sb_random <- spatialBlock(speciesData = points,
                            species = NULL,
                            rasterLayer = RADOLAN,
                            rows = row,
                            cols = col,
                            k = k,
                            selection = typeof)
  
  save(sb_random, file = paste("H:/Masterarbeit/data/spatial_blocks/sb_",typeof,"_",sep,"_", data,sep=""))
  
}

system.time(for (i in 1:length(number_of_files)){
  
  run_rf_sb_random(number_of_files[i], RADOLAN, "random", 5, 5, 10, "random")
  run_rf_sb_random(number_of_files[i], RADOLAN, "checkerboard", 5, 5, 10, "checkerboard")
  run_rf_sb_random(number_of_files[i], RADOLAN, "systematic", 0, 6, 3, "columns")
  run_rf_sb_random(number_of_files[i], RADOLAN, "systematic", 6, 0, 3, "rows")
  
}
)



####################################################################################################
#                               spatial validation RF runs
####################################################################################################
library(CAST)
library(sp)

########################spatial block
run_rf_sb <- function(data, train) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
 

  train_data <-get(data)
  
  foldID <- train_data$foldID
  foldyDF <- as.data.frame(foldID)
  class(train_data)
  training_data_sb <- cbind(train, foldyDF)
  
  ctrl_index <- CreateSpacetimeFolds(x = training_data_sb, spacevar = "foldID", k = 10)
  
  ctrl = trainControl(method="cv", returnResamp = "final",savePredictions = TRUE, allowParallel = TRUE, index = ctrl_index$index)#, index = #createFolds()result)
  tGrid = data.frame(mtry= c(2:5))
  
  fit_rf_sb <- train(RADOLAN ~ .,
                  data = train,
                  method = "rf",
                  trControl = ctrl,
                  tuneGrid = tGrid,
                  ntree = 1000,
                  metric = "RMSE",
                  maximize = FALSE,
                  importance = TRUE
  )

  stopCluster(cl)

  save(fit_rf_sb, file = paste("H:/Masterarbeit/data/results_spatial_blocks/spatial_blocks_case_", data,sep=""))

}

setwd("H:/Masterarbeit/data/model_data")
number_of_files <- list.files()

system.time(for (i in 1:length(number_of_files)){
  
  load(number_of_files[i],.GlobalEnv)
  
}
)

#load spatial blocks and run rf 1000_10
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_10")
number_of_files_sp_clustered_1000_10 <- list.files()

system.time(for (i in 1:length(number_of_files_sp_clustered_1000_10)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_clustered_1000_10[i])
  
  load(number_of_files_sp_clustered_1000_10[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_clustered_1000_10[i], d)
  run_rf_sb(number_of_files_sp_clustered_1000_10[i], train)
  
  }
)

#load spatial blocks and run rf 1000_20
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_20")
number_of_files_sp_clustered_1000_20 <- list.files()

system.time(for (i in 1:length(number_of_files_sp_clustered_1000_20)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_clustered_1000_20[i])
  
  load(number_of_files_sp_clustered_1000_20[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_clustered_1000_20[i], d)
  run_rf_sb(number_of_files_sp_clustered_1000_20[i], train)
  
}
)

#load spatial blocks and run rf regular
setwd("H:/Masterarbeit/data/spatial_blocks_regular")
number_of_files_sp_regular <- list.files()

system.time(for (i in 1:length(number_of_files_sp_regular)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_regular[i])
  
  load(number_of_files_sp_regular[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_regular[i], d)
  run_rf_sb(number_of_files_sp_regular[i], train)
  
}
)


#load spatial blocks and run rf random
setwd("H:/Masterarbeit/data/spatial_blocks_random")
number_of_files_sp_random <- list.files()

system.time(for (i in 1:length(number_of_files_sp_random)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_random[i])
  
  load(number_of_files_sp_random[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_random[i], d)
  run_rf_sb(number_of_files_sp_random[i], train)
  
}
)


########################ffs spatial block

predictors <- c("VIS0.6", "VIS0.8", "NIR1.6", "IR3.9", "WV6.2", "WV7.3", "IR8.7", "IR9.7", "IR10.8", "IR12.0", "IR13.4", "SRTM", "ASPECT", "x", "y")

run_rf_sb_ffs <- function(data, train) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
  
  train_data <-get(data)
  
  foldID <- train_data$foldID
  foldyDF <- as.data.frame(foldID)

  training_data_sb <- cbind(train, foldyDF)
  
  ctrl_index <- CreateSpacetimeFolds(x = training_data_sb, spacevar = "foldID", k = 10)
  
  ctrl = trainControl(method="cv", returnResamp = "final",savePredictions = TRUE, allowParallel = TRUE,index = ctrl_index$index)
  tGrid = data.frame(mtry= c(2:5))
  
  fit_sb_ffs_rf <- ffs(train[,predictors],
                    train$RADOLAN,
                    method = "rf",
                    trControl = ctrl,
                    tuneGrid = tGrid,
                    ntree = 1000,
                    metric = "RMSE",
                    maximize = FALSE,
                    importance = TRUE
  )
  stopCluster(cl)
  
  save(fit_sb_ffs_rf, file = paste("H:/Masterarbeit/data/results_spatial_blocks_ffs/spatial_blocks_case_ffs_", data,sep=""))
  
}

#load spatial blocks and run rf 1000_10
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_10")
number_of_files_sp_clustered_1000_10 <- list.files()

system.time(for (i in 1:length(number_of_files_sp_clustered_1000_10)){

  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_clustered_1000_10[i])

  load(number_of_files_sp_clustered_1000_10[i],.GlobalEnv)
  #  d <- sb_random
  #assign(number_of_files_sp_clustered_1000_10[i], d)
  # run_rf_sb_ffs(number_of_files_sp_clustered_1000_10[i], train)

  }
)

#load spatial blocks and run rf 1000_20
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_20")
number_of_files_sp_clustered_1000_20 <- list.files()

system.time(for (i in 1:length(number_of_files_sp_clustered_1000_20)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_clustered_1000_20[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_clustered_1000_20[i], d)
  run_rf_sb_ffs(number_of_files_sp_clustered_1000_20[i], train)
  
}
)

#load spatial blocks and run rf random
setwd("H:/Masterarbeit/data/spatial_blocks_random")
number_of_files_sp_random <- list.files()

system.time(for (i in 1:length(number_of_files_sp_random)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_random[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_random[i], d)
  run_rf_sb_ffs(number_of_files_sp_random[i], train)
  
}
)

#load spatial blocks and run rf regular
setwd("H:/Masterarbeit/data/spatial_blocks_regular")
number_of_files_sp_regular <- list.files()

system.time(for (i in 1:length(number_of_files_sp_regular)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_regular[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_regular[i], d)
  run_rf_sb_ffs(number_of_files_sp_regular[i], train)
  
}
)


########################rfe spatial block############################

#load spatial blocks and run rf 1000_10
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_10")
number_of_files_sp_clustered_1000_10 <- list.files()


run_rf_sb_rfe <- function(data, train) {
  set.seed(28)
  ncores <- 3
  cl <- makeCluster(ncores)
  registerDoParallel(ncores)
  
  train_data <-get(data)
  
  foldID <- train_data$foldID
  foldyDF <- as.data.frame(foldID)
  
  training_data_sb <- cbind(train, foldyDF)
  
  ctrl_index <- CreateSpacetimeFolds(x = training_data_sb, spacevar = "foldID", k = 10)
  
  ctrl <- rfeControl(functions=rfFuncs, method="cv", saveDetails = TRUE,returnResamp = "all", allowParallel = TRUE,index = ctrl_index$index)
  tGrid = data.frame(mtry= c(2:5))
  
  
  fit_sb_rfe_rf <- rfe(RADOLAN ~.,
                    data = train, 
                    sizes=c(1:15),
                    ntree=1000,
                    rfeControl=ctrl)
  
  stopCluster(cl)
  
  save(fit_sb_rfe_rf, file = paste("H:/Masterarbeit/data/results_spatial_block_rfe/spatial_blocks_case_rfe_", data,sep=""))
  
}


system.time(for (i in 1:length(number_of_files_sp_clustered_1000_10)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_10[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  print(number_of_files_sp_clustered_1000_10[i])
  
  load(number_of_files_sp_clustered_1000_10[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_clustered_1000_10[i], d)
  run_rf_sb_rfe(number_of_files_sp_clustered_1000_10[i], train)
  
}
)

#load spatial blocks and run rf 1000_20
setwd("H:/Masterarbeit/data/spatial_blocks_clustered_1000_20")
number_of_files_sp_clustered_1000_20 <- list.files()

system.time(for (i in 1:length(number_of_files_sp_clustered_1000_20)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_clustered_1000_20[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_clustered_1000_20[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_clustered_1000_20[i], d)
  run_rf_sb_rfe(number_of_files_sp_clustered_1000_20[i], train)
  
}
)

#load spatial blocks and run rf random
setwd("H:/Masterarbeit/data/spatial_blocks_random")
number_of_files_sp_random <- list.files()

system.time(for (i in 1:length(number_of_files_sp_random)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_random[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_random[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_random[i], d)
  run_rf_sb_rfe(number_of_files_sp_random[i], train)
  
}
)

#load spatial blocks and run rf regular
setwd("H:/Masterarbeit/data/spatial_blocks_regular")
number_of_files_sp_regular <- list.files()

system.time(for (i in 1:length(number_of_files_sp_regular)){
  
  if ( grepl("_clustered_1000_10", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_10
  } else if ( grepl("_clustered_1000_20", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_clustered_1000_20
  } else if ( grepl("set_random", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_random
  } else if ( grepl("set_regular", number_of_files_sp_regular[i], fixed=TRUE)) {
    train <- training_set_regular
  }
  
  load(number_of_files_sp_regular[i],.GlobalEnv)
  d <- sb_random
  assign(number_of_files_sp_regular[i], d)
  run_rf_sb_rfe(number_of_files_sp_regular[i], train)
  
}
)



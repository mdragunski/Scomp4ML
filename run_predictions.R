# Clear workspace
rm(list=ls(all=TRUE))

#load libraries
library(caret)
library(raster)

## load from DIRECTORY
setwd("/media/marc/ADATA HV100/Masterarbeit/data/results")

number_of_files <- list.files()

##################################LOAD IN BASE CASE
system.time(for (i in 1:length(number_of_files)){
  
  a <- load(number_of_files[i],.GlobalEnv)
  assign(gsub(".Rdata", "", number_of_files[i]), get(a))
}
)
rm(fit_ffs_rf)
rm(fit_rf)
rm(fit_rfe_rf)

list_of_env <- ls(envir = .GlobalEnv)
list_of_data <- list_of_env[grepl("base_case", list_of_env)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")

###RUN PREDICTIONS FOR THE BASE CASE
system.time(for (i in 1:length(list_of_data)){
  
  pred_fit <- predict(merged_raster, model=get(list_of_data[i]), na.rm=TRUE)
  assign(paste("prediction_", list_of_data[i],sep=""),pred_fit)
  }
)
rm(pred_fit)

###SAVE PREDICTIONS FOR THE BASE CASE
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl("prediction_base_case", list_of_pred)]

setwd("H:/Masterarbeit/data/predictions_result")
system.time(for (i in 1:length(list_of_pred_to_save)){
  
  writeRaster(get(list_of_pred_to_save[i]), filename = paste(list_of_pred_to_save[i], ".grd",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
}
)

#merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_ffs_training_set_clustered_1000_20.grd")

##################################LOAD IN sb base CASE
# Clear workspace
rm(list=ls(all=TRUE))

## load from DIRECTORY
setwd("/media/marc/ADATA HV100/Masterarbeit/data/results_spatial_blocks")

number_of_files <- list.files()

system.time(for (i in 1:length(number_of_files)){
  
  a <- load(number_of_files[i],.GlobalEnv)
  assign(gsub(".Rdata", "", number_of_files[i]), get(a))
}
)
rm(fit_rf_sb)

list_of_env <- ls(envir = .GlobalEnv)
list_of_data <- list_of_env[grepl("spatial_blocks", list_of_env)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")

###RUN PREDICTIONS FOR THE sb CASE
system.time(for (i in 1:length(list_of_data)){
  
  pred_fit <- predict(merged_raster, model=get(list_of_data[i]), na.rm=TRUE)
  assign(paste("prediction_", list_of_data[i],sep=""),pred_fit)
  }
)
rm(pred_fit)

###SAVE PREDICTIONS FOR THE SB CASE
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl("prediction_spatial_blocks", list_of_pred)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result_spatial_blocks")
system.time(for (i in 1:length(list_of_pred_to_save)){
  
  writeRaster(get(list_of_pred_to_save[i]), filename = paste(list_of_pred_to_save[i], ".grd",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
}
)


##################################LOAD IN sb rfe CASE
# Clear workspace
rm(list=ls(all=TRUE))

## load from DIRECTORY
setwd("/media/marc/ADATA HV100/Masterarbeit/data/results_spatial_block_rfe")

number_of_files <- list.files()

system.time(for (i in 1:length(number_of_files)){
  
  a <- load(number_of_files[i],.GlobalEnv)
  assign(gsub(".Rdata", "", number_of_files[i]), get(a))
}
)
rm(fit_sb_rfe_rf)

list_of_env <- ls(envir = .GlobalEnv)
list_of_data <- list_of_env[grepl("spatial_blocks_case_rfe", list_of_env)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")

###RUN PREDICTIONS FOR THE sb CASE
system.time(for (i in 1:length(list_of_data)){
  
  pred_fit <- predict(merged_raster, model=get(list_of_data[i]), na.rm=TRUE)
  assign(paste("prediction_", list_of_data[i],sep=""),pred_fit)
  }
)
rm(pred_fit)

###SAVE PREDICTIONS FOR THE sb rfe CASE
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl("prediction_spatial_blocks_case_rfe_sb", list_of_pred)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_block_rfe")
system.time(for (i in 1:length(list_of_pred_to_save)){
  
  writeRaster(get(list_of_pred_to_save[i]), filename = paste(list_of_pred_to_save[i], ".grd",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
}
)


##################################LOAD IN sb ffs CASE
# Clear workspace
rm(list=ls(all=TRUE))

## load from DIRECTORY
setwd("/media/marc/ADATA HV100/Masterarbeit/data/results_spatial_blocks_ffs")

number_of_files <- list.files()

system.time(for (i in 1:length(number_of_files)){
  
  a <- load(number_of_files[i],.GlobalEnv)
  assign(gsub(".Rdata", "", number_of_files[i]), get(a))
}
)
rm(fit_sb_ffs_rf)

list_of_env <- ls(envir = .GlobalEnv)
list_of_data <- list_of_env[grepl("spatial_blocks_case_ffs", list_of_env)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")

###RUN PREDICTIONS FOR THE sb ffs CASE
system.time(for (i in 1:length(list_of_data)){
  
  pred_fit <- predict(merged_raster, model=get(list_of_data[i]), na.rm=TRUE)
  assign(paste("prediction_", list_of_data[i],sep=""),pred_fit)
  }
)
rm(pred_fit)

###SAVE PREDICTIONS FOR THE sb ffs CASE
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl("prediction_spatial_blocks_case_ffs_sb", list_of_pred)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_blocks_ffs")
system.time(for (i in 1:length(list_of_pred_to_save)){
  
  writeRaster(get(list_of_pred_to_save[i]), filename = paste(list_of_pred_to_save[i], ".grd",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
}
)

#########################################
#          predictions w/o spatial depencencies
##########################################
setwd("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_spatial")

number_of_files <- list.files()

##################################LOAD IN W/O CASE
system.time(for (i in 1:length(number_of_files)){
  
  a <- load(number_of_files[i],.GlobalEnv)
  assign(gsub(".Rdata", "", number_of_files[i]), get(a))
}
)

list_of_env <- ls(envir = .GlobalEnv)
list_of_data <- list_of_env[grepl("fit_rf", list_of_env)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")

###RUN PREDICTIONS FOR THE W/O CASE
system.time(for (i in 1:length(list_of_data)){
  
  pred_fit <- predict(merged_raster, model=get(list_of_data[i]), na.rm=TRUE)
  assign(paste("prediction_", list_of_data[i],sep=""),pred_fit)
}
)
rm(pred_fit)

###SAVE PREDICTIONS FOR THE W/O CASE
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl("prediction_fit_rf", list_of_pred)]

setwd("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_predictions")
system.time(for (i in 1:length(list_of_pred_to_save)){
  
  writeRaster(get(list_of_pred_to_save[i]), filename = paste(list_of_pred_to_save[i], ".grd",sep=""), options="INTERLEAVE=BAND", overwrite=TRUE)
}
)


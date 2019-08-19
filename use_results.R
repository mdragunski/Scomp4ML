source("/media/marc/ADATA HV100/Masterarbeit/se.R")
source("/media/marc/ADATA HV100/Masterarbeit/RegressionStats.R")

library(caret)
library(raster)

#setwd("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result")
merged_raster <- stack("/media/marc/ADATA HV100/Masterarbeit/data/merged_raster.grd")
#teststack <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_ffs_training_set_random.grd")


filename <- "df_base_case_wo.Rdata"
#manually change to filter after the correct prediction files
filtername <- "prediction_fit_rf"

model_vs_pred.data <- data.frame(
  mod_Rsq = double(),
  mod_RMSE = double(),
  pred_Rsq = double(),
  pred_RMSE = double(),
  name = character()
)

###SAVE model_vs_pred data frame
#number_of_files <- list.files()
list_of_pred <- ls(envir = .GlobalEnv)
list_of_pred_to_save <- list_of_pred[grepl(filtername, list_of_pred)]

merged_raster_df <- as.data.frame(merged_raster$RADOLAN)
  
  system.time(for (i in 1:length(list_of_pred_to_save)){
    #print(list_of_pred_to_save[i])
    pred_df <- as.data.frame(get(list_of_pred_to_save[i])$layer)
    result <- regressionStats(pred_df$layer, merged_raster_df$RADOLAN, adj.rsq = FALSE)
    
    if(grepl("rfe",list_of_pred_to_save[i], fixed=TRUE)){
      #print(list_of_pred_to_save[i])
      mod_rf <- get(gsub("prediction_", "", list_of_pred_to_save[i]))
      predictions <- mod_rf$pred[mod_rf$pred$Variables==mod_rf$optsize,]
      #abs error
      result_mod <- regressionStats(predictions$pred,predictions$obs)
      list_to_add <- list(mod_RMSE = result_mod$RMSE,mod_Rsq = result_mod$Rsq ,pred_Rsq = result$Rsq, pred_RMSE = result$RMSE, name = list_of_pred_to_save[i])
      model_vs_pred.data = rbind(model_vs_pred.data,list_to_add, stringsAsFactors=FALSE)  
    } else {
      mod_rf <- get(gsub("prediction_", "", list_of_pred_to_save[i]))
      predictions <- mod_rf$pred[mod_rf$pred$mtry==mod_rf$finalModel$tuneValue$mtry,]
      #abs error
      result_mod <- regressionStats(predictions$pred,predictions$obs)
      list_to_add <- list(mod_RMSE = result_mod$RMSE,mod_Rsq = result_mod$Rsq ,pred_Rsq = result$Rsq, pred_RMSE = result$RMSE, name = list_of_pred_to_save[i])
      model_vs_pred.data = rbind(model_vs_pred.data,list_to_add, stringsAsFactors=FALSE)  
    }
  }
  )
#saving under the correct name
#df_base_case <- model_vs_pred.data
#df_spatial_blocks_case <- model_vs_pred.data
#df_spatial_blocks_case <- format(df_spatial_blocks_case, scientific=FALSE)
#df_spatial_blocks_case_RFE <- format(model_vs_pred.data, scientific=FALSE)
#df_spatial_blocks_case_FFS <- format(model_vs_pred.data, scientific=FALSE)
df_base_case_wo <- format(model_vs_pred.data, scientific=FALSE)
  
save(df_base_case_wo, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_2/", filename,sep=""))


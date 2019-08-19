install.packages("viridis")
library(viridis)
install.packages("ggplot2")
install.packages("ggmap")
library(ggmap)
library(ggplot2)
install.packages("cowplot")
library(cowplot)
#theme_set(theme_bw())
library(sf)
library(sp)
library(raster)
library(tidyverse)
install.packages("corrplot")
library(corrplot)
library(reshape2)


######## Create prediction plots corresponding to table 2-5 #############################

# Table 2
table2_spatial_columns_ffs <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_blocks_ffs/prediction_spatial_blocks_case_ffs_sb_systematic_columns_training_set_random.grd")
table2_kfold_CV_none <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_training_set_random.grd")
table2_kfold_CV_manual <- stack("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_predictions/prediction_fit_rf_wo_spatial_random.grd")

table2 <- stack(list(table2_spatial_columns_ffs,
                     table2_kfold_CV_none,
                     table2_kfold_CV_manual,
                     RADOLAN))
spplot(table2,names.attr = c("Spatial Columns & FFS", "k-fold CV & None", "k-fold CV & Manual", "RADOLAN RW"),col.regions=viridis_pal(),par.settings = list(panel.background=list(col="white"),strip.background=list(col="grey")))

# Table 3
table3_kfold_CV_ffs <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_ffs_training_set_regular.grd")
table3_spatial_rows_rfe <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_block_rfe/prediction_spatial_blocks_case_rfe_sb_systematic_rows_training_set_regular.grd")
table3_kfold_CV_manual <- stack("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_predictions/prediction_fit_rf_wo_spatial_regular.grd")

table3 <- stack(list(table3_kfold_CV_ffs,
                     table3_spatial_rows_rfe,
                     table3_kfold_CV_manual,
                     RADOLAN))
spplot(table3,names.attr = c("k-fold CV & FFS", "Spatial Rows & RFE", "k-fold CV & Manual", "RADOLAN RW"),col.regions=viridis_pal(),par.settings = list(panel.background=list(col="white"),strip.background=list(col="grey")))

# Table 4
table4_spatial_blocks_ffs <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_blocks_ffs/prediction_spatial_blocks_case_ffs_sb_random_random_training_set_clustered_1000_10.grd")
table4_kfold_CV_ffs <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_ffs_training_set_clustered_1000_10.grd")
table4_kfold_CV_manual <- stack("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_predictions/prediction_fit_rf_wo_spatial_cluster_10.grd")

table4 <- stack(list(table4_spatial_blocks_ffs,
                     table4_kfold_CV_ffs,
                     table4_kfold_CV_manual,
                     RADOLAN))
spplot(table4,names.attr = c("Spatial Blocks & FFS", "k-fold CV & FFS", "k-fold CV & Manual", "RADOLAN RW"),col.regions=viridis_pal(),par.settings = list(panel.background=list(col="white"),strip.background=list(col="grey")))

# Table 5
table5_spatial_columns_rfe <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_block_rfe/prediction_spatial_blocks_case_rfe_sb_systematic_columns_training_set_clustered_1000_20.grd")
table5_kfold_CV_ffs <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_result/prediction_base_case_ffs_training_set_clustered_1000_20.grd")
table5_kfold_CV_manual <- stack("/media/marc/ADATA HV100/Masterarbeit/data/results_wo_predictions/prediction_fit_rf_wo_spatial_cluster_20.grd")

table5 <- stack(list(table5_spatial_columns_rfe,
                     table5_kfold_CV_ffs,
                     table5_kfold_CV_manual,
                     RADOLAN))
spplot(table5,names.attr = c("Spatial Columns & RFE", "k-fold CV & FFS", "k-fold CV & Manual", "RADOLAN RW"),col.regions=viridis_pal(),par.settings = list(panel.background=list(col="white"),strip.background=list(col="grey")))

#############
############Create figures which display the predictor designs
#merc = CRS("+init=epsg:3857")
WGS84 = CRS("+init=epsg:4326")
points.random <- spTransform(points_random, WGS84)
pra <- as.data.frame(points.random)
sites.random <- st_as_sf(pra, coords = c("x", "y"), 
                         crs = 4326, agr = "constant")


points.regular <- spTransform(points_regular, WGS84)
prr <- as.data.frame(points.regular)
sites.regular <- st_as_sf(prr, coords = c("x1", "x2"), 
                         crs = 4326, agr = "constant")

points.clustered.1000.10 <- spTransform(points_clustered_1000_10, WGS84)
prc10 <- as.data.frame(points.clustered.1000.10)
sites.clustered.1000.10 <- st_as_sf(prc10, coords = c("x1", "x2"), 
                          crs = 4326, agr = "constant")

points.clustered.1000.20 <- spTransform(points_clustered_1000_20, WGS84)
prc20 <- as.data.frame(points.clustered.1000.20)
sites.clustered.1000.20 <- st_as_sf(prc20, coords = c("x1", "x2"), 
                                    crs = 4326, agr = "constant")

world <- ne_countries(scale = "medium", returnclass = "sf")

map_random <- ggplot(data = world) +
                    geom_sf() +
                    geom_sf(data = sites.random, size = 1, shape = 21, fill = "blue", col="green") +
                    coord_sf(xlim = c(1.71, 18.82), ylim = c(45.80, 56.946), expand = FALSE)+
                    theme_cowplot(12) 

map_regular <- ggplot(data = world) +
                      geom_sf() +
                      geom_sf(data = sites.regular, size = 1, shape = 21, fill = "blue", col="green") +
                      coord_sf(xlim = c(1.71, 18.82), ylim = c(45.80, 56.946), expand = FALSE) +
                      theme_cowplot(12)
  

map_clustered_1000_10 <- ggplot(data = world) +
                                geom_sf() +
                                geom_sf(data = sites.clustered.1000.10, size = 1, shape = 21, fill = "blue", col="green") +
                                coord_sf(xlim = c(1.71, 18.82), ylim = c(45.80, 56.946), expand = FALSE) +
                                theme_cowplot(12)

map_clustered_1000_20 <- ggplot(data = world) +
                                geom_sf() +
                                geom_sf(data = sites.clustered.1000.20, size = 1, shape = 21, fill = "blue", col="green") +
                                coord_sf(xlim = c(1.71, 18.82), ylim = c(45.80, 56.946), expand = FALSE) +
                                theme_cowplot(12)



plot_grid(map_random, map_regular, map_clustered_1000_10, map_clustered_1000_20, labels = c('A', 'B', 'C', 'D'), label_size = 12)
setwd("/media/marc/ADATA HV100/Masterarbeit/plots")
ggsave("study_area.png", width = 12, height = 12, dpi = "print")

#################################Create figures which display the pearson correlation

my_data.random <- as_tibble(training_set_random)

random.cor <- my_data.random %>% 
  rename(
    LONGITUDE = x,
    LATITUDE = y
  )

correlationMatrix.random <- cor(random.cor, method = c("pearson"))
cor.1 <- corrplot(correlationMatrix.random, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
#
my_data.regular <- as_tibble(training_set_regular)

regular.cor <- my_data.regular %>% 
  rename(
    LONGITUDE = x,
    LATITUDE = y
  )

correlationMatrix.regular <- cor(regular.cor, method = c("pearson"))
cor.2 <- corrplot(correlationMatrix.regular, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)
#
my_data.cluster.10 <- as_tibble(training_set_clustered_1000_10)

cluster.10.cor <- my_data.cluster.10 %>% 
  rename(
    LONGITUDE = x,
    LATITUDE = y
  )

correlationMatrix.cluster.10 <- cor(cluster.10.cor, method = c("pearson"))
cor.3 <- corrplot(correlationMatrix.cluster.10, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)


#
my_data.cluster.20 <- as_tibble(training_set_clustered_1000_20)

cluster.20.cor <- my_data.cluster.20 %>% 
  rename(
    LONGITUDE = x,
    LATITUDE = y
  )

correlationMatrix.cluster.20 <- cor(cluster.20.cor, method = c("pearson"))
cor.4 <- corrplot(correlationMatrix.cluster.20, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)


setwd("/media/marc/ADATA HV100/Masterarbeit/plots")
png(height=2000, width=1800, pointsize=25, file="random_pearson.png")
corrplot(correlationMatrix.random, type = "lower", order = "alphabet", tl.col = "black",tl.cex = 2.5,cl.cex = 2,mar=c(0,0,1,0))
dev.off()

png(height=2000, width=1800, pointsize=25, file="cluster_20_pearson.png")
corrplot(correlationMatrix.cluster.20, type = "lower", order = "alphabet", tl.col = "black",tl.cex = 2.5,cl.cex = 2,mar=c(0,0,1,0))
dev.off()
#########################


########################### Create Figures which display the Feature importance scores

random_fi <- fit_rf
rm(fit_rf)
regular_fi <- fit_rf
rm(fit_rf)
cluster_10_fi <- fit_rf
rm(fit_rf)
cluster_20_fi <- fit_rf
rm(fit_rf)

a <- ggplot(varImp(random_fi,type=1)) + theme_cowplot(12)
b <- ggplot(varImp(regular_fi,type=1)) + theme_cowplot(12)
c <- ggplot(varImp(cluster_10_fi,type=1)) + theme_cowplot(12)
d <- ggplot(varImp(cluster_20_fi,type=1)) + theme_cowplot(12)

plot_grid(a, b, c, d, labels = c('A', 'B', 'C', 'D'), label_size = 12)
setwd("/media/marc/ADATA HV100/Masterarbeit/plots")
ggsave("feature_importance.png", width = 6, height = 6, dpi = "print")

prediction <- stack("/media/marc/ADATA HV100/Masterarbeit/data/predictions_results_spatial_blocks_ffs/prediction_spatial_blocks_case_ffs_sb_random_random_training_set_random.grd")
spplot(prediction,col.regions=viridis_pal(),par.settings = list(panel.background=list(col="white"),strip.background=list(col="grey")))


###################### Create TABLES #################
install.packages("kableExtra")
library(kableExtra)
library(dplyr)

###create a table for all results with a randomized predictor sampling 
randoms_df_base_case <- df_base_case  %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("random",name, fixed=TRUE))

randoms_df_base_case_wo <- df_base_case_wo %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("random",name, fixed=TRUE))

randoms_df_spatial_blocks_case <- df_spatial_blocks_case %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_random",name, fixed=TRUE))

randoms_df_spatial_blocks_case_FFS <- df_spatial_blocks_case_FFS %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_random",name, fixed=TRUE))

randoms_df_spatial_blocks_case_RFE <- df_spatial_blocks_case_RFE %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_random",name, fixed=TRUE))

df_randomized_predictors <- rbind(randoms_df_base_case, randoms_df_base_case_wo, randoms_df_spatial_blocks_case, randoms_df_spatial_blocks_case_FFS, randoms_df_spatial_blocks_case_RFE)

df_randomized_predictors <- df_randomized_predictors %>% 
  mutate(mod_RMSE = as.numeric(mod_RMSE)) %>%
  mutate(mod_Rsq = as.numeric(mod_Rsq)) %>%
  mutate(pred_Rsq = as.numeric(pred_Rsq)) %>%
  mutate(pred_RMSE = as.numeric(pred_RMSE)) 

df_randomized_predictors <- df_randomized_predictors %>% 
  mutate_if(is.numeric, round, digits = 3)

save(df_randomized_predictors, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_3/", "df_randomized_predictors.Rdata",sep=""))


###create a table for all results with regular predictor sampling 
regular_df_base_case <- df_base_case  %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("regular",name, fixed=TRUE))

regular_df_base_case_wo <- df_base_case_wo %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("regular",name, fixed=TRUE))

regular_df_spatial_blocks_case <- df_spatial_blocks_case %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_regular",name, fixed=TRUE))

regular_df_spatial_blocks_case_FFS <- df_spatial_blocks_case_FFS %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_regular",name, fixed=TRUE))

regular_df_spatial_blocks_case_RFE <- df_spatial_blocks_case_RFE %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("training_set_regular",name, fixed=TRUE))

df_regular_predictors <- rbind(regular_df_base_case, regular_df_base_case_wo, regular_df_spatial_blocks_case, regular_df_spatial_blocks_case_FFS, regular_df_spatial_blocks_case_RFE)

df_regular_predictors <- df_regular_predictors %>% 
  mutate(mod_RMSE = as.numeric(mod_RMSE)) %>%
  mutate(mod_Rsq = as.numeric(mod_Rsq)) %>%
  mutate(pred_Rsq = as.numeric(pred_Rsq)) %>%
  mutate(pred_RMSE = as.numeric(pred_RMSE)) 

df_regular_predictors <- df_regular_predictors %>% 
  mutate_if(is.numeric, round, digits = 3)

save(df_regular_predictors, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_3/", "df_regular_predictors.Rdata",sep=""))


###create a table for all results with cluster design 1 predictor sampling 
cluster_1_df_base_case <- df_base_case  %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_1",name, fixed=TRUE))

cluster_1_df_base_case_wo <- df_base_case_wo %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("cluster_1",name, fixed=TRUE))

cluster_1_df_spatial_blocks_case <- df_spatial_blocks_case %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_1",name, fixed=TRUE))

cluster_1_df_spatial_blocks_case_FFS <- df_spatial_blocks_case_FFS %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_1",name, fixed=TRUE))

cluster_1_df_spatial_blocks_case_RFE <- df_spatial_blocks_case_RFE %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_1",name, fixed=TRUE))

df_cluster_1_predictors <- rbind(cluster_1_df_base_case, cluster_1_df_base_case_wo, cluster_1_df_spatial_blocks_case, cluster_1_df_spatial_blocks_case_FFS, cluster_1_df_spatial_blocks_case_RFE)

df_cluster_1_predictors <- df_cluster_1_predictors %>% 
  mutate(mod_RMSE = as.numeric(mod_RMSE)) %>%
  mutate(mod_Rsq = as.numeric(mod_Rsq)) %>%
  mutate(pred_Rsq = as.numeric(pred_Rsq)) %>%
  mutate(pred_RMSE = as.numeric(pred_RMSE)) 

df_cluster_1_predictors <- df_cluster_1_predictors %>% 
  mutate_if(is.numeric, round, digits = 3)

save(df_cluster_1_predictors, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_3/", "df_cluster_1_predictors.Rdata",sep=""))

###create a table for all results with cluster design 2 predictor sampling 
cluster_2_df_base_case <- df_base_case  %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_2",name, fixed=TRUE))

cluster_2_df_base_case_wo <- df_base_case_wo %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("cluster_2",name, fixed=TRUE))

cluster_2_df_spatial_blocks_case <- df_spatial_blocks_case %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_2",name, fixed=TRUE))

cluster_2_df_spatial_blocks_case_FFS <- df_spatial_blocks_case_FFS %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_2",name, fixed=TRUE))

cluster_2_df_spatial_blocks_case_RFE <- df_spatial_blocks_case_RFE %>% 
  select(mod_RMSE, mod_Rsq, pred_Rsq, pred_RMSE, name) %>%
  filter(grepl("clustered_1000_2",name, fixed=TRUE))

df_cluster_2_predictors <- rbind(cluster_2_df_base_case, cluster_2_df_base_case_wo, cluster_2_df_spatial_blocks_case, cluster_2_df_spatial_blocks_case_FFS, cluster_2_df_spatial_blocks_case_RFE)

df_cluster_2_predictors <- df_cluster_2_predictors %>% 
  mutate(mod_RMSE = as.numeric(mod_RMSE)) %>%
  mutate(mod_Rsq = as.numeric(mod_Rsq)) %>%
  mutate(pred_Rsq = as.numeric(pred_Rsq)) %>%
  mutate(pred_RMSE = as.numeric(pred_RMSE)) 

df_cluster_2_predictors <- df_cluster_2_predictors %>% 
  mutate_if(is.numeric, round, digits = 3)

save(df_cluster_2_predictors, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_3/", "df_cluster_2_predictors.Rdata",sep=""))

################################## create the actual latex tables
##for the randomized predictors

df_randomized_predictors <- mutate(df_randomized_predictors, Validation = ifelse(grepl("sb_systematic_columns",df_randomized_predictors$name),"Spatial Columns",
                                                                                 ifelse(grepl("sb_systematic_rows",df_randomized_predictors$name),"Spatial Rows",
                                                                                        ifelse(grepl("sb_checkerboard",df_randomized_predictors$name),"Spatial Checkerboard",
                                                                                               ifelse(grepl("sb_random",df_randomized_predictors$name),"Spatial Blocks",
                                                                                                  "k-fold CV")))))

df_randomized_predictors <- mutate(df_randomized_predictors, "Feature Selection" = ifelse(grepl("ffs",df_randomized_predictors$name),"FFS",
                                                                                          ifelse(grepl("rfe",df_randomized_predictors$name),"RFE",
                                                                                                 ifelse(grepl("wo",df_randomized_predictors$name),"Manual",
                                                                                                        "None"))))


df_randomized_predictors_tex <- df_randomized_predictors %>% rename("Model RMSE" = mod_RMSE, "Model R²" = mod_Rsq, "Prediction R²" = pred_Rsq , "Prediction RMSE" = pred_RMSE)

df_randomized_predictors_tex <- select(df_randomized_predictors_tex,-c(name))
df_randomized_predictors_tex <- df_randomized_predictors_tex[,c(5,6,1,4,3,2)]
save(df_randomized_predictors_tex, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_4_tex/", "df_randomized_predictors_tex.Rdata",sep=""))

#### helper step to find out which rows to highlight later on
#df_randomized_predictors <- mutate(df_randomized_predictors, Rsq_diff = mod_Rsq - pred_Rsq)
#
#df_randomized_predictors_tex %>% arrange(desc(!! rlang::sym("Prediction R²")))
#actual latex table
randomized_tex <- kable(cbind(df_randomized_predictors_tex %>% arrange(desc(!! rlang::sym("Prediction R²")))), "latex", caption = "Random table", booktabs = T, linesep = "") %>%
  kable_styling(latex_options = c("scale_down"))


##for the regular predictors

df_regular_predictors <- mutate(df_regular_predictors, Validation = ifelse(grepl("sb_systematic_columns",df_regular_predictors$name),"Spatial Columns",
                                                                                 ifelse(grepl("sb_systematic_rows",df_regular_predictors$name),"Spatial Rows",
                                                                                        ifelse(grepl("sb_checkerboard",df_regular_predictors$name),"Spatial Checkerboard",
                                                                                               ifelse(grepl("sb_random",df_regular_predictors$name),"Spatial Blocks",
                                                                                                      "k-fold CV")))))

df_regular_predictors <- mutate(df_regular_predictors, "Feature Selection" = ifelse(grepl("ffs",df_regular_predictors$name),"FFS",
                                                                                          ifelse(grepl("rfe",df_regular_predictors$name),"RFE",
                                                                                                 ifelse(grepl("wo",df_regular_predictors$name),"Manual",
                                                                                                        "None"))))


df_regular_predictors_tex <- df_regular_predictors %>% rename("Model RMSE" = mod_RMSE, "Model R²" = mod_Rsq, "Prediction R²" = pred_Rsq , "Prediction RMSE" = pred_RMSE)

df_regular_predictors_tex <- select(df_regular_predictors_tex,-c(name))
df_regular_predictors_tex <- df_regular_predictors_tex[,c(5,6,1,4,3,2)]
save(df_regular_predictors_tex, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_4_tex/", "df_regular_predictors_tex.Rdata",sep=""))

#### helper step to find out which rows to highlight later on
#df_regular_predictors <- mutate(df_regular_predictors, Rsq_diff = mod_Rsq - pred_Rsq)
#
#actual latex table
regular_tex <- kable(cbind(df_regular_predictors_tex %>% arrange(desc(!! rlang::sym("Prediction R²")))), "latex", caption = "Regular table", booktabs = T, linesep = "") %>%
  kable_styling(latex_options = c("scale_down"))

##for the cluster 1 predictors

df_cluster_1_predictors <- mutate(df_cluster_1_predictors, Validation = ifelse(grepl("sb_systematic_columns",df_cluster_1_predictors$name),"Spatial Columns",
                                                                           ifelse(grepl("sb_systematic_rows",df_cluster_1_predictors$name),"Spatial Rows",
                                                                                  ifelse(grepl("sb_checkerboard",df_cluster_1_predictors$name),"Spatial Checkerboard",
                                                                                         ifelse(grepl("sb_random",df_cluster_1_predictors$name),"Spatial Blocks",
                                                                                                "k-fold CV")))))

df_cluster_1_predictors <- mutate(df_cluster_1_predictors, "Feature Selection" = ifelse(grepl("ffs",df_cluster_1_predictors$name),"FFS",
                                                                                    ifelse(grepl("rfe",df_cluster_1_predictors$name),"RFE",
                                                                                           ifelse(grepl("wo",df_cluster_1_predictors$name),"Manual",
                                                                                                  "None"))))


df_cluster_1_predictors_tex <- df_cluster_1_predictors %>% rename("Model RMSE" = mod_RMSE, "Model R²" = mod_Rsq, "Prediction R²" = pred_Rsq , "Prediction RMSE" = pred_RMSE)

df_cluster_1_predictors_tex <- select(df_cluster_1_predictors_tex,-c(name))

df_cluster_1_predictors_tex <- df_cluster_1_predictors_tex[,c(5,6,1,4,3,2)]
save(df_cluster_1_predictors_tex, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_4_tex/", "df_cluster_1_predictors_tex.Rdata",sep=""))

#### helper step to find out which rows to highlight later on
#df_cluster_1_predictors <- mutate(df_cluster_1_predictors, Rsq_diff = mod_Rsq - pred_Rsq)
#
#actual latex table
cluster_1_tex <- kable(cbind(df_cluster_1_predictors_tex %>% arrange(desc(!! rlang::sym("Prediction R²")))), "latex", caption = "Regular table", booktabs = T, linesep = "") %>%
  kable_styling(latex_options = c("scale_down"))

##for the cluster 2 predictors

df_cluster_2_predictors <- mutate(df_cluster_2_predictors, Validation = ifelse(grepl("sb_systematic_columns",df_cluster_2_predictors$name),"Spatial Columns",
                                                                               ifelse(grepl("sb_systematic_rows",df_cluster_2_predictors$name),"Spatial Rows",
                                                                                      ifelse(grepl("sb_checkerboard",df_cluster_2_predictors$name),"Spatial Checkerboard",
                                                                                             ifelse(grepl("sb_random",df_cluster_2_predictors$name),"Spatial Blocks",
                                                                                                    "k-fold CV")))))

df_cluster_2_predictors <- mutate(df_cluster_2_predictors, "Feature Selection" = ifelse(grepl("ffs",df_cluster_2_predictors$name),"FFS",
                                                                                        ifelse(grepl("rfe",df_cluster_2_predictors$name),"RFE",
                                                                                               ifelse(grepl("wo",df_cluster_2_predictors$name),"Manual",
                                                                                                      "None"))))


df_cluster_2_predictors_tex <- df_cluster_2_predictors %>% rename("Model RMSE" = mod_RMSE, "Model R²" = mod_Rsq, "Prediction R²" = pred_Rsq , "Prediction RMSE" = pred_RMSE)

df_cluster_2_predictors_tex <- select(df_cluster_2_predictors_tex,-c(name))

df_cluster_2_predictors_tex <- df_cluster_2_predictors_tex[,c(5,6,1,4,3,2)]
save(df_cluster_2_predictors_tex, file = paste("/media/marc/ADATA HV100/Masterarbeit/data/result_data_frames_4_tex/", "df_cluster_2_predictors_tex.Rdata",sep=""))

#### helper step to find out which rows to highlight later on
#df_cluster_2_predictors <- mutate(df_cluster_2_predictors, Rsq_diff = mod_Rsq - pred_Rsq)
#
#actual latex table
cluster_2_tex <- kable(cbind(df_cluster_2_predictors_tex %>% arrange(desc(!! rlang::sym("Prediction R²")))), "latex", caption = "Regular table", booktabs = T, linesep = "") %>%
  kable_styling(latex_options = c("scale_down"))

########################################################## Creation of Boxplots



a <- ggplot(boxplot(df_randomized_predictors_tex$`Prediction R²`)) + theme_cowplot(12)
b <- ggplot(varImp(regular_fi,type=1)) + theme_cowplot(12)
c <- ggplot(varImp(cluster_10_fi,type=1)) + theme_cowplot(12)
d <- ggplot(varImp(cluster_20_fi,type=1)) + theme_cowplot(12)

plot_grid(a, b, c, d, labels = c('A', 'B', 'C', 'D'), label_size = 12)
setwd("/media/marc/ADATA HV100/Masterarbeit/plots")
ggsave("feature_importance.png", width = 12, height = 12, dpi = "print")

##############################


box1 <- select(df_randomized_predictors_tex,-c(1,2,3,4))
random_box <- melt(box1)
random_box <- mutate(random_box, label = "Random")

box2 <- select(df_regular_predictors_tex,-c(1,2,3,4))
regular_box <- melt(box2)
regular_box <- mutate(regular_box, label = "Regular")

box3 <- select(df_cluster_1_predictors_tex,-c(1,2,3,4))
cluster_1_box <- melt(box3)
cluster_1_box <- mutate(cluster_1_box, label = "Clustered 1")

box4 <- select(df_cluster_2_predictors_tex,-c(1,2,3,4))
cluster_2_box <- melt(box4)
cluster_2_box <- mutate(cluster_2_box, label = "Clustered 2")

df_boxplot_1 <- rbind(random_box, regular_box, cluster_1_box, cluster_2_box)


ggplot(data = df_boxplot_1, aes(x=label, y=value)) + 
  geom_boxplot(aes(fill=variable)) + 
  xlab("") + ylab("") + 
  guides(fill=guide_legend(title="")) + 
  theme_cowplot(12) + scale_fill_manual(values=c("#52E146", "#007390")) 

setwd("/media/marc/ADATA HV100/Masterarbeit/plots")
ggsave("boxplot_1.png", width = 6, height = 6, dpi = "print")
###################


#prob <- select(df_randomized_predictors_tex,-c(1,2,3,4))
probtest <- select(df_randomized_predictors_tex,-c(3,4))
probtest_meld <- melt(probtest)
#random_box <- melt(box1)
#random_box <- mutate(random_box, label = "Random")

#not good enough.
# ggplot(data = probtest_meld, aes(x=Validation, y=value)) + 
#   #geom_boxplot(aes(fill=variable)) + 
#   geom_point(aes(colour = factor(variable))) +
#   xlab("") + ylab("") + 
#   guides(fill=guide_legend(title="")) + 
#   theme_cowplot(12) + scale_fill_manual(values=c("#52E146", "#007390")) + facet_wrap( ~ `Feature Selection`, scales="free")

#plots random point design as a boxplot with selection methods on the x axis
a <- ggplot(data = probtest_meld, aes(x=`Feature Selection`, y=value)) + 
  geom_boxplot(aes(fill=variable), show.legend = FALSE) + 
  xlab("") + ylab("") + 
  guides(fill=guide_legend(title="")) + 
  theme_cowplot(12) + scale_fill_manual(values=c("#52E146", "#007390")) #+ facet_wrap( ~ Validation, scales="free")
  
#plots random point design as a boxplot with validation methods on the x axis
b <- ggplot(data = probtest_meld, aes(x=Validation, y=value)) + 
  geom_boxplot(aes(fill=variable)) + 
  xlab("") + ylab("") + 
  guides(fill=guide_legend(title="")) + 
  theme_cowplot(12) + scale_fill_manual(values=c("#52E146", "#007390")) #+ facet_wrap( ~ Validation, scales="free")




plot_grid(a, b, labels = c('A', 'B', 'C', 'D'), label_size = 12)


boxplot(df_base_case$mod_Rsq)
boxplot(df_base_case$pred_Rsq)
error <- preds$pred-preds$obs
boxplot(error)

#
#cluster 1
max(df_cluster_1_predictors_tex$`Prediction R²`)
max(df_cluster_1_predictors_tex$`Model R²`)
median(df_cluster_1_predictors_tex$`Prediction R²`)
median(df_cluster_1_predictors_tex$`Model R²`)
#cluster 2
max(df_cluster_2_predictors_tex$`Prediction R²`)
max(df_cluster_2_predictors_tex$`Model R²`)
median(df_cluster_2_predictors_tex$`Prediction R²`)
median(df_cluster_2_predictors_tex$`Model R²`)
#random
median(df_randomized_predictors_tex$`Prediction R²`)
median(df_randomized_predictors_tex$`Model R²`)
#regular
median(df_regular_predictors_tex$`Prediction R²`)
median(df_regular_predictors_tex$`Model R²`)


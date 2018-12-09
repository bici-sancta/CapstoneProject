library(caret)
library(dplyr)
library(leaps)
library(MASS)
library(car)
library(schoolmath)
library(ggplot2)
library(ggfortify)
library(e1071)

home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

# Binary predictor code - unused
# infile <- "df_model_w_cell_id_ZERO.csv"
# df_model_w_cell_id_ZERO <- read.csv(infile,
#                                stringsAsFactors = FALSE, header = TRUE)
# 
# 
# infile <- "random_forest_binary_predictor.csv"
# BinaryPred <- read.csv(infile,
#                             stringsAsFactors = FALSE, header = TRUE)
# 
# 
# # Merge predictor to new dataset
# RegData <- merge(BinaryPred, df_model_w_cell_id_ZERO, by = "cell_id")
# 
# # Drop predicted 0-Cost incidents.
# RegData <- RegData[RegData$binary_predictor == 1,]
# 
# 
# # Add column names to list drop to be dropped from dataset.
# drop <- c("cell_id", "binary_predictor")
# RegData <- RegData[, !names(RegData) %in% drop]



infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Drop unused columns
drop <- c("num_pedestrian_events")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

RegData$IsZero <- ifelse(RegData$krnl_cost_pedestrian_events !=0, "Not Zero", "Zero")

set.seed(0737)


# Identify columns to log transform - unused
# 
#  Col2Transform <- c("krnl_cost_pedestrian_events", 
#                     "num_near_misses", "access", "dblprk", "dnotyld",
#                     "jywalk", "lvisib", "lwfws", "nobikef", "noswlk", "other", "prkint", "prkswlk", "speed", "vrrlss", "wlksig", "xwalk",
#                     "assist", "bikes", "drives", "other.1", "walks", "n_rqst", "mean_walk_score", "min_walk_score", "max_walk_score", 
#                     "num_walk_scores", "sum_lane_cnt", "sum_width", "sum_area", "num_streets", "num_fire_incd", "med_sale_res_y", 
#                     "med_sale_res_n", "med_sale_com_y", "med_sale_com_n", "med_sale_ind_y", "med_sale_ind_n", "med_sale_pbo_y", 
#                     "med_sale_pbo_n", "sum_cost_non_pedestrian_events", "num_non_pedestrian_events", "dist", "n_object",
#                     "animals_insects", "building.related", "construction", "food", "others", "police.property", "service.complaint", 
#                     "street_sidewalk", "traffic_signal", "trash", "trees_plants", "water.leak", "zoning_parking", "n_request")
#  
#  RegData[RegData == 0] <- .001
#  
#  RegData[Col2Transform] <- log(RegData[Col2Transform])
#  

#####
##### Begin Regressions
#####

# Logistic Regression



# Begin 10-fold CV with stepwise.

RegData.control <- trainControl(method = "cv", number=10)

LogRModel <- train(IsZero ~ . -krnl_cost_pedestrian_events -cell_id, data=RegData,
                   method = "glm", family="binomial", trControl = RegData.control)

LogRModel$results


# Create prediction based on model results 
RegData$predictlogistic <- predict(LogRModel, RegData)



LRModel.Step.Train <- train(krnl_cost_pedestrian_events ~ . -cell_id -IsZero -predictlogistic, data=RegData,
                            method = "leapSeq",
                            tuneGrid = data.frame(nvmax = 1:40),
                            trControl = RegData.control,
                            subset = RegData$predictlogistic == "Not Zero")

LRModel.Step.Train$results
LRModel.Step.Train$bestTune


# DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | 
#########################################################################################################################################

# Set the number equal to the number output from the last line of code: LRModel.Step.Train$bestTune
coef(LRModel.Step.Train$finalModel, 40)
# Manually write all coeficients into the model below


# Run linear model of chosen predictors - CV Output
LRModel.CrossValidatedResult <- lm(formula = krnl_cost_pedestrian_events ~ num_near_misses + 
                                     dblprk + dnotyld + prkint + speed + vrrlss + other.1 + mean_walk_score + 
                                     sum_lane_cnt + sum_area + med_sale_com_y + med_sale_com_n + 
                                     sum_cost_non_pedestrian_events + dist + animals_insects + 
                                     construction + food + others + traffic_signal, data = RegData)

sink("LRModel.CrossValidatedResult.txt")
summary(LRModel.CrossValidatedResult)
sink()
vif(LRModel.CrossValidatedResult)



plot(LRModel.CrossValidatedResult$residuals)
qqnorm(LRModel.CrossValidatedResult$residuals)
qqline(LRModel.CrossValidatedResult$residuals)



# Create prediction dataset
RegDataPred <- RegData

# Create prediction based on model results 
RegDataPred$predict <- predict(LRModel.CrossValidatedResult, RegData)
# Re-add zeros from logistic model
RegDataPred$predict[RegDataPred$predictlogistic == "Zero"] <- 0


# Un-loggify (exponentiate) prediction which was generated in log scale - unused
# Leave commented out if your data is not log transformed, or if it is and you'd like to view the pred vs actual plots in log scale
#RegDataPred$predict <- exp(RegDataPred$predict)

# Exponentiate the logged columns - unused
# Leave commented out if your data is not log transformed, or if it is and you'd like to view the pred vs actual plots in log scale
#RegDataPred[Col2Transform] <- exp(RegDataPred[Col2Transform])



# Residuals used to split color on chart
RegDataPred$Residuals<-RegDataPred$krnl_cost_pedestrian_events - RegDataPred$predict

# Hocus pocus magic line to split the colors
RegDataPred <- RegDataPred %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPred$Residuals) == FALSE, krnl_cost_pedestrian_events, NULL))

setwd(home_dir)
setwd(plot_dir)

ggplot(data = RegDataPred) + 
  geom_point(aes(x = predict, y = krnl_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
  stat_smooth(color = 'dodgerblue3', aes(x = predict, y = krnl_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
  scale_y_continuous(breaks = seq(0,5,.5)) +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,5) +
  ylab("Actual Sum Cost ($ in Millions)") +
  xlab("Predicted Sum Cost ($ in Millions)") +
  ggtitle("Predicted vs Actual Cost of Injury", subtitle = "Linear Model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") 






#######################################################################################################################


 # Final Full Model from CV
 LRModel.CrossValidatedResultFull <- lm(formula = krnl_cost_pedestrian_events ~ num_near_misses + 
                                          access + dblprk + dnotyld + lwfws + nobikef + prkint + prkswlk + 
                                          speed + vrrlss + xwalk + bikes + drives + other.1 + mean_walk_score + 
                                          min_walk_score + sum_lane_cnt + sum_area + num_streets + 
                                          med_sale_res_y + med_sale_res_n + med_sale_com_y + med_sale_com_n + 
                                          med_sale_ind_y + med_sale_ind_n + med_sale_pbo_y + sum_cost_non_pedestrian_events + 
                                          dist + n_object + animals_insects + construction + food + 
                                          others + police.property + service.complaint + traffic_signal + 
                                          trash + trees_plants + zoning_parking + n_request, data = RegData)
 
 setwd(home_dir)
 setwd(data_dir)
 
 sink("LRModel.CrossValidatedResult.Full.txt")
 summary(LRModel.CrossValidatedResultFull)
 sink()
 vif(LRModel.CrossValidatedResultFull)
 
 
 # Create prediction dataset
 RegDataPredFull <- RegData
 # Create prediction based on model results 
 RegDataPredFull$predict <- predict(LRModel.CrossValidatedResultFull, RegData)
 # Re-add zeros from logistic model
 RegDataPredFull$predict[RegDataPred$predictlogistic == "Zero"] <- 0
 
 RegDataPredFull$Residuals<-RegDataPredFull$krnl_cost_pedestrian_events - RegDataPredFull$predict
 
 RMSE(RegDataPredFull$krnl_cost_pedestrian_events, RegDataPredFull$predict)
 
 
 # Hocus pocus magic line to split the colors
 RegDataPredFull <- RegDataPredFull %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPredFull$Residuals) == FALSE, krnl_cost_pedestrian_events, NULL))
 
 
 FullPlot<-ggplot(data = RegDataPredFull) + 
   geom_point(aes(x = predict, y = krnl_cost_pedestrian_events, colour = 'Negative Residuals')) +
   geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
   stat_smooth(color = 'dodgerblue3', aes(x = predict, y = krnl_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
   scale_y_continuous(breaks = seq(0,5,.5)) +
   ylim(0,5) +
   scale_x_continuous(breaks = seq(0,5,.5)) +
   xlim(0,5) +
   ylab("Actual Sum Cost ($ in Millions)") +
   xlab("Predicted Sum Cost ($ in Millions)") +
   ggtitle("Predicted vs Actual Cost of Injury", subtitle = "Full Linear Model") +
   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
   scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") +
   theme(text = element_text(size = 25))
 
 FullPlot
 
 setwd(home_dir)
 setwd(plot_dir)
 
 ggsave(filename = "Linear_Model_Full_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
 ggsave(filename = "Linear_Model_Full.png", width = 16, height = 9, units = "in")



#####################################################################################################################################



# Final Reduced Model 
LRModel.CrossValidatedResultReduced <- lm(formula = krnl_cost_pedestrian_events ~ num_near_misses + 
                                            dblprk + dnotyld + prkint + speed + vrrlss + other.1 + mean_walk_score + 
                                            sum_lane_cnt + sum_area + med_sale_com_y + med_sale_com_n + 
                                            sum_cost_non_pedestrian_events + dist + animals_insects + 
                                            construction + food + others + traffic_signal, data = RegData)

setwd(home_dir)
setwd(data_dir)

sink("LRModel.CrossValidatedResult.Reduced.txt")
summary(LRModel.CrossValidatedResultReduced)
sink()
vif(LRModel.CrossValidatedResultReduced)




# Create prediction dataset
RegDataPredReduced <- RegData
# Create prediction based on model results 
RegDataPredReduced$predict <- predict(LRModel.CrossValidatedResultReduced, RegData)
# Re-add zeros from logistic model
RegDataPredReduced$predict[RegDataPred$predictlogistic == "Zero"] <- 0

RegDataPredReduced$Residuals<-RegDataPredReduced$krnl_cost_pedestrian_events - RegDataPredReduced$predict



# Hocus pocus magic line to split the colors
RegDataPredReduced <- RegDataPredReduced %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPredReduced$Residuals) == FALSE, krnl_cost_pedestrian_events, NULL))


ReducedPlot <- ggplot(data = RegDataPredReduced) + 
  geom_point(aes(x = predict, y = krnl_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
  #stat_smooth(color = 'dodgerblue3', aes(x = predict, y = krnl_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
  scale_y_continuous(breaks = seq(0,5,.5)) +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,5) +
  ylab("Observed Sum Cost ($ in Millions)") +
  xlab("Expected Sum Cost ($ in Millions)") +
  ggtitle("Expected vs Observed Cost of Non-Fatality Incident", subtitle = "Reduced Logistic - Linear Model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") +
  theme(text = element_text(size = 25)) 

setwd(home_dir)
setwd(plot_dir)

ReducedPlot  

ggsave(filename = "Linear_Model_Reduced_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
ggsave(filename = "Linear_Model_Reduced.png", width = 16, height = 9, units = "in")

ReducedDiags <- autoplot(LRModel.CrossValidatedResultReduced, which = 1:6)
ReducedDiags

ggsave(plot = ReducedDiags, filename = "Linear_Model_Reduced_Diagnostics.png", width = 16, height = 9, units ="in")



#Output residual and cells for visualization later
PSIFullFinalResiduals <- RegDataPredReduced$krnl_cost_pedestrian_events - RegDataPredReduced$predict
PSIFullFinalcell_id <- RegDataPredReduced$cell_id

setwd(home_dir)
setwd(data_dir)

ResidualCSV <- data.frame(Cell_ID = PSIFullFinalcell_id, Residuals = PSIFullFinalResiduals)
write.csv(ResidualCSV, "Residuals_by_Gridcell_Reduced_Model.csv", row.names = FALSE)



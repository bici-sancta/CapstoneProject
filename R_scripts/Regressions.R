library(caret)
library(dplyr)
library(leaps)
library(MASS)
library(car)
library(schoolmath)
library(ggplot2)
library(ggfortify)
library(e1071)
library(ROCR)
library(data.table)


home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)



infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Drop unused columns
drop <- c("num_pedestrian_events")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

RegData$IsZero <- ifelse(RegData$krnl_cost_pedestrian_events !=0, "1", "0")

set.seed(0737)

  

#####
##### Begin Regressions
#####





# Setup 10-fold CV
RegData.control <- trainControl(method = "cv", number=10)

# Logistic Regression
LogRModel <- train(IsZero ~ . -krnl_cost_pedestrian_events -cell_id, data=RegData,
                   method = "glm", family="binomial", trControl = RegData.control)

LogRModel$results



# Create prediction based on model results 
RegData$predictlogistic <- predict(LogRModel, RegData)

#########################################################################################################

# AUC of predicted class probabilities

prob <- predict(LogRModel, newdata=RegData, type="prob")
pred <- prediction(prob$`1`, RegData$IsZero)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")


df_roc <- data.frame(perf@x.values, perf@y.values)
colnames(df_roc) <- c("FPR", "TPR")


roc_plot <- ggplot(df_roc, aes(x = FPR, y = TPR)) +
  #geom_point(color = "#354ca1") +
  geom_line(color = "#354ca1", size = 2) +
  #    geom_smooth() +
  ggtitle("ROC of Predicted Class Probabilities") +
  xlab("False Positive Rate") + ylab("True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  theme(text = element_text(size = 25))

roc_plot

setwd(home_dir)
setwd(plot_dir)

ggsave(filename = "Logistic_Model_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
ggsave(filename = "Logistic_Model.png", width = 16, height = 9, units = "in")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc

###################################################################################################

RegData <- as.data.table(RegData)
RegData <- RegData[, logkrnl_cost_pedestrian_events := ifelse(krnl_cost_pedestrian_events > 0, log(krnl_cost_pedestrian_events), NA)]

ggplot(RegData, aes(x = krnl_cost_pedestrian_events)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "White") +
  stat_function(fun = dnorm, args = fitdistr(RegData[krnl_cost_pedestrian_events > 0 , krnl_cost_pedestrian_events],
                                             "normal")$estimate)

ggplot(RegData, aes(x = logkrnl_cost_pedestrian_events)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "White") +
  stat_function(fun = dnorm, args = fitdistr(RegData[krnl_cost_pedestrian_events > 0 , logkrnl_cost_pedestrian_events],
                                             "normal")$estimate)

# 10-fold CV Stepwise Regression
LRModel.Step.Train <- train(logkrnl_cost_pedestrian_events ~ . -krnl_cost_pedestrian_events -cell_id -IsZero -predictlogistic, data=RegData,
                            method = "leapSeq",
                            tuneGrid = data.frame(nvmax = 1:40),
                            trControl = RegData.control,
                            subset = is.na(RegData$logkrnl_cost_pedestrian_events))

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
                                     sum_lane_cnt + med_sale_com_y + med_sale_com_n + 
                                     sum_cost_non_pedestrian_events + dist + animals_insects + 
                                     construction + food + others + traffic_signal, data = RegData)

sink("LRModel.CrossValidatedResult.txt")
summary(LRModel.CrossValidatedResult)
sink()
vif(LRModel.CrossValidatedResult)


# Create prediction dataset
RegDataPred <- RegData

# Create prediction based on model results 
RegDataPred$predict <- predict(LRModel.CrossValidatedResult, RegData)
# Re-add zeros from logistic model
RegDataPred$predict[RegDataPred$predictlogistic == "Zero"] <- 0


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
                                            sum_lane_cnt + med_sale_com_y + med_sale_com_n + 
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
  ggtitle("Expected vs Observed Cost of Non-Fatality Incident", subtitle = "Logistic-Linear Two-Part Model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") +
  theme(text = element_text(size = 25)) 

setwd(home_dir)
setwd(plot_dir)

ReducedPlot  

ggsave(filename = "Linear_Model_Reduced_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
ggsave(filename = "Linear_Model_Reduced.png", width = 16, height = 9, units = "in")

ReducedDiags <- autoplot(LRModel.CrossValidatedResultReduced, which = 1:6) + theme(text = element_text(size = 18))
ReducedDiags

ggsave(plot = ReducedDiags, filename = "Linear_Model_Reduced_Diagnostics.png", width = 16, height = 9, units ="in")



#Output residual and cells for visualization later
PSIFullFinalResiduals <- RegDataPredReduced$krnl_cost_pedestrian_events - RegDataPredReduced$predict
PSIFullFinalcell_id <- RegDataPredReduced$cell_id

setwd(home_dir)
setwd(data_dir)

ResidualCSV <- data.frame(Cell_ID = PSIFullFinalcell_id, Residuals = PSIFullFinalResiduals)
write.csv(ResidualCSV, "Residuals_by_Gridcell_Reduced_Model.csv", row.names = FALSE)



#####################
# Model Comparison and Evaluation
####################

# Stepwise vs Manual Pruning. Manual Pruning Statistically Different
anova(LRModel.CrossValidatedResultFull, LRModel.CrossValidatedResultReduced, test = "F")


library(caret)
library(dplyr)
library(leaps)
library(MASS)
library(car)
library(schoolmath)
library(ggplot2)
library(ggfortify)

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



infile <- "df_model_w_cell_id_ZERO.csv"
df_model_w_cell_id_ZERO <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Drop unused data
drop <- c("med_sale_pbo_n", "med_sale_pbo_y")
RegData <- df_model_w_cell_id_ZERO[, !names(df_model_w_cell_id_ZERO) %in% drop]

# Drop >5-Cost incidents and 0-cost incidents
RegData <- RegData[!RegData$sum_cost_pedestrian_events == 0,]
RegData <- RegData[!RegData$sum_cost_pedestrian_events > 5,]


set.seed(0737)


# Identify columns to log transform - unused

# Col2Transform <- c("sum_cost_pedestrian_events",
#                    "num_near_misses",            "n_rqst",                    "access",                    
#                    "dblprk",                     "dnotyld",                    "jywalk",                     "lvisib",                    
#                    "lwfws",                      "nobikef",                    "noswlk",                     "other",                     
#                    "prkint",                     "prkswlk",                    "speed",                      "vrrlss",                    
#                    "wlksig",                    "xwalk",                      "assist",                     "bikes",                     
#                    "drives",                     "other.1",                    "walks",                      "num_walk_scores",
#                    "sum_lane_cnt",           "sum_width",                  "sum_area",                   "num_streets",                "num_fire_incd",             
#                    "med_sale_res_y",             "med_sale_res_n",             "med_sale_com_y",             "med_sale_com_n",            
#                    "med_sale_ind_y",             #"med_sale_ind_n",             "med_sale_pbo_y", Not applicable with 0s removed? ###################                        
#                    "sum_cost.y",                 "num_events.y",               "dist",                       "n_object",                  
#                    "animals_insects",            "building.related",           "construction",               "food",                      
#                    "others",                     "police.property",            "service.complaint",          "street_sidewalk",           
#                    "traffic_sig0l",              "trash",                      "trees_plants",               "water.leak",                
#                    "zoning_parking",             "n_request")
# 
# RegData[RegData == 0] <- .001
# 
# RegData[Col2Transform] <- log(RegData[Col2Transform])


#####
##### Begin Regressions
#####

# Basic regressions commented out to not waste procesing time. Already examined.
# Simplest linear regression.
# LRModel.full <- lm(sum_cost_pedestrian_events ~ . -cell_id, RegData)
# sink("LRModel.full.txt")
# print(summary(LRModel.full))
# sink()

# Linear regression with every combination of interaction
# LRModel.fullint <- lm(sum_cost_pedestrian_events ~ (.)^2, RegData)
# sink("LRModel.fullint.txt")
# print(summary(LRModel.fullint))
# sink()




# Stepwise regressions commented out since we give preference to the CV stepwise regs

# LRModel.Step <- stepAIC(LRModel.full, direction = "both", trace = FALSE)
# Output results to txt file
# sink("LRModel.full.step.txt")
# print(summary(LRModel.step))
# sink()

# LRModel.forward <- stepAIC(LRModel.full, direction = "forward", trace = FALSE)
# Output results to txt file
# sink("LRModel.full.forward.txt")
# print(summary(LRModel.forward))
# sink()

# LRModel.backward <- stepAIC(LRModel.full, direction = "backward", trace = FALSE)
# Output results to txt file
# sink("LRModel.full.backward.txt")
# print(summary(LRModel.backward))
# sink()
## Stepwise is currently best preforming model.




# Begin 10-fold CV with stepwise.

RegData.control <- trainControl(method = "cv", number=10)

LRModel.Step.Train <- train(sum_cost_pedestrian_events ~ . -cell_id, data=RegData,
                            method = "leapSeq",
                            tuneGrid = data.frame(nvmax = 10:35),
                            trControl = RegData.control)

LRModel.Step.Train$results
LRModel.Step.Train$bestTune


# DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | DO NOT RUN PAST THIS LINE IF YOU ARE EXPLORING | 
#########################################################################################################################################

# Set the number equal to the number output from the last line of code: LRModel.Step.Train$bestTune
coef(LRModel.Step.Train$finalModel, 30)
# Manually write all coeficients into the model below


# Run linear model of chosen predictors - CV Output
LRModel.CrossValidatedResult <- lm(formula = sum_cost_pedestrian_events ~ access + dblprk + jywalk + 
                                            lvisib + nobikef + prkint + prkswlk + speed + vrrlss + wlksig + 
                                            assist + drives + max_walk_score + sum_area + num_streets + 
                                            num_fire_incd + med_sale_res_n + med_sale_com_y + med_sale_com_n + 
                                            sum_cost.y + dist + n_object + food + police.property + street_sidewalk + 
                                            trees_plants + water.leak + zoning_parking + xwalk + n_request, 
                                          data = RegData)

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


# Un-loggify (exponentiate) prediction which was generated in log scale - unused
# Leave commented out if your data is not log transformed, or if it is and you'd like to view the pred vs actual plots in log scale
#RegDataPred$predict <- exp(RegDataPred$predict)

# Exponentiate the logged columns - unused
# Leave commented out if your data is not log transformed, or if it is and you'd like to view the pred vs actual plots in log scale
#RegDataPred[Col2Transform] <- exp(RegDataPred[Col2Transform])



# Residuals used to split color on chart
RegDataPred$Residuals<-RegDataPred$sum_cost_pedestrian_events - RegDataPred$predict

# Hocus pocus magic line to split the colors
RegDataPred <- RegDataPred %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPred$Residuals) == FALSE, sum_cost_pedestrian_events, NULL))

setwd(home_dir)
setwd(plot_dir)

ggplot(data = RegDataPred) + 
  geom_point(aes(x = predict, y = sum_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
  stat_smooth(color = 'dodgerblue3', aes(x = predict, y = sum_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
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
LRModel.CrossValidatedResultFull <- lm(formula = sum_cost_pedestrian_events ~ access + dblprk + jywalk + 
                                     lvisib + nobikef + prkint + prkswlk + speed + vrrlss + wlksig + 
                                     assist + drives + max_walk_score + sum_area + num_streets + 
                                     num_fire_incd + med_sale_res_n + med_sale_com_y + med_sale_com_n + 
                                     sum_cost.y + dist + n_object + food + police.property + street_sidewalk + 
                                     trees_plants + water.leak + zoning_parking + xwalk + n_request, 
                                   data = RegData)

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
RegDataPredFull$Residuals<-RegDataPredFull$sum_cost_pedestrian_events - RegDataPredFull$predict



# Hocus pocus magic line to split the colors
RegDataPredFull <- RegDataPredFull %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPredFull$Residuals) == FALSE, sum_cost_pedestrian_events, NULL))


FullPlot<-ggplot(data = RegDataPredFull) + 
  geom_point(aes(x = predict, y = sum_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
  stat_smooth(color = 'dodgerblue3', aes(x = predict, y = sum_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
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
LRModel.CrossValidatedResultReduced <- lm(sum_cost_pedestrian_events ~ 
                                            prkint+prkswlk+speed+
                                            vrrlss+max_walk_score+
                                            sum_cost.y+food+police.property+trees_plants+
                                            water.leak+xwalk
                                          , data = RegData)

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
RegDataPredReduced$Residuals<-RegDataPredReduced$sum_cost_pedestrian_events - RegDataPredReduced$predict



# Hocus pocus magic line to split the colors
RegDataPredReduced <- RegDataPredReduced %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPredReduced$Residuals) == FALSE, sum_cost_pedestrian_events, NULL))


ReducedPlot <- ggplot(data = RegDataPredReduced) + 
  geom_point(aes(x = predict, y = sum_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = predict, y = cost_gt_pred, colour = "Positive Residuals")) +
  #stat_smooth(color = 'dodgerblue3', aes(x = predict, y = sum_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
  scale_y_continuous(breaks = seq(0,5,.5)) +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,5) +
  ylab("Observed Sum Cost ($ in Millions)") +
  xlab("Expected Sum Cost ($ in Millions)") +
  ggtitle("Expected vs Observed Cost of Non-Fatality Incident", subtitle = "Reduced Linear Model") +
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
PSIFullFinalResiduals <- RegDataPredReduced$sum_cost_pedestrian_events - RegDataPredReduced$predict
PSIFullFinalcell_id <- RegDataPredReduced$cell_id

setwd(home_dir)
setwd(data_dir)

ResidualCSV <- data.frame(Cell_ID = PSIFullFinalcell_id, Residuals = PSIFullFinalResiduals)
write.csv(ResidualCSV, "Residuals_by_Gridcell_Reduced_Model.csv", row.names = FALSE)



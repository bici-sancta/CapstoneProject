library(caret)
library(dplyr)
library(leaps)
library(MASS)
library(car)

home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

# Binary predictor code - unused
# infile <- "df_model_w_cell_id.csv"
# df_model_w_cell_id <- read.csv(infile,
#                                stringsAsFactors = FALSE, header = TRUE)
# 
# 
# infile <- "random_forest_binary_predictor.csv"
# BinaryPred <- read.csv(infile,
#                             stringsAsFactors = FALSE, header = TRUE)
# 
# 
# # Merge predictor to new dataset
# RegData <- merge(BinaryPred, df_model_w_cell_id, by = "cell_id")
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

# Drop unused data
drop <- c("med_sale_pbo_n", "med_sale_pbo_y")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

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



# Hocus pocus magic line to split the colors
RegDataPlot <- RegDataPred %>% mutate(cost_gt_pred = ifelse(sum_cost_pedestrian_events > (1.10 * predict), sum_cost_pedestrian_events, NA))

par(bg = "lightgrey")
plot(sum_cost_pedestrian_events ~ predict, data = RegDataPlot,
     col = "springgreen4", #Type 1 error color
     xlim = c(0, 5), #adjust to fit plot if in log scale or diff data
     ylim = c(0, 5), #adjust to fit plot if in log scale or diff data
     xlab="Predicted Cost ($ in Millions)", #labels
     ylab="Actual Sum Cost ($ in Millions)") #labels
points(cost_gt_pred ~ predict, data = RegDataPlot, col = "red4") #Type 2 error color
abline(a = 0, b = 1, col = "dodgerblue3") #line color


#Output residual and cells for visualization later
PSIFullFinalResiduals <- RegDataPred$sum_cost_pedestrian_events - RegDataPred$predict
PSIFullFinalcell_id <- RegDataPred$cell_id

setwd(home_dir)
setwd(data_dir)

PSIFinal <- data.frame(Cell_ID = PSIFullFinalcell_id, Residuals = PSIFullFinalResiduals)
write.csv(PSIFinal, "Residuals_by_Gridcell_Reduced_Model.csv", row.names = FALSE)




# Final Full Model from CV
LRModel.CrossValidatedResult <- lm(formula = sum_cost_pedestrian_events ~ access + dblprk + jywalk + 
                                     lvisib + nobikef + prkint + prkswlk + speed + vrrlss + wlksig + 
                                     assist + drives + max_walk_score + sum_area + num_streets + 
                                     num_fire_incd + med_sale_res_n + med_sale_com_y + med_sale_com_n + 
                                     sum_cost.y + dist + n_object + food + police.property + street_sidewalk + 
                                     trees_plants + water.leak + zoning_parking + xwalk + n_request, 
                                   data = RegData)

sink("LRModel.CrossValidatedResult.Full.txt")
summary(LRModel.CrossValidatedResult)
sink()
vif(LRModel.CrossValidatedResult)



# Final Reduced Model 
LRModel.CrossValidatedResultReduced <- lm(sum_cost_pedestrian_events ~ 
                                            lvisib+prkint+prkswlk+speed+
                                            vrrlss+wlksig+max_walk_score+sum_area+num_streets
                                          +sum_cost.y+food+police.property+trees_plants+
                                            water.leak+xwalk
                                          , data = RegData)

sink("LRModel.CrossValidatedResult.Reduced.txt")
summary(LRModel.CrossValidatedResultReduced)
sink()
vif(LRModel.CrossValidatedResultReduced)



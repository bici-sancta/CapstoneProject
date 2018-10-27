
library(caret)
library(dplyr)
library(leaps)



home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]


set.seed(0737)


Col2Transform <- c("sum_cost_pedestrian_events",
                   "num_near_misses",            "n_rqst",                    "access",                    
                   "dblprk",                     "dnotyld",                    "jywalk",                     "lvisib",                    
                    "lwfws",                      "nobikef",                    "noswlk",                     "other",                     
                    "prkint",                     "prkswlk",                    "speed",                      "vrrlss",                    
                    "wlksig",                    "xwalk",                      "assist",                     "bikes",                     
                    "drives",                     "other.1",                    "walks",                      "num_walk_scores",
                    "sum_lane_cnt",           "sum_width",                  "sum_area",                   "num_streets",                "num_fire_incd",             
                    "med_sale_res_y",             "med_sale_res_n",             "med_sale_com_y",             "med_sale_com_n",            
                    "med_sale_ind_y",             "med_sale_ind_n",             "med_sale_pbo_y",                         
                    "sum_cost.y",                 "num_events.y",               "dist",                       "n_object",                  
                    "animals_insects",            "building.related",           "construction",               "food",                      
                    "others",                     "police.property",            "service.complaint",          "street_sidewalk",           
                    "traffic_sig0l",              "trash",                      "trees_plants",               "water.leak",                
                    "zoning_parking",             "n_request")

RegData[RegData == 0] <- .01

RegData[Col2Transform] <- log(RegData[Col2Transform])


# # Output large results to txt file
# sink("LRModel.fullint.txt")
# print(summary(lm(sum_cost_pedestrian_events ~ (.)^2, RegData)))
# sink()
# 
# LRModel.fullint <- lm(sum_cost_pedestrian_events ~ (.)^2, RegData)
# 
# sink("LRModel.full.txt")
# print(summary(lm(sum_cost_pedestrian_events ~ ., RegData)))
# sink()
# LRModel.full <- lm(sum_cost_pedestrian_events ~ ., RegData)

# Stepwise regressions.

# Output results to txt file
sink("LRModel.full.step.txt")
print(summary(stepAIC(LRModel.full, direction = "both", trace = FALSE)))
sink()
# Also have results in R
LRModel.step <- stepAIC(LRModel.full, direction = "both", trace = FALSE)
# 
# # Output results to txt file
# sink("LRModel.full.forward.txt")
# print(summary(stepAIC(LRModel.full, direction = "forward", trace = FALSE)))
# sink()
# # Also have results in R
# LRModel.forward <- stepAIC(LRModel.full, direction = "forward", trace = FALSE)
# 
# # Output results to txt file
# sink("LRModel.full.backward.txt")
# print(summary(stepAIC(LRModel.full, direction = "backward", trace = FALSE)))
# sink()
# # Also have results in R
# LRModel.backward <- stepAIC(LRModel.full, direction = "backward", trace = FALSE)



## Stepwise is currently best preforming model.
# 10-fold CV with stepwise.

RegData.control <- trainControl(method = "cv", number=10)

LRModel.Step.Train <- train(sum_cost_pedestrian_events ~ ., data=RegData,
                            method = "leapSeq",
                            tuneGrid = data.frame(nvmax = 10:35),
                            trControl = RegData.control)

LRModel.Step.Train$results
LRModel.Step.Train$bestTune
#summary(LRModel.Step.Train$finalModel)
coef(LRModel.Step.Train$finalModel, 28)



# Run linear model of chosen predictors - CV Output
sink("LRModel.CrossValidatedResult.txt")
print(summary(lm(sum_cost_pedestrian_events ~ 
                 access+jywalk+noswlk+speed+other.1+mean_walk_score+num_walk_scores+sum_width+sum_area+num_fire_incd+med_sale_res_n+
                 med_sale_com_y+med_sale_com_n+med_sale_ind_y+med_sale_pbo_y+num_events.y+dist+n_object+building.related+
                 construction+others+police.property+service.complaint+street_sidewalk+trash+trees_plants+water.leak+n_request, data = RegData)))
sink()

LRModel.CrossValidatedResult <- lm(formula = sum_cost_pedestrian_events ~ access + jywalk + noswlk + 
                                     speed + other.1 + mean_walk_score + num_walk_scores + sum_width + 
                                     sum_area + num_fire_incd + med_sale_res_n + med_sale_com_y + 
                                     med_sale_com_n + med_sale_ind_y + med_sale_pbo_y + num_events.y + 
                                     dist + n_object + building.related + construction + others + 
                                     police.property + service.complaint + street_sidewalk + trash + 
                                     trees_plants + water.leak + n_request, data = RegData)

plot(LRModel.CrossValidatedResult$residuals)
qqnorm(LRModel.CrossValidatedResult$residuals)
qqline(LRModel.CrossValidatedResult$residuals)



# Generate predictions
RegDataPred <- RegData

# Create prediction based on model results 
RegDataPred$predict <- predict(LRModel.CrossValidatedResult, RegData)

# Un-loggify (exponentiate) prediction which was generated in log scale
#RegDataPred$predict <- exp(RegDataPred$predict)

# Exponentiate the logged columns
#RegDataPred[Col2Transform] <- exp(RegDataPred[Col2Transform])



# Create prediction plot
RegDataPlot <- RegDataPred %>% mutate(cost_gt_pred = ifelse(sum_cost_pedestrian_events > (1.10 * predict), sum_cost_pedestrian_events, NA))

par(bg = "lightgrey")
plot(sum_cost_pedestrian_events ~ predict, data = RegDataPlot,
     col = "springgreen4", 
     xlim = c(-7, 5),
     ylim = c(-7, 5),
     xlab="Log Predicted Cost ($ in Millions)",
     ylab="Log Actual Sum Cost ($ in Millions)")
points(cost_gt_pred ~ predict, data = RegDataPlot, col = "red4")
abline(a = 0, b = 1, col = "dodgerblue3")







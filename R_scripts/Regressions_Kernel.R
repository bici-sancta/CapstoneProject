library(MASS)
library(caret)
library(dplyr)
library(leaps)



home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)



infile <- "ped_crash_cost_kernel_distribution.csv"
crash_cost_krnl <- read.csv(infile,
                            stringsAsFactors = FALSE, header = TRUE)

drop <- c("lat", "long")
crash_cost_krnl <- crash_cost_krnl[, !names(crash_cost_krnl) %in% drop]



infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)



# ...   merge costs into df_model
drop <- c("sum_cost_pedestrian_events")
df_model <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

RegDataKern <- merge(crash_cost_krnl, df_model, by = "cell_id")

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id")
RegDataKern <- RegDataKern[, !names(RegDataKern) %in% drop]


RegDataKern <- RegDataKern[RegDataKern$kernelized_cost<40000,]
plot(RegDataKern$kernelized_cost)

set.seed(0737)


Col2Transform <- c("kernelized_cost",
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

RegDataKern[RegDataKern == 0] <- .01

RegDataKern[Col2Transform] <- log(RegDataKern[Col2Transform])


# # Output large results to txt file
# sink("LRModel.Kernel.fullint.txt")
# print(summary(lm(kernelized_cost ~ (.)^2, RegDataKern)))
# sink()
# 
# LRModel.Kernel.fullint <- lm(kernelized_cost ~ (.)^2, RegDataKern)
# 
 sink("LRModel.Kernel.full.txt")
 print(summary(lm(kernelized_cost ~ ., RegDataKern)))
 sink()
 LRModel.Kernel.full <- lm(kernelized_cost ~ ., RegDataKern)

# Stepwise regressions.

# Output results to txt file
sink("LRModel.Kernel.full.step.txt")
print(summary(stepAIC(LRModel.Kernel.full, direction = "both", trace = FALSE)))
sink()
# Also have results in R
LRModel.Kernel.step <- stepAIC(LRModel.Kernel.full, direction = "both", trace = FALSE)
# 
# # Output results to txt file
# sink("LRModel.Kernel.full.forward.txt")
# print(summary(stepAIC(LRModel.Kernel.full, direction = "forward", trace = FALSE)))
# sink()
# # Also have results in R
# LRModel.Kernel.forward <- stepAIC(LRModel.Kernel.full, direction = "forward", trace = FALSE)
# 
# # Output results to txt file
# sink("LRModel.Kernel.full.backward.txt")
# print(summary(stepAIC(LRModel.Kernel.full, direction = "backward", trace = FALSE)))
# sink()
# # Also have results in R
# LRModel.Kernel.backward <- stepAIC(LRModel.Kernel.full, direction = "backward", trace = FALSE)



## Stepwise is currently best preforming model.
# 10-fold CV with stepwise.

RegDataKern.control <- trainControl(method = "cv", number=10)

LRModel.Kernel.Step.Train <- train(kernelized_cost ~ ., data=RegDataKern,
                            method = "leapSeq",
                            tuneGrid = data.frame(nvmax = 1:57),
                            trControl = RegDataKern.control)

LRModel.Kernel.Step.Train$results
LRModel.Kernel.Step.Train$bestTune
#summary(LRModel.Kernel.Step.Train$finalModel)
coef(LRModel.Kernel.Step.Train$finalModel, 53)



# Run linear model of chosen predictors - CV Output
sink("LRModel.Kernel.CrossValidatedResult.txt")
print(summary(lm(kernelized_cost ~ num_near_misses+n_rqst+access+dblprk+dnotyld+jywalk+
                   lvisib+lwfws+nobikef+noswlk+other+prkint+speed+
                   vrrlss+wlksig+xwalk+assist+drives+other.1+walks+
                   mean_walk_score+min_walk_score+max_walk_score+num_walk_scores+sum_lane_cnt+sum_width+sum_area+
                   num_streets+num_fire_incd+med_sale_res_y+med_sale_res_n+med_sale_com_y+med_sale_com_n+med_sale_ind_y+
                   med_sale_ind_n+med_sale_pbo_y+num_events.y+dist+n_object+animals_insects+building.related+
                   construction+food+others+police.property+service.complaint+street_sidewalk+trash+
                   trees_plants+water.leak+zoning_parking+n_request+med_sale_pbo_n
                   
                   , data = RegDataKern)))
sink()

LRModel.Kernel.CrossValidatedResult <- lm(formula = kernelized_cost ~ num_near_misses + n_rqst + access + 
                                     dblprk + dnotyld + jywalk + lvisib + lwfws + nobikef + noswlk + 
                                     other + prkint + speed + vrrlss + wlksig + xwalk + assist + 
                                     drives + other.1 + walks + mean_walk_score + min_walk_score + 
                                     max_walk_score + num_walk_scores + sum_lane_cnt + sum_width + 
                                     sum_area + num_streets + num_fire_incd + med_sale_res_y + 
                                     med_sale_res_n + med_sale_com_y + med_sale_com_n + med_sale_ind_y + 
                                     med_sale_ind_n + med_sale_pbo_y + num_events.y + dist + n_object + 
                                     animals_insects + building.related + construction + food + 
                                     others + police.property + service.complaint + street_sidewalk + 
                                     trash + trees_plants + water.leak + zoning_parking + n_request + 
                                     med_sale_pbo_n, data = RegDataKern)

plot(LRModel.Kernel.CrossValidatedResult$residuals)
qqnorm(LRModel.Kernel.CrossValidatedResult$residuals)
qqline(LRModel.Kernel.CrossValidatedResult$residuals)



# Predictions dataset
RegDataPred <- RegDataKern

# Create prediction based on model results 
RegDataPred$predict <- predict(LRModel.Kernel.CrossValidatedResult, RegDataKern)

# Un-loggify (exponentiate) prediction which was generated in log scale
RegDataPred$predict <- exp(RegDataPred$predict)

# Exponentiate the logged columns (including response variable)
RegDataPred[Col2Transform] <- exp(RegDataPred[Col2Transform])



# Create prediction plot
RegDataPlot <- RegDataPred %>% mutate(cost_gt_pred = ifelse(kernelized_cost > (1.10 * predict), kernelized_cost, NA))

par(bg = "lightgrey")
plot(kernelized_cost ~ predict, data = RegDataPlot,
     col = "springgreen4", 
     xlim = c(0, 30),
     ylim = c(0, 30),
     xlab="Predicted Cost ($ in Millions)",
     ylab="Actual Sum Cost ($ in Millions)")
points(cost_gt_pred ~ predict, data = RegDataPlot, col = "red4")
abline(a = 0, b = 1, col = "dodgerblue3")







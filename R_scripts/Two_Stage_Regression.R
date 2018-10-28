
library(caret)
library(dplyr)
library(leaps)
library(e1071)



home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id", "med_sale_pbo_n")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

LogisticData <- RegData
LogisticData$sum_cost_pedestrian_events <- cut(LogisticData$sum_cost_pedestrian_events,
                                           breaks = c(-Inf,0,Inf),
                                           labels = c("Zero Value", "Non-Zero Value"))




set.seed(0737)


# Log-transform skewed data columns.
Col2Transform <- c(#"sum_cost_pedestrian_events",
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

LogisticData[LogisticData == 0] <- .01

LogisticData[Col2Transform] <- log(LogisticData[Col2Transform])

# Find linear combinations
LogMatrix<-data.matrix(LogisticData)
findLinearCombos(LogMatrix)

# Add column names to list drop to be dropped from dataset.
drop <- c("wlksig", "other.1", "water.leak")
LogisticData <- LogisticData[, !names(LogisticData) %in% drop]

# Find linear combinations
LogMatrix<-data.matrix(LogisticData)
findLinearCombos(LogMatrix)





#https://mathewanalytics.com/2015/09/02/logistic-regression-in-r/

# split the data into training and testing datasets 
Train <- createDataPartition(LogisticData$sum_cost_pedestrian_events, p=0.8, list=FALSE)
training <- LogisticData[ Train, ]
testing <- LogisticData[ -Train, ]

# use glm to train the model on the training dataset. make sure to set family to "binomial"
mod_fit_one <- glm(sum_cost_pedestrian_events ~ ., data=training, family="binomial")

summary(mod_fit_one) # estimates 
exp(coef(mod_fit_one)) # odds ratios
predict(mod_fit_one, newdata=testing, type="response") # predicted probabilities


library(pscl)
pR2(mod_fit_one)

varImp(mod_fit_one)


library(ROCR)
# Compute AUC for predicting Class with the model
prob <- predict(mod_fit_one, newdata=testing, type="response")
pred <- prediction(prob, testing$sum_cost_pedestrian_events)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc


# Set up cross validation

LogisticControl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

LogCVFit <- train(sum_cost_pedestrian_events ~ ., data = LogisticData,
                  method = "glm", family = binomial(),
                  trControl = LogisticControl)


LogCVFit$results
LogCVFit$bestTune

#coef(LogCVFit, 28)


#LogisticPred = predict(LogCVFit, newdata=)


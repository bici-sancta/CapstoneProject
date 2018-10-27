
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
drop <- c("cell_id")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

LogisticData <- RegData
LogisticData$sum_cost_pedestrian_events <- cut(LogisticData$sum_cost_pedestrian_events,
                                           breaks = c(-Inf,0,Inf),
                                           labels = c("Zero Value", "Non-Zero Value"))



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








#https://mathewanalytics.com/2015/09/02/logistic-regression-in-r/


# Set up cross validation

LogisticControl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

LogCVFit <- train(sum_cost_pedestrian_events ~ ., data = LogisticData,
                  method = "glm", family = binomial(),
                  trControl = LogisticControl)


LogCVFit$results
LogCVFit$bestTune

#coef(LogCVFit, 28)


#LogisticPred = predict(LogCVFit, newdata=)


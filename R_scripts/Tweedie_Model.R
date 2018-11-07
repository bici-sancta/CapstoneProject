library(HDtweedie)
library(dplyr)
library(bindrcpp)
library(schoolmath)
library(ggplot2)
library(ggfortify)




home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

set.seed(0737)

# infile <- "df_model_w_cell_id_ZERO.csv"
# df_model_w_cell_id_ZERO <- read.csv(infile,
#                                stringsAsFactors = FALSE, header = TRUE)
# 
# 
# infile <- "random_forest_binary_predictor.csv"
# BinaryPred <- read.csv(infile,
#                        stringsAsFactors = FALSE, header = TRUE)
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


# Drop >5-Cost incidents and 0-cost incidents
df_model_w_cell_id_ZERO <- df_model_w_cell_id_ZERO[!df_model_w_cell_id_ZERO$sum_cost_pedestrian_events == 0,]
df_model_w_cell_id_ZERO <- df_model_w_cell_id_ZERO[!df_model_w_cell_id_ZERO$sum_cost_pedestrian_events > 5,]


Cell_IDs <- df_model_w_cell_id_ZERO$cell_id

# Drop unused data
drop <- c("cell_id", "med_sale_pbo_n", "med_sale_pbo_y")
RegData <- df_model_w_cell_id_ZERO[, !names(df_model_w_cell_id_ZERO) %in% drop]



# Tweedie input must be matrix
# Raw Y values
TwdY<-matrix(RegData$sum_cost_pedestrian_events)


# Mostly Log-transformed X values prep
 
 Col2Transform <- c(#"sum_cost_pedestrian_events",
   "num_near_misses",            "n_rqst",                    "access",                    
   "dblprk",                     "dnotyld",                    "jywalk",                     "lvisib",                    
   "lwfws",                      "nobikef",                    "noswlk",                     "other",                     
   "prkint",                     "prkswlk",                    "speed",                      "vrrlss",                    
   "wlksig",                    "xwalk",                      "assist",                     "bikes",                     
   "drives",                     "other.1",                    "walks",                      "num_walk_scores",
   "sum_lane_cnt",           "sum_width",                  "sum_area",                   "num_streets",                "num_fire_incd",             
   "med_sale_res_y",             "med_sale_res_n",             "med_sale_com_y",             "med_sale_com_n",            
   "med_sale_ind_y",             "med_sale_ind_n",                                      
   "sum_cost.y",                 "num_events.y",               "dist",                       "n_object",                  
   "animals_insects",            "building.related",           "construction",               "food",                      
   "others",                     "police.property",            "service.complaint",          "street_sidewalk",           
   "traffic_sig0l",              "trash",                      "trees_plants",               "water.leak",                
   "zoning_parking",             "n_request")
 
 RegData[RegData == 0] <- .01
 RegData[Col2Transform] <- log(RegData[Col2Transform])
 


# Drop response variable from dataset
drop <- c("sum_cost_pedestrian_events")
RegData <- RegData[, !names(RegData) %in% drop]

# Tweedie input must be matrix
TwdX<-data.matrix(RegData)






# Fit tweedie distribution
TwdFit<-HDtweedie(TwdX, TwdY, group=NULL, p=1.50)


# Show lambda fits on graph
print(TwdFit)
plot(TwdFit)


# 5-Fold Cross-Validation
CVTwd <- cv.HDtweedie(TwdX, TwdY)
plot(CVTwd)


CVTwd$lambda.min

# Output predicted Y values based on best CV
TwdPred<-predict(CVTwd, newx = TwdX, s = "lambda.min")

# Plot Predicted Y Values
plot(TwdPred)
# Plot Actual Y Values
plot(TwdY)





# Create Dataframe
TwdComp <- data.frame(TwdY, TwdPred, Cell_IDs)
names(TwdComp) <- c("Actual", "Tweedie_Prediction", "Cell_ID")


TwdComp$Residuals <- as.numeric(TwdY-TwdPred)
TweedieData <- TwdComp
# Hocus pocus magic line to split the colors
TweedieData <- TweedieData %>% mutate(cost_gt_pred = if_else(is.negative(TweedieData$Residuals) == FALSE, Actual, NULL))


TweediePlot<-ggplot(data = TweedieData) + 
  geom_point(aes(x = Tweedie_Prediction, y = Actual, colour = 'Negative Residuals')) +
  geom_point(aes(x = Tweedie_Prediction, y = cost_gt_pred, colour = "Positive Residuals")) +
  #stat_smooth(color = 'dodgerblue3', aes(x = Tweedie_Prediction, y = Actual), se=F, method = NO SUCH METHOD EXISTS TO MODEL TWEEDIE, fullrange = TRUE) + 
  scale_y_continuous(breaks = seq(0,5,.5)) +
  ylim(0,6) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,6) +
  ylab("Actual Sum Cost ($ in Millions)") +
  xlab("Predicted Sum Cost ($ in Millions)") +
  ggtitle("Predicted vs Actual Cost of Injury", subtitle = "Tweedie Compoung Poisson Model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") 

TweediePlot

setwd(home_dir)
setwd(plot_dir)

ggsave(filename = "Linear_Model_Tweedie_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
ggsave(filename = "Linear_Model_Tweedie.png", width = 16, height = 9, units = "in")



setwd(home_dir)
setwd(data_dir)

TweedieResidualCSV <- data.frame(Cell_ID = TweedieData$Cell_ID, Residuals = TweedieData$Residuals)
write.csv(TweedieResidualCSV, "Residuals_by_Gridcell_Tweedie_Model.csv", row.names = FALSE)


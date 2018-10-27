library(HDtweedie)
library(dplyr)
library(bindrcpp)




home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

set.seed(0737)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]


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
  "med_sale_ind_y",             "med_sale_ind_n",             "med_sale_pbo_y",                         
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
TwdComp <- data.frame(TwdY, TwdPred)
names(TwdComp) <- c("Actual", "Tweedie_Prediction")




# Hocus pocus magic line to split the colors
TwdPlot <- TwdComp %>% mutate(cost_gt_pred = ifelse(Actual > (1.10 * Tweedie_Prediction), Actual, NA))

# Plot predicted vs actual
par(bg = "lightgrey")
plot(Actual ~ Tweedie_Prediction, data = TwdPlot,
     col = "springgreen4", 
     xlim = c(0, 30),
     ylim = c(0, 30))
points(cost_gt_pred ~ Tweedie_Prediction, data = TwdPlot, col = "red4")
abline(a = 0, b = 1, col = "dodgerblue3")
#looks decent


TwdComp$Residuals <- TwdY-TwdPred
qplot(seq_along(TwdComp$Residuals), TwdComp$Residuals) #yikes?

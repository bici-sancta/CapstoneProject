

if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}
if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}

home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id_ZERO.csv"
df_model_w_cell_id_ZERO <- read.csv(infile,
                        stringsAsFactors = FALSE, header = TRUE)


set.seed(0737)


# Determine columns to drop which are not used in classification
# Cell ID removed due to being uninformative
drop <- c("cell_id")
df_tree <- df_model_w_cell_id_ZERO[, !names(df_model_w_cell_id_ZERO) %in% drop]

# # Summary
# library(skimr)
# skim(df_tree)
# 
#  # Plot Column of Choice
#  library(ggplot2)
#  
#  Column <- "sum_cost_pedestrian_events"
#  qplot(seq_along(df_tree[[Column]]), df_tree[[Column]])
#  
#  qplot(seq_along(df_tree[[Column]]), df_tree[[Column]]) + scale_y_continuous(breaks = seq(0,90,1))
#  
  qplot(df_tree[[Column]]) + geom_histogram(binwidth = 1)



# Data Categorization

df_tree$sum_cost_pedestrian_events <- cut(df_tree$sum_cost_pedestrian_events,
                                          breaks = c(-Inf,0,1,3,9,30),
                                          labels = c("0 Cost","0<Cost<=1","1<Cost<=3","3<Cost<=9","More than 9"))

# df_tree$num_near_misses <- cut(df_tree$num_near_misses,
#                                breaks = c(-Inf,0,1,2,Inf),
#                                labels = c("No Near Misses","1 Near Miss","2 Near Misses","More than 2 Near Misses"))
# 
# df_tree$n_rqst <- cut(df_tree$n_rqst,
#                                breaks = c(-Inf,0,1,2,Inf),
#                                labels = c("No Requests","1 Request","2 Requests","More than 2 Requests"))
# 
# df_tree$access <- cut(df_tree$access,
#                       breaks = c(-Inf,0,1,Inf),
#                       labels = c("No Access","1 Access","More than 1 Access"))
# 
# df_tree$dblprk <- cut(df_tree$dblprk,
#                       breaks = c(-Inf,0,Inf),
#                       labels = c("0","More than 0"))



ClassTree = rpart(sum_cost_pedestrian_events ~ ., data = df_tree, control=rpart.control(cp=0.0102249 ))
printcp(ClassTree)

setwd(home_dir)
setwd(plot_dir)
#Plot Training Decision Tree 
rpart.plot(ClassTree, box.palette = "-RdYlGn", extra = "auto")
pdf("DecisionTree.pdf", width = 11, height = 8.5)
rpart.plot(ClassTree, box.palette = "-RdYlGn", extra = "auto")
dev.off()

# The best decision tree has a root node error of .23308 and a cross validation error of .96012 resulting in a 22.4% misclassification rate in cross validation.
# It cannot classify the 3-9 million dollar in sum cost category (the data is very sparse there) or the more than 9 cost castegory.

# Log Transform


setwd(home_dir)
setwd(data_dir)



infile <- "df_model_w_cell_id_ZERO.csv"
df_model_w_cell_id_ZERO <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id")
df_tree2 <- df_model_w_cell_id_ZERO[, !names(df_model_w_cell_id_ZERO) %in% drop]


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

df_tree2[df_tree2 == 0] <- .01

df_tree2[Col2Transform] <- log(df_tree2[Col2Transform])


 # Summary
 library(skimr)
 skim(df_tree2)
 
  # Plot Column of Choice
  library(ggplot2)
  
  Column <- "sum_cost_pedestrian_events"
  qplot(seq_along(df_tree2[[Column]]), df_tree2[[Column]])
  
  qplot(seq_along(df_tree2[[Column]]), df_tree2[[Column]]) + scale_y_continuous(breaks = seq(-90,90,.5))
  
qplot(df_tree2[[Column]]) + geom_histogram(binwidth = 1)
  


df_tree2$sum_cost_pedestrian_events <- cut(df_tree2$sum_cost_pedestrian_events,
                                          breaks = c(-Inf,-4,-1.5,-.5,.5,2,Inf),
                                          labels = c("LogCost<-4","-4<LogCost<=-1.5","-1.5<LogCost<=-.5","-.5<LogCost<=.5",".5<LogCost<=2","2<LogCost"))



ClassTree2 = rpart(sum_cost_pedestrian_events ~ ., data = df_tree2, control=rpart.control(cp=0.0061350))
printcp(ClassTree2)

setwd(home_dir)
setwd(plot_dir)
#Plot Training Decision Tree 
rpart.plot(ClassTree2, box.palette = "-RdYlGn", extra = "auto")
pdf("DecisionTreeLog.pdf", width = 11, height = 8.5)
rpart.plot(ClassTree2, box.palette = "-RdYlGn", extra = "auto")
dev.off()

# The best decision tree has a root node error of .23308 and a cross validation error of .96524 resulting in a 22.5% misclassification rate in cross validation.
# It cannot classify the -4 to -1.5 log sum cost category or the -1.5 to -.5 log sum cost category.

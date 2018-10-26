

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

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                        stringsAsFactors = FALSE, header = TRUE)


# Determine columns to drop which are not used in classification
# Cell ID removed due to being uninformative
# Sum cost removed due to being extremely correlated to response variable
drop <- c("cell_id", "sum_cost.y")
df_tree <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

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



ClassTree = rpart(sum_cost_pedestrian_events ~ ., data = df_tree, control=rpart.control(cp=.005))
printcp(ClassTree)

setwd(home_dir)
setwd(plot_dir)
#Plot Training Decision Tree 
rpart.plot(ClassTree, box.palette = "-RdYlGn", extra = "auto")
pdf("DecisionTree.pdf", width = 7, height = 7)
rpart.plot(ClassTree, box.palette = "-RdYlGn", extra = "auto")
dev.off()

# The best decision tree has a root node error of .23308 and a cross validation error of .89264 resulting in a 22.8% misclassification rate in cross validation.
# It cannot classify the 3-9 million dollar in sum cost category (the data is very sparse there).



#
## Trying to subset dataset based on removing 0 cost cells to see if it will reduce overfitting.
#

df_tree2 <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

df_tree2 <- df_tree2[!df_tree2$sum_cost_pedestrian_events == 0,]

  # Plot Column of Choice
  library(ggplot2)
  
  Column <- "sum_cost_pedestrian_events"
  qplot(seq_along(df_tree2[[Column]]), df_tree2[[Column]])
  
  qplot(seq_along(df_tree2[[Column]]), df_tree2[[Column]]) + scale_y_continuous(breaks = seq(0,90,1))
  
  qplot(df_tree2[[Column]]) + geom_histogram(binwidth = 1) + scale_x_continuous(breaks = seq(0,90,.5))


  # Data Categorization
  
  df_tree2$sum_cost_pedestrian_events <- cut(df_tree2$sum_cost_pedestrian_events,
                                            breaks = c(-Inf,1,2,3,9,20,Inf),
                                            labels = c("0<Cost<=1","1<Cost<=2","2<Cost<=3","3<Cost<=9","9<Cost<=20","More than 20"))


  
  ClassTree2 = rpart(sum_cost_pedestrian_events ~ ., data = df_tree2, control=rpart.control(cp=.0001))
  printcp(ClassTree2)
  rpart.plot(ClassTree2, box.palette = "-RdYlGn", extra = "auto")
  
# Overfitting is now worse. 0/10. Do not recommend.
  
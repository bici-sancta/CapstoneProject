

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

infile <- "df_model.csv"
df_model <- read.csv(infile,
                        stringsAsFactors = FALSE, header = TRUE)

drop <- c("predict", "cost_gt_pred", "PSI")
df_tree <- df_model[, !names(df_model) %in% drop]


ClassTree = rpart(sum_cost ~ ., data = df_tree, method = "anova")

setwd(home_dir)
setwd(plot_dir)
#Plot Training Decision Tree 
prp(ClassTree, extra = 1)
pdf("TreeTest.pdf", width = 7, height = 7)
prp(ClassTree, extra = 1)
dev.off()


# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   file : qtwLx_rforest.R
# ...
# ...   Random Forest Model -  code for qtwLx
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   14-aug-2018
# ...
# ...   MSDS 7332  - quantifying the world
# ...   Summer 2018
# ...   Final Assignment
# ...
# ...   patrick.mcdevitt@smu.edu
# ...
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

library(h2o)
library(randomForest)
library(randomForestExplainer)
library(tictoc)

# ...https://www.analyticsvidhya.com/blog/2016/05/h2o-data-table-build-models-large-data-sets/

library(caret)
library(MASS)
library(dplyr)

printf <- function(...) invisible(cat(sprintf(...)))


#home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")
data_dir <- ("./data/")
plot_dir <- ("./plots/")

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# drop cell_id column

drop <- c("cell_id")
df_model <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

#no_log <- c("mean_walk_score", "min_walk_score", "max_walk_score") 

#df_subset1 <- df_model[, !names(df_model) %in% no_log]
#df_subset2 <- df_model[, names(df_model) %in% no_log]

#df_subset1 <- log(df_subset1[,1:length(df_subset1)]+0.01)

#df_model <- cbind(df_subset1, df_subset2)

all_but_first <- seq(2, length(df_model))
first_to_last <- c(all_but_first, 1)
df_model <- df_model[, first_to_last]

df_binary <- df_model

df_binary$binary <- ifelse(df_binary$sum_cost_pedestrian_events > 0, 1, 0)
df_binary$sum_cost_pedestrian_events <- NULL

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

qtwLx_random_forest <- df_binary

# ...   75 / 25 split for train / test

train_size <- floor(0.75 * nrow(qtwLx_random_forest))

# ...   set seed for reproducibiluty

set.seed(20181026)

train_indx <- sample(seq_len(nrow(qtwLx_random_forest)), size = train_size)

qtwLx_hrf_train <- qtwLx_random_forest[train_indx, ]
qtwLx_hrf_test <- qtwLx_random_forest[-train_indx, ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   spool up h20 for faster processing
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

localH2O <- h2o.init(nthreads = -1)
h2o.init()

train.h2o <- as.h2o(qtwLx_hrf_train)
test.h2o <- as.h2o(qtwLx_hrf_test)

x.indep <- c(1 : (length(train.h2o) - 1))
y.dep <- length(train.h2o)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   Create a Random Forest model with default parameters
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

tic("model 1.h2o")
model_binary.h2o <- h2o.randomForest(y = y.dep,
                                  x = x.indep,
                                  training_frame = train.h2o)
toc()


VI_rf_binary <- h2o.varimp(model_cost.h2o)
h2o.performance(model_binary.h2o)

h2o.varimp_plot(model_binary.h2o)

# ...   predict ... test

train.h2o$pred <- h2o.predict(model_binary.h2o, train.h2o)
test.h2o$pred <- h2o.predict(model_binary.h2o, test.h2o)

df_tmp <- as.data.frame(train.h2o)
qtwLx_hrf_train$hrf_prob <- df_tmp$pred
rm(df_tmp)

df_tmp <- as.data.frame(test.h2o)
qtwLx_hrf_test$hrf_prob <- df_tmp$pred
rm(df_tmp)

#plot(sum_cost_pedestrian_events ~ hrf_prob, data = qtwLx_hrf_test, col = "firebrick")
plot(jitter(binary) ~ hrf_prob, data = qtwLx_hrf_test, col = "firebrick")
abline(0,1)

#plot(sum_cost_pedestrian_events ~ hrf_prob, data = qtwLx_hrf_train, col = "dodgerblue3")
plot(jitter(binary) ~ hrf_prob, data = qtwLx_hrf_train, col = "dodgerblue3")
abline(0,1)

fp_lst <- numeric()
ac_lst <- numeric()
tpr_hrf <- numeric()
fpr_hrf <- numeric()

for (it in seq(1:99))
{
    ones <- qtwLx_hrf_test$hrf_prob >= it/100
    qtwLx_hrf_test$pred <- 0
    qtwLx_hrf_test$pred[ones] <- 1
#    df_test$error <- df_test$target_y - df_test$y_hat
#    table(df_test$error)
    
    cnf <- confusionMatrix(data = as.factor(qtwLx_hrf_test$pred),
                    reference = as.factor(qtwLx_hrf_test$binary),
                    positive = "1")
    fp <- cnf$table[2]
    
    printf("th = %6.2f : fp = %6.4f : acc = %6.4f\n", it/100, fp/dim(qtwLx_hrf_test)[1], cnf$overall[[1]])
    fp_lst[it] <- fp / dim(qtwLx_hrf_test)[1]
    ac_lst[it] <- cnf$overall[[1]]
    
    tpr_hrf[it] <- cnf$byClass[[1]]
    fpr_hrf[it] <- 1.0 - cnf$byClass[[2]]
}

plot(tpr_hrf ~ fpr_hrf,
     xlim = c(0, 1),
     ylim = c(0, 1),
     col = "grey",
     cex = 0.25,
     xlab = "False Positive Rate",
     ylab = "True Positive Rate",
     main = "Random Forest - ROC")
grid (NULL,NULL, lty = 6, col = "lightgrey") 
lines(tpr_hrf ~ fpr_hrf, col = "dodgerblue3", lwd = 4)


plot(jitter(qtwLx_hrf_test$binary) ~ jitter(qtwLx_hrf_test$hrf_prob),
     xlab = "Model Predicted Classification",
     ylab = "Observed Classification",
     main = "Random Forest - Class Distributions vs. Target",
     sub = "(data values jittered for observability)",
     cex = 0.5,
     col = alpha("dodgerblue3", 0.9))

qtwLx_hrf_test$pred <- ifelse(qtwLx_hrf_test$hrf_prob > 0.2, 1, 0)
qtwLx_hrf_train$pred <- ifelse(qtwLx_hrf_train$hrf_prob > 0.2, 1, 0)

df_model$binary_pred <- 0

df_model$binary_pred[train_indx] <- qtwLx_hrf_train$pred
df_model$binary_pred[-train_indx] <- qtwLx_hrf_test$pred


df_model_binary_results <- data.frame(df_model_w_cell_id$cell_id)
df_model_binary_results <- cbind(df_model_binary_results, data.frame(df_model$binary_pred))

names(df_model_binary_results) <- c("cell_id", "binary_predictor")

file_name <- "random_forest_binary_predictor.csv"
write.table(df_model_binary_results, file = file_name, sep = ",",
            row.names = FALSE,
            col.names = TRUE)


df_cost <- df_model[df_model$binary_pred == 1,]

no_log <- c("mean_walk_score", "min_walk_score", "max_walk_score") 

df_subset1 <- df_cost[, !names(df_cost) %in% no_log]
df_subset2 <- df_cost[, names(df_cost) %in% no_log]

df_subset1 <- log(df_subset1[,1:length(df_subset1)]+0.01)

df_cost <- cbind(df_subset1, df_subset2)

df_cost$cost <- df_cost$sum_cost_pedestrian_events

df_cost$binary_pred <- NULL
df_cost$sum_cost_pedestrian_events <- NULL

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   Round Two
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

qtwLx_random_forest <- df_cost

# ...   75 / 25 split for train / test

train_size <- floor(0.75 * nrow(qtwLx_random_forest))

# ...   set seed for reproducibiluty

set.seed(20181026)

train_indx <- sample(seq_len(nrow(qtwLx_random_forest)), size = train_size)

qtwLx_hrf_train <- qtwLx_random_forest[train_indx, ]
qtwLx_hrf_test <- qtwLx_random_forest[-train_indx, ]

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   spool up h20 for faster processing
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

localH2O <- h2o.init(nthreads = -1)
h2o.init()

train.h2o <- as.h2o(qtwLx_hrf_train)
test.h2o <- as.h2o(qtwLx_hrf_test)

x.indep <- c(1 : (length(train.h2o) - 1))
y.dep <- length(train.h2o)

# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ...   Create a Random Forest model with default parameters
# ...   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

tic("model 1.h2o")
model_cost.h2o <- h2o.randomForest(y = y.dep,
                                  x = x.indep,
                                  training_frame = train.h2o)
toc()

# ...   predict ... test

train.h2o$pred <- h2o.predict(model_cost.h2o, train.h2o)
test.h2o$pred <- h2o.predict(model_cost.h2o, test.h2o)

df_tmp <- as.data.frame(train.h2o)
qtwLx_hrf_train$hrf_prob <- df_tmp$pred
rm(df_tmp)

df_tmp <- as.data.frame(test.h2o)
qtwLx_hrf_test$hrf_prob <- df_tmp$pred
rm(df_tmp)

plot((cost) ~ hrf_prob, data = qtwLx_hrf_test, col = "firebrick")
abline(0,1)

plot((cost) ~ hrf_prob, data = qtwLx_hrf_train, col = "dodgerblue3")
abline(0,1)

VI_rf_cost <- h2o.varimp(model_cost.h2o)

h2o.varimp_plot(model_cost.h2o)




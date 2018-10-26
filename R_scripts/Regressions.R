
library(caret)
library(MASS)
library(dplyr)

home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id", "sum_cost.y", "num_events.y")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]










# Output large results to txt file
sink("LRModel.fullint.txt")
print(summary(lm(sum_cost_pedestrian_events ~ (.)^2, RegData)))
sink()

LRModel.fullint <- lm(sum_cost_pedestrian_events ~ (.)^2, RegData)

sink("LRModel.full.txt")
print(summary(lm(sum_cost_pedestrian_events ~ ., RegData)))
sink()
LRModel.full <- lm(sum_cost_pedestrian_events ~ ., RegData)


# Generate predictions
RegData$predict <- predict(LRModel.fullint, RegData)


# Create prediction plot
RegData <- RegData %>% mutate(cost_gt_pred = ifelse(sum_cost_pedestrian_events > (1.10 * predict), sum_cost_pedestrian_events, NA))

par(bg = "lightgrey")
plot(sum_cost_pedestrian_events ~ predict, data = RegData,
     col = "springgreen4", 
     xlim = c(0, 30),
     ylim = c(0, 30))
points(cost_gt_pred ~ predict, data = RegData, col = "red4")
abline(a = 0, b = 1, col = "dodgerblue3")


# Stepwise regressions.

# Output results to txt file
sink("LRModel.full.step.txt")
print(summary(stepAIC(LRModel.full, direction = "both", trace = FALSE)))
sink()
# Also have results in R
LRModel.step <- stepAIC(LRModel.full, direction = "both", trace = FALSE)

# Output results to txt file
sink("LRModel.full.forward.txt")
print(summary(stepAIC(LRModel.full, direction = "forward", trace = FALSE)))
sink()
# Also have results in R
LRModel.forward <- stepAIC(LRModel.full, direction = "forward", trace = FALSE)

# Output results to txt file
sink("LRModel.full.backward.txt")
print(summary(stepAIC(LRModel.full, direction = "backward", trace = FALSE)))
sink()
# Also have results in R
LRModel.backward <- stepAIC(LRModel.full, direction = "backward", trace = FALSE)



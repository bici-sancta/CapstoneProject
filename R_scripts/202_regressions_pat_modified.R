
library(caret)
library(MASS)
library(dplyr)

#home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
home_dir <- ("/home/mcdevitt/_ds/_smu/_src/CapstoneProject/")

data_dir <- ("./data/")
plot_dir <- ("./plots/")
grid_mapped_dir <- ("./data/grid_mapped")



setwd(home_dir)
setwd(data_dir)

infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

setwd(home_dir)
setwd(grid_mapped_dir)
infile <- "ped_crash_cost_kernel_distribution.csv"
crash_cost_krnl <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

crash_cost_krnl <- crash_cost_krnl[crash_cost_krnl$kernelized_cost < 1000,]

drop <- c("lat", "long")
crash_cost_krnl <- crash_cost_krnl[, !names(crash_cost_krnl) %in% drop]

# ...   merge costs into df_model

drop <- c("sum_cost_pedestrian_events")
df_model <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]

df_rgr <- merge(crash_cost_krnl, df_model, by = "cell_id")

# Add column names to list drop to be dropped from dataset.
drop <- c("cell_id")
df_rgr <- df_rgr[, !names(df_rgr) %in% drop]


# ...   ln transforms

walk_names <- c("mean_walk_score", "min_walk_score", "max_walk_score")
df_walk <- df_rgr[, names(df_rgr) %in% walk_names]
df_trnsfrm <- df_rgr[, !names(df_rgr) %in% walk_names]

df_trnsfrm <- log(df_trnsfrm[, 1:length(df_trnsfrm)] + 0.001)

df_rgr <- cbind(df_trnsfrm, df_walk)

df_rgr_scaled <- as.data.frame(scale(df_rgr))

fit1 <- lm(kernelized_cost ~ ., data = df_rgr_scaled)
summary(fit1)


df_rgr_scaled$pred <- predict(fit1, df_rgr_scaled)

plot(kernelized_cost ~ pred, data = df_rgr_scaled)
abline(0, 1)

df_rgr$krnl_exp <- exp(df_rgr$kernelized_cost)
df_rgr$pred_exp <- exp(df_rgr$pred)
plot(krnl_exp ~ pred_exp, data = df_rgr)
abline(0, 1)

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



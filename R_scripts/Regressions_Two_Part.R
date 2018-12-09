library(MASS)
library(data.table)

home_dir <- ("G:/JoshuaData/Classes/MSDS61X0 Capstone/CapstoneProject")
data_dir <- ("./data/")
plot_dir <- ("./plots/")


setwd(home_dir)
setwd(data_dir)


infile <- "df_model_w_cell_id.csv"
df_model_w_cell_id <- read.csv(infile,
                               stringsAsFactors = FALSE, header = TRUE)

# Drop unused columns
drop <- c("num_pedestrian_events")
RegData <- df_model_w_cell_id[, !names(df_model_w_cell_id) %in% drop]
set.seed(0737)



RegDataD <- as.data.table(RegData)
RegDataL <- RegDataD[, logkrnl_cost_pedestrian_events := ifelse(krnl_cost_pedestrian_events > 0, log(krnl_cost_pedestrian_events), NA)]
ggplot(RegDataL, aes(x = logkrnl_cost_pedestrian_events)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "White") +
  stat_function(fun = dnorm, args = fitdistr(RegDataL[krnl_cost_pedestrian_events > 0 , logkrnl_cost_pedestrian_events],
                                             "normal")$estimate)


RegDataL[, sample := sample(c("train", "test"), nrow(RegDataL), replace = TRUE)]



xvars <- c("num_near_misses", "access", "dblprk", "dnotyld",
                                "jywalk", "lvisib", "lwfws", "nobikef", "noswlk", "other", "prkint", "prkswlk", "speed", "vrrlss", "wlksig", "xwalk",
                                "assist", "bikes", "drives", "other.1", "walks", "n_rqst", "mean_walk_score", "min_walk_score", "max_walk_score", 
                                "num_walk_scores", "sum_lane_cnt", "sum_width", "sum_area", "num_streets", "num_fire_incd", "med_sale_res_y", 
                                "med_sale_res_n", "med_sale_com_y", "med_sale_com_n", "med_sale_ind_y", "med_sale_ind_n", "med_sale_pbo_y", 
                                "med_sale_pbo_n", "sum_cost_non_pedestrian_events", "num_non_pedestrian_events", "dist", "n_object",
                                "animals_insects", "building.related", "construction", "food", "others", "police.property", "service.complaint", 
                                "street_sidewalk", "traffic_signal", "trash", "trees_plants", "water.leak", "zoning_parking", "n_request")

Fm <- function(y, xvars){
  return(as.formula(paste(y, "~", paste(xvars, collapse = "+"))))
}

RegDataL[, d_krnl_cost_pedestrian_events := ifelse(krnl_cost_pedestrian_events == 0, 0, 1)] # indicator for positive spending

# part 1
logistic.fit <- glm(Fm("d_krnl_cost_pedestrian_events", xvars), RegDataL[sample == "train"], family = binomial)

# part 2
ols.fit <- lm(Fm("krnl_cost_pedestrian_events", xvars), RegDataL[krnl_cost_pedestrian_events > 0 & sample == "train"])
logols.fit <- lm(Fm("logkrnl_cost_pedestrian_events", xvars), RegDataL[krnl_cost_pedestrian_events > 0 & sample == "train"])
gamma.fit <- glm(Fm("krnl_cost_pedestrian_events", xvars), RegDataL[krnl_cost_pedestrian_events > 0 & sample == "train"], 
                 family = Gamma(link = log))


phat <- predict(logistic.fit, RegDataL[sample == "test"], type = "response")
pred <- data.table(krnl_cost_pedestrian_events = RegDataL[sample == "test", krnl_cost_pedestrian_events])
pred$ols <- phat * predict(ols.fit, RegDataL[sample == "test"])
pred$logols <- phat * exp(predict(logols.fit, RegDataL[sample == "test"]) + summary(logols.fit)$sigma^2/2)
pred$gamma <- phat * predict(gamma.fit, RegDataL[sample == "test"], type = "response")

RMSE <- function(x, y)  sqrt(mean((y - x)^2, na.rm = TRUE))
rmse <- c(RMSE(pred$krnl_cost_pedestrian_events, pred$ols),
          RMSE(pred$krnl_cost_pedestrian_events, pred$logols),
          RMSE(pred$krnl_cost_pedestrian_events, pred$gamma))
names(rmse) <- c("OLS", "Log OLS", "Gamma")
print(rmse)

#logistic-normal test
n <- nrow(RegDataL[sample == "test"])
d <- rbinom(n, 1, phat)
y.norm <- d * rnorm(n, pred$ols, summary(ols.fit)$sigma)
#logistic-lognormal test
y.lognorm <- d * rlnorm(n, predict(logols.fit, RegDataL[sample == "test"]) , 
                        summary(logols.fit)$sigma)
#gamma test
res <- (gamma.fit$model$krnl_cost_pedestrian_events - gamma.fit$fit)/gamma.fit$fit # this is equivalent to gamma.fit$res
c(sum(res^2)/gamma.fit$df.res, summary(gamma.fit)$dispersion)

a <- gamma.shape(gamma.fit)$alpha
b <- a/pred$gamma
y.gamma <- d * rgamma(n, shape = a , rate = b)

#Prediction Accuracy Plots
y <- RegDataL[sample == "test", krnl_cost_pedestrian_events]
p.dat <- data.table(y = c(y, y.norm, y.lognorm, y.gamma),
                    lab = c(rep("Observed", n), rep("Normal", n), 
                            rep("Lognormal", n), rep("Gamma", n)))
p <- ggplot(p.dat[y > 0 & y < 10000], aes(x = y, col = lab)) + 
  geom_density(kernel = "gaussian") +
  xlab("Kernal Cost") + ylab("Density") +
  theme(legend.position="bottom") + labs(col = "") +
  scale_color_manual(values=c(Observed = "black", Normal = "red", 
                              Lognormal = "blue", Gamma = "green")) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,5)


print(p)

MySum <- function(x){
  q <- c(0.30, 0.5, 0.75, .9, .95, .98)
  dat <- c(100 * mean(x == 0, na.rm = TRUE),
           min(x, na.rm = TRUE), quantile(x, probs = q, na.rm = TRUE), 
           max(x, na.rm = TRUE))
  names(dat) <- c("PercentZero", "Min", paste0("Q", 100 * q), "Max")
  return(round(dat, 0))
} 
sumstats <- rbind(MySum(y), MySum(y.norm), 
                  MySum(y.lognorm), MySum(y.gamma))
rownames(sumstats) <- c("Observed", "Normal", "Lognormal", "Gamma")
print(sumstats)




# Generate Dataset for Residuals
RegDataPred <- data.table(krnl_cost_pedestrian_events = RegDataL[sample == "test", krnl_cost_pedestrian_events])
RegDataPred$ols <- phat * predict(ols.fit, RegDataL[sample == "test"])
#RegDataPred$logols <- phat * exp(predict(logols.fit, RegDataL[sample == "test"]) + summary(logols.fit)$sigma^2/2)
#RegDataPred$gamma <- phat * predict(gamma.fit, RegDataL[sample == "test"], type = "response")

RegDataPred$Residuals<-RegDataPred$krnl_cost_pedestrian_events - RegDataPred$ols

RegDataPred <- RegDataPred %>% mutate(cost_gt_pred = if_else(is.negative(RegDataPred$Residuals) == FALSE, krnl_cost_pedestrian_events, NULL))

setwd(home_dir)
setwd(plot_dir)

TwoPart <- ggplot(data = RegDataPred) + 
  geom_point(aes(x = ols, y = krnl_cost_pedestrian_events, colour = 'Negative Residuals')) +
  geom_point(aes(x = ols, y = cost_gt_pred, colour = "Positive Residuals")) +
  #stat_smooth(color = 'dodgerblue3', aes(x = predict, y = krnl_cost_pedestrian_events), se=F, method = lm, fullrange = TRUE) +
  scale_y_continuous(breaks = seq(0,5,.5)) +
  ylim(0,5) +
  scale_x_continuous(breaks = seq(0,5,.5)) +
  xlim(0,5) +
  ylab("Observed Sum Cost ($ in Millions)") +
  xlab("Expected Sum Cost ($ in Millions)") +
  ggtitle("Expected vs Observed Cost of Incident", subtitle = "Two-Part Model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.8,0.3)) + #Centers title and subtitle
  scale_color_manual(values = c('Negative Residuals' = 'springgreen4', 'Positive Residuals' = 'red4'), name = "Points") +
  theme(text = element_text(size = 25)) 

TwoPart  

ggsave(filename = "Linear_Two_Part_Model_HD.png", width = 24, height = 13.4, units = "in", dpi = 100.13)
ggsave(filename = "Linear_Two_Part_Model.png", width = 16, height = 9, units = "in")



#Output residual and cells for visualization later
PSIFullFinalResiduals <- RegDataPred$krnl_cost_pedestrian_events - RegDataPred$predict
PSIFullFinalcell_id <- RegDataPred$cell_id

setwd(home_dir)
setwd(data_dir)

ResidualCSV <- data.frame(Cell_ID = PSIFullFinalcell_id, Residuals = PSIFullFinalResiduals)
write.csv(ResidualCSV, "Residuals_by_Gridcell_Two_Part_Model.csv", row.names = FALSE)



#Import libraries
library(ggplot2)
library(forecast)
library(mvpart)
library(MultivariateRandomForest)
library(randomForestSRC)
source("Evaluation.R")
library(randomForest)

data <- read.csv("Data/data.csv")
train <- read.csv("Data/train.csv")


#Simple model tuning for ntree and mtry unsing grid search
ntree <- c(100,300,500,700,900)
mtry <- c(5,6,7,8,9,10)
var_grid <- expand.grid(ntree, mtry)

test <- train[(ceiling(nrow(train)*0.8)):nrow(train),]
train <- train[1:(floor(nrow(train)*0.8)),]


cases_result_df <- data.frame(Correlation = c(),
                              MAE = c(),
                              RSquared = c(),
                              Model = c(),
                              V = c())

deaths_result_df <- data.frame(Correlation = c(),
                               MAE = c(),
                               RSquared = c(),
                               Model = c(),
                               V = c())
for(v in 1:nrow(var_grid)){
  print(v)
  ntree <- var_grid$Var1[v]
  mtry <- var_grid$Var2[v]
  
  rf1 <- data.frame(build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                         testX= as.matrix(test[,2:54])))
  colnames(rf1) <- c("cases_pred","deaths_pred")
  cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test$cases_1Ahead,"RF1",v))
  deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test$deaths_1Ahead,"RF1",v))
  
  
  
  
  
  rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                                        testX= as.matrix(test[,2:54])))
  colnames(rf2) <- c("cases_pred","deaths_pred")
  cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test$cases_1Ahead,"RF2",v))
  deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test$deaths_1Ahead,"RF2",v))
  
  
  rf <- rfsrc (Multivar(cases_1Ahead, deaths_1Ahead)~., train[,2:56], ntree = ntree, mtry = mtry)
  rf_predict <- predict(rf, newdata=test, type="matrix")
  rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_1Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_1Ahead$predicted)
  cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test$cases_1Ahead,"RFSRC",v))
  deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test$deaths_1Ahead,"RFSRC",v))
  
  #Individual Random Forest
  cases_rf <- randomForest(cases_1Ahead~., data = train[,2:55], importance = T, ntree = ntree, mtry = mtry)
  cases_rf_pred <- predict(cases_rf, test[,2:55])
  cases_result_df <- rbind(cases_result_df,eval_func(cases_rf_pred, test$cases_1Ahead,"Basic RF",v))
  
  
  deaths_rf <- randomForest(deaths_1Ahead~., data = train[,c(2:54,56)], ntree = ntree, mtry = mtry)
  deaths_rf_pred <- predict(deaths_rf, test[,c(2:54,56)])
  deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test$deaths_1Ahead,"Basic RF",v))

}

tune_results <- data.frame(c_cor = c(),
                           c_mae = c(),
                           c_rsq = c(),
                           d_cor = c(),
                           d_mae = c(),
                           d_rsq = c(),
                           model = c())

for(m in unique(cases_result_df$Model)){
  temp <- cases_result_df[cases_result_df$Model == m,]
  best <- c(temp[order(-temp$Correlation),]$V[1],
            temp[order(temp$MAE),]$V[1],
            temp[order(-temp$RSquared),]$V[1])
  
  temp <- deaths_result_df[deaths_result_df$Model == m,]
  best <- c(best, c(temp[order(-temp$Correlation),]$V[1],
                    temp[order(temp$MAE),]$V[1],
                    temp[order(-temp$RSquared),]$V[1]))
  best <- data.frame(matrix(best, nrow = 1))
  best$model <- m
  colnames(best) <- c("c_cor", "c_mae", "c_rsq", "d_cor", "d_mae", "d_rsq", "model")
  tune_results <- rbind(tune_results, best)
  
}


train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")

#Plot cases and deaths
data$date <- as.Date(data$date, fomat = ("%Y-%m-%d"))
ggplot(data, aes(x = date, y = cases)) +
  geom_line() +
  xlab("Date") + 
  ylab("Cases")

ggplot(data, aes(x = date, y = deaths)) +
  geom_line() +
  xlab("Date") + 
  ylab("Deaths")

cases_result_df <- data.frame(Correlation = c(),
                        MAE = c(),
                        RSquared = c(),
                        Model = c())

deaths_result_df <- data.frame(Correlation = c(),
                              MAE = c(),
                              RSquared = c(),
                              Model = c())

#NULL-Model
cases_null <- test$cases
deaths_null <- test$deaths

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test$cases_1Ahead, "NULL"))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test$deaths_1Ahead, "NULL"))

#Autoarima
##Cases
plot(test$cases, main = "Graph without forecasting",
     col.main = "darkgreen", type = "l")

arima_cases <- auto.arima(train$cases)
arima_pred_cases <- forecast(arima_cases, 50)
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases$mean, test$cases, "ARIMA"))


# plot(arima_pred_cases, main = "Graph with forecasting",
#      col.main = "darkgreen") 
# ggplot(data = data.frame(x = 1:50, cases = test$cases,
#                          pred = arima_pred_cases$mean), aes(x = x)) +
#   geom_line(aes(y=cases, color="Cases")) +
#   geom_line(aes(y=pred, color="Predictions")) + 
#   labs(color="")

##Deaths
plot(test$deaths, main = "Graph without forecasting",
     col.main = "darkgreen", type = "l")

arima_deaths <- auto.arima(train$deaths)
arima_pred_deaths <- forecast(arima_deaths, 50)
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths$mean, test$deaths, "ARIMA"))

# plot(arima_pred_deaths, main = "Graph with forecasting",
#      col.main = "darkgreen") 
# ggplot(data = data.frame(x = 1:50, deaths = test$deaths,
#                          pred = arima_pred_deaths$mean), aes(x = x)) +
#   geom_line(aes(y=deaths, color="Cases")) +
#   geom_line(aes(y=pred, color="Predictions")) + 
#   labs(color="")

#Basic linear regression
cases_lm <- glm(cases_1Ahead~., data = train[,2:55])
summary(cases_lm)
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test$cases_1Ahead,"Linear Regression"))

deaths_lm <-  glm(deaths_1Ahead~., data = train[,c(2:54,56)])
summary(deaths_lm)
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test$deaths_1Ahead, "Linear Regression"))


#SARIMAX
##Cases

sarimax_cases <- auto.arima(train$cases_1Ahead, xreg = as.matrix(train[,c(5,6,23:54)]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,c(5,6,23:54)]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test$cases,"SARIMAX"))


##Deaths
sarimax_deaths <- auto.arima(train$deaths_1Ahead, xreg = as.matrix(train[,c(5,6,23:54)]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,c(5,6,23:54)]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test$deaths, "SARIMAX"))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#
##
### MultivariateRandomForest and IntegratedMRF
##
#
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                            testX= as.matrix(test[,2:54])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test$cases_1Ahead,"RF1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test$deaths_1Ahead,"RF1"))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                           testX= as.matrix(test[,2:54])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test$cases_1Ahead,"RF2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test$deaths_1Ahead,"RF2"))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,55:56])~.,
                      train[,2:54], prn=TRUE, method="mrt", size=3)
# plot(mvpart_run1)
# text(mvpart_run1)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test$cases_1Ahead,"MVPart1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test$deaths_1Ahead,"MVPart1"))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,55:56], meth="euc")~.,
                      train[,2:54], method="mrt",prn=TRUE,  size=3)
# plot(mvpart_run2)
# text(mvpart_run2)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test$cases_1Ahead,"MVPart2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test$deaths_1Ahead,"MVPart2"))
# "dist"
mvpart_run3 <- mvpart(gdist(train[,55:56], meth="euc",full=TRUE, sq=TRUE)~.,
                      train[,2:54], method="dist",prn=TRUE,  size=3)
# plot(mvpart_run3)
# text(mvpart_run3)
# mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
# colnames(mv_predict3) <- c("cases_pred","deaths_pred")
# eval_func(mv_predict3$cases_pred, test$cases)
# eval_func(mv_predict3$deaths_pred, test$death)

#
##
### rfsrc
##
#
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_1Ahead, deaths_1Ahead)~., train[,2:56], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_1Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_1Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test$cases_1Ahead,"RFSRC"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test$deaths_1Ahead,"RFSRC"))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_1Ahead~., data = train[,2:55], importance = T, ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,2:55])
cases_result_df <- rbind(cases_result_df,eval_func(cases_rf_pred, test$cases_1Ahead,"Basic RF"))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_1Ahead~., data = train[,c(2:54,56)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,c(2:54,56)])
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test$deaths_1Ahead,"Basic RF"))






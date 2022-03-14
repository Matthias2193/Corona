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
rf1 <- data.frame(build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = 10, m_feature = 3, min_leaf=5,
                            testX= as.matrix(test[,2:54])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test$cases_1Ahead,"RF1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test$deaths_1Ahead,"RF1"))





rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,2:54]), as.matrix(train[,55:56]), n_tree = 10, m_feature = 3, min_leaf=5,
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
rf <- rfsrc (Multivar(cases_1Ahead, deaths_1Ahead)~., train[,2:56])
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_1Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_1Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test$cases_1Ahead,"RFSRC"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test$deaths_1Ahead,"RFSRC"))

#Individual Random Forest
cases_rf <- randomForest(cases_1Ahead~., data = train[,2:55], importance = T)
cases_rf_pred <- predict(cases_rf, test[,2:55])
cases_result_df <- rbind(cases_result_df,eval_func(cases_rf_pred, test$cases_1Ahead,"Basic RF"))


deaths_rf <- randomForest(deaths_1Ahead~., data = train[,c(2:54,56)])
deaths_rf_pred <- predict(deaths_rf, test[,c(2:54,56)])
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test$deaths_1Ahead,"Basic RF"))


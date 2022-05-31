#Import libraries
library(ggplot2)
library(forecast)
library(mvpart)
library(MultivariateRandomForest)
library(randomForestSRC)
source("Evaluation.R")
source("Helper.R")
library(randomForest)


load(file = "tuning.RData")

ahead <- 7
lag <- 7
add <- c("measures14_select","variants", "vaccine", "tests")
scaling <- F
cases_target <- paste("cases_", ahead, "Ahead", sep = "")
deaths_target <- paste("deaths_", ahead, "Ahead", sep = "")

ntree <- c(100,300,500,700,900)
mtry <- c(5,6,7,8,9,10)
var_grid <- expand.grid(ntree, mtry)

data <- get_data(add = add, scale = scaling, ahead = ahead, lag = lag)
train <- data$train
test <- data$test
if(scaling){
  cases_scaler <- data$cases_scaler
  deaths_scaler <- data$deaths_scaler
} else{
  cases_scaler <- 1
  deaths_scaler <- 1
}

feature_colnames <- colnames(train)[!(colnames(train) %in% c("date","cases_1Ahead","deaths_1Ahead", "cases_7Ahead", "deaths_7Ahead"))]

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

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test[,cases_target], "NULL", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test[,deaths_target], "NULL", scale_value = deaths_scaler))

#Autoarima
##Cases
arima_pred_cases <- c()
for(x in 1:nrow(test)){
  arima_cases <- auto.arima(c(train$cases,test$cases[1:x]))
  arima_pred_cases <- c(arima_pred_cases,tail(forecast(arima_cases, 7)$mean,1))
}
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases, test[,cases_target], "ARIMA", scale_value = cases_scaler))

##Deaths
arima_pred_deaths <- c()
for(x in 1:nrow(test)){
  arima_deaths <- auto.arima(c(train$deaths,test$deaths[1:x]))
  arima_pred_deaths <- c(arima_pred_deaths,tail(forecast(arima_deaths, 7)$mean,1))
}
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths, test[,deaths_target], "ARIMA", scale_value = deaths_scaler))

#Basic linear regression
cases_lm <- glm(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)])
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test[,cases_target],"Linear Regression", scale_value = cases_scaler))

deaths_lm <-  glm(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)])
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test[,deaths_target], "Linear Regression", scale_value = deaths_scaler))


#SARIMAX
##Cases
sarimax_cases <- auto.arima(train[,cases_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,feature_colnames]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test[,cases_target],"SARIMAX", scale_value = cases_scaler))


##Deaths
sarimax_deaths <- auto.arima(train[,deaths_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,feature_colnames]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test[,deaths_target], "SARIMAX", scale_value = deaths_scaler))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#MultivariateRandomForest and IntegratedMRF
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                            testX= as.matrix(test[,feature_colnames])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1", scale_value = deaths_scaler))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                           testX= as.matrix(test[,feature_colnames])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2", scale_value = deaths_scaler))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                      train[,feature_colnames], prn=TRUE, method="mrt", size=10)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1", scale_value = deaths_scaler))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                      train[,feature_colnames], method="mrt",prn=TRUE,  size=10)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
temp_test <- cmds.diss(test[,c(cases_target, deaths_target)])
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2", scale_value = deaths_scaler))

# "dist"
mvpart_run3 <- mvpart(gdist(train[,c(cases_target, deaths_target)], meth="euc",full=TRUE, sq=TRUE)~.,
                      train[,feature_colnames], method="dist",prn=TRUE,  size=3)
mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
colnames(mv_predict3) <- c("cases_pred","deaths_pred")
eval_func(mv_predict3$cases_pred, test$cases)
eval_func(mv_predict3$deaths_pred, test$death)

# rfsrc
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_7Ahead, deaths_7Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_7Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_7Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC", scale_value = cases_scaler))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC", scale_value = deaths_scaler))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)], ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames)])
cases_results <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF", scale_value = cases_scaler))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,feature_colnames])
deaths_results <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF", scale_value = deaths_scaler))

cases_results$ahead <- deaths_results$ahead <- ahead
cases_results$lag <- deaths_results$lag <- lag
cases_results$scaling <- deaths_results$scaling <- scaling
cases_results$add <- deaths_results$add <- paste(add, collapse = "_")

save(cases_results, file = paste(paste("Results/cases", ahead, lag, scaling, paste(add, collapse = "_"), sep = "_"),".RData",sep = ""))
save(deaths_results, file = paste(paste("Results/deaths", ahead, lag, scaling, paste(add, collapse = "_"), sep = "_"),".RData",sep = ""))

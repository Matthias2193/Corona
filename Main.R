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

#Set target
cases_target <- "cases_7Ahead"
deaths_target <- "deaths_7Ahead"

feature_colnames <- colnames(train)[!(colnames(train) %in% c("date","cases_1Ahead","deaths_1Ahead", "cases_7Ahead", "deaths_7Ahead"))]

#Tuning ----

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

size <- c(5,10,20,50,100)
xv <- c("1se", "min",
       "pick", "none")
mv_grid <- expand.grid(size, xv)
for(v in 1:nrow(mv_grid)){
  print(v)
  size <- mv_grid$Var1[v]
  xv <- mv_grid$Var2[v]
  mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                        train[,feature_colnames], method="mrt", size=size, xv = xv)
  mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
  colnames(mv_predict1) <- c("cases_pred","deaths_pred")
  cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1",v))
  deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1",v))
  
  
  # cmds.diss (Classical scaling of Dissimilarity Measures)
  mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                        train[,feature_colnames], method="mrt",  size=size, xv = xv)
  mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
  colnames(mv_predict2) <- c("cases_pred","deaths_pred")
  cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2",v))
  deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2",v))
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
  
# for(v in 1:nrow(var_grid)){
#   print(v)
#   ntree <- var_grid$Var1[v]
#   mtry <- var_grid$Var2[v]
#   
#   rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, 
#                                          m_feature = mtry, min_leaf=5, testX= as.matrix(test[,feature_colnames])))
#   colnames(rf1) <- c("cases_pred","deaths_pred")
#   cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1",v))
#   deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1",v))
#   
#   
#   
#   
#   
#   rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
#                                                         testX= as.matrix(test[,feature_colnames])))
#   colnames(rf2) <- c("cases_pred","deaths_pred")
#   cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2",v))
#   deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2",v))
#   
#   
#   rf <- rfsrc(Multivar(cases_7Ahead, deaths_7Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
#   rf_predict <- predict(rf, newdata=test, type="matrix")
#   rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput[[cases_target]]$predicted, deaths_pred = rf_predict$regrOutput[[deaths_target]]$predicted)
#   cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC",v))
#   deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC",v))
#   
#   #Individual Random Forest
#   cases_rf <- randomForest(as.formula(paste(cases_target,".", sep="~")), data = train[,c(feature_colnames, cases_target)], importance = T, ntree = ntree, mtry = mtry)
#   cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames, cases_target)])
#   cases_result_df <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF",v))
#   
#   
#   deaths_rf <- randomForest(as.formula(paste(deaths_target,".", sep="~")), data = train[,c(feature_colnames,deaths_target)], ntree = ntree, mtry = mtry)
#   deaths_rf_pred <- predict(deaths_rf, test[,c(feature_colnames,deaths_target)])
#   deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF",v))
# 
# }
# # save(cases_result_df, file = "Cases.RData")
# # save(deaths_result_df, file = "Deaths.RData")
# 
# tune_results <- data.frame(c_cor = c(),
#                            c_mae = c(),
#                            c_rsq = c(),
#                            d_cor = c(),
#                            d_mae = c(),
#                            d_rsq = c(),
#                            model = c())
# 
# for(m in unique(cases_result_df$Model)){
#   temp <- cases_result_df[cases_result_df$Model == m,]
#   best <- c(temp[order(-temp$Correlation),]$V[1],
#             temp[order(temp$MAE),]$V[1],
#             temp[order(-temp$RSquared),]$V[1])
#   
#   temp <- deaths_result_df[deaths_result_df$Model == m,]
#   best <- c(best, c(temp[order(-temp$Correlation),]$V[1],
#                     temp[order(temp$MAE),]$V[1],
#                     temp[order(-temp$RSquared),]$V[1]))
#   best <- data.frame(matrix(best, nrow = 1))
#   best$model <- m
#   colnames(best) <- c("c_cor", "c_mae", "c_rsq", "d_cor", "d_mae", "d_rsq", "model")
#   tune_results <- rbind(tune_results, best)
#   
# }

#save(tune_results, file = "tuning.RData")
load(file = "tuning.RData")

#Plot
data$cases <- data$cases/max(data$cases)
data$deaths <- data$deaths/max(data$deaths)

#Plot cases and deaths
data$date <- as.Date(data$date, fomat = ("%Y-%m-%d"))
ggplot(data, aes(x = date)) +
  geom_line(aes(y = deaths, color = 'Deaths'), size = 1) +
  geom_line(aes(y = cases, color = 'Cases'), size = 1) +
  xlab("Date") + 
  ylab("Scaled Cases and Deaths") +
  labs(title = "COVID-19 Cases and Deaths in Germany") + 
  scale_color_manual(values=c("blue", "red")) +
  theme_minimal() +
  theme(text = element_text(size = 32)) +
  guides(color = guide_legend(title = "", size = 40))



ggplot(data, aes(x = date, y = deaths)) +
  geom_line() +
  xlab("Date") + 
  ylab("Deaths")


#7 days ahead ----

train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")
cases_target <- "cases_7Ahead"
deaths_target <- "deaths_7Ahead"

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

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test[,cases_target], "NULL"))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test[,deaths_target], "NULL"))

#Autoarima
##Cases
arima_pred_cases <- c()
for(x in 1:nrow(test)){
  arima_cases <- auto.arima(c(train$cases,test$cases[1:x]))
  arima_pred_cases <- c(arima_pred_cases,tail(forecast(arima_cases, 7)$mean,1))
}
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases, test[,cases_target], "ARIMA"))

##Deaths
arima_pred_deaths <- c()
for(x in 1:nrow(test)){
  arima_deaths <- auto.arima(c(train$deaths,test$deaths[1:x]))
  arima_pred_deaths <- c(arima_pred_deaths,tail(forecast(arima_deaths, 7)$mean,1))
}
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths, test[,deaths_target], "ARIMA"))

#Basic linear regression
cases_lm <- glm(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)])
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test[,cases_target],"Linear Regression"))

deaths_lm <-  glm(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)])
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test[,deaths_target], "Linear Regression"))


#SARIMAX
##Cases
sarimax_cases <- auto.arima(train[,cases_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,feature_colnames]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test[,cases_target],"SARIMAX"))


##Deaths
sarimax_deaths <- auto.arima(train[,deaths_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,feature_colnames]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test[,deaths_target], "SARIMAX"))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#MultivariateRandomForest and IntegratedMRF
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                            testX= as.matrix(test[,feature_colnames])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1"))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                           testX= as.matrix(test[,feature_colnames])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2"))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                      train[,feature_colnames], prn=TRUE, method="mrt", size=10)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1"))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                      train[,feature_colnames], method="mrt",prn=TRUE,  size=10)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2"))

# "dist"
mvpart_run3 <- mvpart(gdist(train[,c(cases_target, deaths_target)], meth="euc",full=TRUE, sq=TRUE)~.,
                      train[,feature_colnames], method="dist",prn=TRUE,  size=3)
# mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
# colnames(mv_predict3) <- c("cases_pred","deaths_pred")
# eval_func(mv_predict3$cases_pred, test$cases)
# eval_func(mv_predict3$deaths_pred, test$death)

# rfsrc
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_7Ahead, deaths_7Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_7Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_7Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC"))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)], ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames)])
cases_result_df_7 <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF"))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,feature_colnames])
deaths_result_df_7 <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF"))



#1 day ahead----
cases_target <- "cases_1Ahead"
deaths_target <- "deaths_1Ahead"

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

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test[,cases_target], "NULL"))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test[,deaths_target], "NULL"))

#Autoarima
##Cases
arima_pred_cases <- c()
for(x in 1:nrow(test)){
  arima_cases <- auto.arima(c(train$cases,test$cases[1:x]))
  arima_pred_cases <- c(arima_pred_cases,forecast(arima_cases, 1)$mean)
}
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases, test[,cases_target], "ARIMA"))

##Deaths
arima_pred_deaths <- c()
for(x in 1:nrow(test)){
  arima_deaths <- auto.arima(c(train$deaths,test$deaths[1:x]))
  arima_pred_deaths <- c(arima_pred_deaths,forecast(arima_deaths, 1)$mean)
}
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths, test[,deaths_target], "ARIMA"))

#Basic linear regression
cases_lm <- glm(cases_1Ahead~., data = train[,c(feature_colnames, cases_target)])
#summary(cases_lm)
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test[,cases_target],"Linear Regression"))

deaths_lm <-  glm(deaths_1Ahead~., data = train[,c(feature_colnames, deaths_target)])
#summary(deaths_lm)
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test[,deaths_target], "Linear Regression"))


#SARIMAX
##Cases

sarimax_cases <- auto.arima(train[,cases_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,feature_colnames]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test[,cases_target],"SARIMAX"))


##Deaths
sarimax_deaths <- auto.arima(train[,deaths_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,feature_colnames]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test[,deaths_target], "SARIMAX"))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#MultivariateRandomForest and IntegratedMRF
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                       testX= as.matrix(test[,feature_colnames])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1"))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                                      testX= as.matrix(test[,feature_colnames])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2"))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                      train[,feature_colnames], method="mrt", size=20)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1"))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                      train[,feature_colnames], method="mrt",  size=20)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2"))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2"))
# "dist"
# mvpart_run3 <- mvpart(gdist(train[,c(cases_target, deaths_target)], meth="euc", sq=TRUE)~.,
#                       train[,feature_colnames], method="dist",  size=10)
# mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
# colnames(mv_predict3) <- c("cases_pred","deaths_pred")
# eval_func(mv_predict3$cases_pred, test$cases)
# eval_func(mv_predict3$deaths_pred, test$deaths)

#rfsrc
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_1Ahead, deaths_1Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_1Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_1Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC"))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC"))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_1Ahead~., data = train[,c(feature_colnames, cases_target)], importance = T, ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames)])
cases_result_df_1 <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF"))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_1Ahead~., data = train[,c(feature_colnames,deaths_target)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,feature_colnames])
deaths_result_df_1 <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF"))



###Scaled

load(file = "tuning.RData")
#7 days ahead ----

train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")
cases_target <- "cases_7Ahead"
deaths_target <- "deaths_7Ahead"

cases_result_df <- data.frame(Correlation = c(),
                              MAE = c(),
                              RSquared = c(),
                              Model = c())

deaths_result_df <- data.frame(Correlation = c(),
                               MAE = c(),
                               RSquared = c(),
                               Model = c())

scale_value_cases <- max(train$cases)
scale_value_deaths <- max(train$deaths)

train$cases <- train$cases/scale_value_cases
train[,grepl("lag_cases",colnames(train))] <- train[,grepl("lag_cases",colnames(train))]/scale_value_cases
train[,c("cases_1Ahead","cases_7Ahead")] <- train[,c("cases_1Ahead","cases_7Ahead")]/scale_value_cases

train$deaths <- train$deaths/scale_value_deaths
train[,grepl("lag_deaths",colnames(train))] <- train[,grepl("lag_deaths",colnames(train))]/scale_value_deaths
train[,c("deaths_1Ahead","deaths_7Ahead")] <- train[,c("deaths_1Ahead","deaths_7Ahead")]/scale_value_deaths

test$cases <- test$cases/scale_value_cases
test[,grepl("lag_cases",colnames(test))] <- test[,grepl("lag_cases",colnames(test))]/scale_value_cases

test$deaths <- test$deaths/scale_value_deaths
test[,grepl("lag_deaths",colnames(test))] <- test[,grepl("lag_deaths",colnames(test))]/scale_value_deaths
#NULL-Model
cases_null <- test$cases
deaths_null <- test$deaths

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test[,cases_target], "NULL",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test[,deaths_target], "NULL",scale_value = scale_value_deaths))

#Autoarima
##Cases
arima_pred_cases <- c()
for(x in 1:nrow(test)){
  arima_cases <- auto.arima(c(train$cases,test$cases[1:x]))
  arima_pred_cases <- c(arima_pred_cases,tail(forecast(arima_cases, 7)$mean,1))
}
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases, test[,cases_target], "ARIMA",scale_value = scale_value_cases))

##Deaths
arima_pred_deaths <- c()
for(x in 1:nrow(test)){
  arima_deaths <- auto.arima(c(train$deaths,test$deaths[1:x]))
  arima_pred_deaths <- c(arima_pred_deaths,tail(forecast(arima_deaths, 7)$mean,1))
}
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths, test[,deaths_target], "ARIMA",scale_value = scale_value_deaths))

#Basic linear regression
cases_lm <- glm(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)])
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test[,cases_target],"Linear Regression",scale_value = scale_value_cases))

deaths_lm <-  glm(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)])
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test[,deaths_target], "Linear Regression",scale_value = scale_value_deaths))


#SARIMAX
##Cases
sarimax_cases <- auto.arima(train[,cases_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,feature_colnames]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test[,cases_target],"SARIMAX",scale_value = scale_value_cases))


##Deaths
sarimax_deaths <- auto.arima(train[,deaths_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,feature_colnames]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test[,deaths_target], "SARIMAX",scale_value = scale_value_deaths))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#MultivariateRandomForest and IntegratedMRF
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                       testX= as.matrix(test[,feature_colnames])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1",scale_value = scale_value_deaths))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                                      testX= as.matrix(test[,feature_colnames])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2",scale_value = scale_value_deaths))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                      train[,feature_colnames], prn=TRUE, method="mrt", size=10)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1",scale_value = scale_value_deaths))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                      train[,feature_colnames], method="mrt",prn=TRUE,  size=10)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2",scale_value = scale_value_deaths))

# "dist"
mvpart_run3 <- mvpart(gdist(train[,c(cases_target, deaths_target)], meth="euc",full=TRUE, sq=TRUE)~.,
                      train[,feature_colnames], method="dist",prn=TRUE,  size=3)
# mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
# colnames(mv_predict3) <- c("cases_pred","deaths_pred")
# eval_func(mv_predict3$cases_pred, test$cases)
# eval_func(mv_predict3$deaths_pred, test$death)

# rfsrc
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_7Ahead, deaths_7Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_7Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_7Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC",scale_value = scale_value_deaths))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_7Ahead~., data = train[,c(feature_colnames, cases_target)], ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames)])
cases_result_df_7_scaled <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF",scale_value = scale_value_cases))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_7Ahead~., data = train[,c(feature_colnames, deaths_target)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,feature_colnames])
deaths_result_df_7_scaled <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF",scale_value = scale_value_deaths))



#1 day ahead----
cases_target <- "cases_1Ahead"
deaths_target <- "deaths_1Ahead"

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

cases_result_df <- rbind(cases_result_df,eval_func(cases_null, test[,cases_target], "NULL",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(deaths_null, test[,deaths_target], "NULL",scale_value = scale_value_deaths))

#Autoarima
##Cases
arima_pred_cases <- c()
for(x in 1:nrow(test)){
  arima_cases <- auto.arima(c(train$cases,test$cases[1:x]))
  arima_pred_cases <- c(arima_pred_cases,forecast(arima_cases, 1)$mean)
}
cases_result_df <- rbind(cases_result_df,eval_func(arima_pred_cases, test[,cases_target], "ARIMA",scale_value = scale_value_cases))

##Deaths
arima_pred_deaths <- c()
for(x in 1:nrow(test)){
  arima_deaths <- auto.arima(c(train$deaths,test$deaths[1:x]))
  arima_pred_deaths <- c(arima_pred_deaths,forecast(arima_deaths, 1)$mean)
}
deaths_result_df <- rbind(deaths_result_df,eval_func(arima_pred_deaths, test[,deaths_target], "ARIMA",scale_value = scale_value_deaths))

#Basic linear regression
cases_lm <- glm(cases_1Ahead~., data = train[,c(feature_colnames, cases_target)])
#summary(cases_lm)
cases_lm_pred <- predict(cases_lm, test)
cases_result_df <- rbind(cases_result_df, eval_func(cases_lm_pred, test[,cases_target],"Linear Regression",scale_value = scale_value_cases))

deaths_lm <-  glm(deaths_1Ahead~., data = train[,c(feature_colnames, deaths_target)])
#summary(deaths_lm)
deaths_lm_pred <- predict(deaths_lm, test)
deaths_result_df <- rbind(deaths_result_df, eval_func(deaths_lm_pred, test[,deaths_target], "Linear Regression",scale_value = scale_value_deaths))


#SARIMAX
##Cases

sarimax_cases <- auto.arima(train[,cases_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_cases <- forecast(sarimax_cases, xreg = as.matrix(test[,feature_colnames]))
cases_result_df <- rbind(cases_result_df, eval_func(sarimax_pred_cases$mean, test[,cases_target],"SARIMAX",scale_value = scale_value_cases))


##Deaths
sarimax_deaths <- auto.arima(train[,deaths_target], xreg = as.matrix(train[,feature_colnames]))
sarimax_pred_deaths <- forecast(sarimax_deaths, xreg = as.matrix(test[,feature_colnames]))
deaths_result_df <- rbind(deaths_result_df, eval_func(sarimax_pred_deaths$mean, test[,deaths_target], "SARIMAX",scale_value = scale_value_deaths))




#Models from "Machine Learning for Multi-Output Regression: When should a holistic multivariate approach be preferred over separate univariate ones?"
#MultivariateRandomForest and IntegratedMRF
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[1,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf1 <- data.frame(build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                       testX= as.matrix(test[,feature_colnames])))
colnames(rf1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf1$cases_pred, test[,cases_target],"RF1",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf1$deaths_pred, test[,deaths_target],"RF1",scale_value = scale_value_deaths))




par_var <- as.numeric(names(sort(table(as.numeric(tune_results[2,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf2 <- data.frame(IntegratedMRF::build_forest_predict(as.matrix(train[,feature_colnames]), as.matrix(train[,c(cases_target, deaths_target)]), n_tree = ntree, m_feature = mtry, min_leaf=5,
                                                      testX= as.matrix(test[,feature_colnames])))
colnames(rf2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(rf2$cases_pred, test[,cases_target],"RF2",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf2$deaths_pred, test[,deaths_target],"RF2",scale_value = scale_value_deaths))



#MVPart
# method mrt (distance euc)
mvpart_run1 <- mvpart(data.matrix(train[,c(cases_target, deaths_target)])~.,
                      train[,feature_colnames], method="mrt", size=20)
mv_predict1 <- data.frame(predict(mvpart_run1, newdata=test, type="matrix"))
colnames(mv_predict1) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict1$cases_pred, test[,cases_target],"MVPart1",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict1$deaths_pred, test[,deaths_target],"MVPart1",scale_value = scale_value_deaths))


# cmds.diss (Classical scaling of Dissimilarity Measures)
mvpart_run2 <- mvpart(cmds.diss(train[,c(cases_target, deaths_target)], meth="euc")~.,
                      train[,feature_colnames], method="mrt",  size=20)
mv_predict2 <- data.frame(predict(mvpart_run2, newdata=test, type="matrix"))
colnames(mv_predict2) <- c("cases_pred","deaths_pred")
cases_result_df <- rbind(cases_result_df,eval_func(mv_predict2$cases_pred, test[,cases_target],"MVPart2",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(mv_predict2$deaths_pred, test[,deaths_target],"MVPart2",scale_value = scale_value_deaths))
# "dist"
# mvpart_run3 <- mvpart(gdist(train[,c(cases_target, deaths_target)], meth="euc", sq=TRUE)~.,
#                       train[,feature_colnames], method="dist",  size=10)
# mv_predict3 <- data.frame(predict(mvpart_run3, newdata=test, type="matrix"))
# colnames(mv_predict3) <- c("cases_pred","deaths_pred")
# eval_func(mv_predict3$cases_pred, test$cases)
# eval_func(mv_predict3$deaths_pred, test$deaths)

#rfsrc
par_var <- as.numeric(names(sort(table(as.numeric(tune_results[3,1:6])), decreasing = T)[1]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
rf <- rfsrc (Multivar(cases_1Ahead, deaths_1Ahead)~., train[,c(feature_colnames, cases_target, deaths_target)], ntree = ntree, mtry = mtry)
rf_predict <- predict(rf, newdata=test, type="matrix")
rf_predictions <- data.frame(cases_pred = rf_predict$regrOutput$cases_1Ahead$predicted, deaths_pred = rf_predict$regrOutput$deaths_1Ahead$predicted)
cases_result_df <- rbind(cases_result_df,eval_func(rf_predictions$cases_pred, test[,cases_target],"RFSRC",scale_value = scale_value_cases))
deaths_result_df <- rbind(deaths_result_df,eval_func(rf_predictions$deaths_pred, test[,deaths_target],"RFSRC",scale_value = scale_value_deaths))

#Individual Random Forest
par_var <- median(as.numeric(tune_results[4,1:3]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
cases_rf <- randomForest(cases_1Ahead~., data = train[,c(feature_colnames, cases_target)], importance = T, ntree = ntree, mtry = mtry)
cases_rf_pred <- predict(cases_rf, test[,c(feature_colnames)])
cases_result_df_1_scaled <- rbind(cases_result_df,eval_func(cases_rf_pred, test[,cases_target],"Basic RF",scale_value = scale_value_cases))

par_var <- median(as.numeric(tune_results[4,4:6]))
pars <- as.numeric(var_grid[par_var,])
ntree <- pars[1]
mtry <- pars[2]
deaths_rf <- randomForest(deaths_1Ahead~., data = train[,c(feature_colnames,deaths_target)], ntree = ntree, mtry = mtry)
deaths_rf_pred <- predict(deaths_rf, test[,feature_colnames])
deaths_result_df_1_scaled <- rbind(deaths_result_df,eval_func(deaths_rf_pred, test[,deaths_target],"Basic RF",scale_value = scale_value_deaths))
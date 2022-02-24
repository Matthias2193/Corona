#Import libraries
library(ggplot2)
library(forecast)
library(mvpart)
#source("Preprocessing.R")
data <- read.csv("Data/Data.csv")
for(s in 1:5){
  data[paste("lag_cases",s,sep="")] <- c(rep(NA,s), data$cases[1:(nrow(data)-s)])
  data[paste("lag_deaths",s,sep="")] <- c(rep(NA,s), data$deaths[1:(nrow(data)-s)])
}
data <- na.omit(data)
data$X <- NULL
#Plot cases and deaths
ggplot(data, aes(x = date, y = cases)) +
  geom_line() +
  xlab("Date") + 
  ylab("Cases")

ggplot(data, aes(x = date, y = deaths)) +
  geom_line() +
  xlab("Date") + 
  ylab("Deaths")

#NULL-Model
cases_null <- data$cases[2:nrow(data)]
deaths_null <- data$deaths[2:nrow(data)]

#Autoarima
plot(data_min$cases[300:nrow(data_min)], main = "Graph without forecasting",
     col.main = "darkgreen", type = "line")

fit <- auto.arima(data_min$cases[0:(nrow(data_min)-50)])

# Next 10 forecasted values 
forecastedValues <- forecast(fit, 50)

# Print forecasted values
print(forecastedValues)

plot(forecastedValues, main = "Graph with forecasting",
     col.main = "darkgreen") 
ggplot(data = data.frame(x = 1:50, cases = data_min$cases[(nrow(data_min)-49):nrow(data_min)],
                         pred = forecastedValues$mean), aes(x = x)) +
  geom_line(aes(y=cases, color="Cases")) +
  geom_line(aes(y=pred, color="Predictions")) + 
  labs(color="")

#
##
### MultivariateRandomForest and IntegratedMRF
##
#
library(MultivariateRandomForest)


rf1 <- data.frame(build_forest_predict(as.matrix(data[1:300,8:160]), as.matrix(data[1:300,6:7]), n_tree = 10, m_feature = 3, min_leaf=5,
                            testX= as.matrix(data[301:375,8:160])))
colnames(rf1) <- c("cases_pred","deaths_pred")
rf1$cases <- data[301:375,]$cases
rf1$deaths <- data[301:375,]$deaths

rf2 <- IntegratedMRF::build_forest_predict(as.matrix(concrete[1:90,1:7]), as.matrix(concrete[1:90,8:9]), n_tree = 10, m_feature = 3, min_leaf=5,
                                           testX= as.matrix(concrete[91:103,1:7]))

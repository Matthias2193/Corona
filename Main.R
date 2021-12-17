#Import libraries
library(ggplot2)
library(forecast)
source("Preprocessing.R")

#Plot cases and deaths
ggplot(data, aes(x = date, y = cases)) +
  geom_line() +
  xlab("Date") + 
  ylab("Cases")

ggplot(data, aes(x = date, y = deaths)) +
  geom_line() +
  xlab("Date") + 
  ylab("Deaths")


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

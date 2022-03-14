#Import libraries
library(randomForest)
library(forecast)

#Load data
data <- read.csv("Data/data_cleaned.csv")

#Add Lag-variables for cases and deaths
for(s in 1:6){
  data[paste("lag_cases",s,sep="")] <- c(rep(NA,s), data$cases[1:(nrow(data)-s)])
  data[paste("lag_deaths",s,sep="")] <- c(rep(NA,s), data$deaths[1:(nrow(data)-s)])
}

#Add next day values as targets for prediction
data$cases_1Ahead <- c(data$cases[2:nrow(data)],NA)
data$deaths_1Ahead <- c(data$deaths[2:nrow(data)],NA)

#Format date
data$date <- as.Date(data$date, fomat = ("%Y-%m-%d"))

#Remove NAs
data <- na.omit(data)

#Train-Test split
train <- data[0:(nrow(data)-50),]
test <- tail(data,50)

#Remove constant columns in train and test
constant_columns <- apply(train[,9:ncol(train)], 2, is.constant)
train[,names(constant_columns[constant_columns])] <- NULL
test[,names(constant_columns[constant_columns])] <- NULL
train$ClosureOfPublicTransportPartial_duration <- NULL
test$ClosureOfPublicTransportPartial_duration <- NULL


#Use basic linear regression and remove all columns with NA estimates
cases_lm <- glm(cases_1Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead", "date")])
summary(cases_lm)
remove_cols <- c(is.na(cases_lm$coefficients),F,F)
train[,remove_cols] <- NULL
test[,remove_cols] <- NULL

#Random forests for feature importance
cases_rf <- randomForest(cases_1Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead", "date")], importance = T)
cases_importance <- importance(cases_rf)

deaths_rf <- randomForest(deaths_1Ahead~., data = train[,!colnames(train) %in% c("cases_1Ahead", "date")], importance = T)
deaths_importance <- importance(deaths_rf)

importance_df <- data.frame(IncMSE = (cases_importance[,1] + deaths_importance[,1])/2, IncPurity = (cases_importance[,2] + deaths_importance[,2])/2)

#Remove columns with negative IncMSE
train[,rownames(importance_df[importance_df$IncMSE<0,])] <- NULL
test[,rownames(importance_df[importance_df$IncMSE<0,])] <- NULL


#Save training and test data
write.csv(train, file = "Data/train.csv", row.names = F)
write.csv(test, file = "Data/test.csv", row.names = F)

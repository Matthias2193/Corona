#Import libraries
library(randomForest)
library(forecast)
library(tidyverse)
source("Helper.R")

#Load data
data <- read.csv("Data/data_cleaned.csv")

# #Add Lag-variables for cases and deaths
# for(s in 1:13){
#   data[paste("lag_cases",s,sep="")] <- c(rep(NA,s), data$cases[1:(nrow(data)-s)])
#   data[paste("lag_deaths",s,sep="")] <- c(rep(NA,s), data$deaths[1:(nrow(data)-s)])
# }
# 
# #Add next day values as targets for prediction
# data$cases_1Ahead <- c(data$cases[2:nrow(data)],NA)
# data$deaths_1Ahead <- c(data$deaths[2:nrow(data)],NA)
# 
#Add one week ahead
data$cases_7Ahead <- c(data$cases[8:nrow(data)],rep(NA,7))
data$deaths_7Ahead <- c(data$deaths[8:nrow(data)],rep(NA,7))

#Remove NAs
data <- na.omit(data)

#Remove duplicated columns
data <- data[,!duplicated(as.list(data))]

#Format date
data$date <- as.Date(data$date, fomat = ("%Y-%m-%d"))

#Train-Test split
train <- data[0:(nrow(data)-50),]
test <- tail(data,50)

#Remove constant columns in train and test
constant_columns <- apply(train[,2:ncol(train)], 2, is.constant)
train[,names(constant_columns[constant_columns])] <- NULL
test[,names(constant_columns[constant_columns])] <- NULL
train$ClosureOfPublicTransportPartial_duration <- NULL
test$ClosureOfPublicTransportPartial_duration <- NULL
data[,constant_columns] <- NULL

#Create several data sets
keep_cols <- c("date","year", "month", "cases", "deaths", "Montag", "Dienstag","Mittwoch","Donnerstag","Freitag","Samstag", "R_Wert","number_hosp")
# keep_cols <- c(keep_cols, colnames(data)[grepl("lag", colnames(data))])
# keep_cols <- c(keep_cols, colnames(data)[grepl("Ahead", colnames(data))])

data_base <- data[,keep_cols]
write.csv(data_base, file = "Data/data_base.csv", row.names = F)


#Use basic linear regression and remove all columns with NA estimates
cases_lm <- glm(cases_7Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date")])
summary(cases_lm)
remove_cols <- c(is.na(cases_lm$coefficients),F,F)
train[,remove_cols] <- NULL
test[,remove_cols] <- NULL
data[,remove_cols] <- NULL
# #Random forests for feature importance
# cases_rf <- randomForest(cases_7Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date")], 
#                          ntree = 1000, mtry = round(sqrt(sum(!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date"))),0), 
#                          importance = T)
# cases_importance <- importance(cases_rf)
# 
# deaths_rf <- randomForest(deaths_7Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead","cases_1Ahead","cases_7Ahead", "date")], 
#                           ntree = 1000, mtry = round(sqrt(sum(!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date"))),0), 
#                           importance = T)
# deaths_importance <- importance(deaths_rf)
# 
# importance_df <- data.frame(IncMSE = (cases_importance[,1] + deaths_importance[,1])/2, IncPurity = (cases_importance[,2] + deaths_importance[,2])/2)
# 
# #Remove columns with negative IncMSE
# train[,rownames(importance_df[importance_df$IncMSE<0,])] <- NULL
# test[,rownames(importance_df[importance_df$IncMSE<0,])] <- NULL

#Vaccine data
keep_cols <- c("cummulative_FirstDose", "cummulative_SecondDose","cummulative_DoseAdditional1")
data_vaccine <- data[,c("date",keep_cols)]
write.csv(data_vaccine, file = "Data/data_vaccine.csv", row.names = F)
data[,keep_cols] <- NULL

#Measures14
keep_cols <- colnames(data)[grepl("_14",colnames(data))]
data_measures14 <- data[,c("date", keep_cols)]
write.csv(data_measures14, file = "Data/data_measures14.csv", row.names = F)
data[,keep_cols] <- NULL


#Measures
keep_cols <- c()
for(cn in colnames(data)){
  if(is.logical(data[,cn])){
    keep_cols <- c(keep_cols,cn)
  }
}
data_measures <- data[,c("date", keep_cols)]
write.csv(data_measures, file = "Data/data_measures.csv", row.names = F)
data[,keep_cols] <- NULL

added_cols <- c("positivity_rate","B.1.1.529_percent_variant","ClosureOfPublicTransportPartial","MasksMandatoryAllSpacesPartial",
                "B.1.617.2_percent_variant","PrivateGatheringRestrictionsPartial",
                "ClosDaycarePartial", "ClosPrimPartial","EntertainmentVenues")

#Variants
keep_cols <- colnames(data)[grepl("variant",colnames(data))]
data_variants <- data[,c("date", keep_cols)]
write.csv(data_variants, file = "Data/data_variants.csv", row.names = F)

#Tests
keep_cols <- c("tests_done","positivity_rate")
data_tests <- data[,c("date", keep_cols)]
write.csv(data_tests, file = "Data/data_tests.csv", row.names = F)


data$date <- as.Date(data$date, fomat = ("%Y-%m-%d"))

#Train-Test split
ahead <- 7
lag <- 7
add <- c("measures14")
scaling <- F
cases_target <- paste("cases_", ahead, "Ahead", sep = "")
deaths_target <- paste("deaths_", ahead, "Ahead", sep = "")


data <- get_data(add = add, scale = scaling, ahead = ahead, lag = lag)
train <- data$train
test <- data$test

measures14_select <- read.csv("Data/data_measures14.csv")
cases_lm <- glm(cases_7Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date")])
lm_sum <- summary(cases_lm)
lm_coef <- lm_sum$coefficients
keep_cols <- rownames(lm_coef)[lm_coef[,4] < 0.05]
keep_cols <- keep_cols[grepl("TRUE",keep_cols)]
for(k in 1:length(keep_cols)){
  keep_cols[k] <- substr(keep_cols[k],1,(nchar(keep_cols[k])-4))
}
measures14_select <- measures14_select[c("date",keep_cols)]
write.csv(measures14_select,"Data/data_measures14_select.csv", row.names = F)


add <- c("measures")
data <- get_data(add = add, scale = scaling, ahead = ahead, lag = lag)
train <- data$train
test <- data$test

measures_select <- read.csv("Data/data_measures.csv")
cases_lm <- glm(cases_7Ahead~., data = train[,!colnames(train) %in% c("deaths_1Ahead","deaths_7Ahead","cases_1Ahead", "date")])
lm_sum <- summary(cases_lm)
lm_coef <- lm_sum$coefficients
keep_cols <- rownames(lm_coef)[lm_coef[,4] < 0.05]
keep_cols <- keep_cols[grepl("TRUE",keep_cols)]
for(k in 1:length(keep_cols)){
  keep_cols[k] <- substr(keep_cols[k],1,(nchar(keep_cols[k])-4))
}
measures_select <- measures_select[c("date",keep_cols)]
write.csv(measures_select,"Data/data_measures_select.csv", row.names = F)

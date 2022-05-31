get_data <- function(add = c(), scale = F, ahead = 7, lag = 7,scale_method="max"){
  data <- read.csv("Data/data_base.csv")
  if(scale){
    #Get scaling values from the training data
    cases_scaler <- max(data$cases[1:(length(data$cases)-50)])
    deaths_scaler <- max(data$deaths[1:(length(data$deaths)-50)])
    #Scale all values
    data$cases <- data$cases/cases_scaler
    data$deaths <- data$deaths/deaths_scaler
  }
  #Add Lag-variables for cases and deaths
  for(s in 1:ahead){
    data[paste("lag_cases",s,sep="")] <- c(rep(NA,s), data$cases[1:(nrow(data)-s)])
    data[paste("lag_deaths",s,sep="")] <- c(rep(NA,s), data$deaths[1:(nrow(data)-s)])
  }
  
  #Add next day values as targets for prediction
  data[paste("cases_", ahead,"Ahead",sep="")] <- c(data$cases[(ahead+1):nrow(data)],rep(NA, ahead))
  data[paste("deaths_", ahead,"Ahead",sep="")] <- c(data$deaths[(ahead+1):nrow(data)],rep(NA, ahead))
  
  data <- na.omit(data)
  
  for(a in add){
    data_add <- read.csv(paste("Data/data_",a,".csv",sep=""))
    data <- merge(data, data_add, by = "date", all.x = T)
  }
  train <- data[0:(nrow(data)-50),]
  test <- tail(data,50)
  if(scale){
    return(list("train" = train, "test" = test, "cases_scaler" = cases_scaler, "deaths_scaler" = deaths_scaler))
  } else{
    return(list("train" = train, "test" = test))
  }
  
}

#Function to evaluate the models
eval_func <- function(pred, y){
  #Correlation
  c <- cor(pred, y)
  
  #Mean Absoulte Error
  MAE <- mean(abs(pred - y))
  
  #R-Squared
  r_squared <- 1 - (sum((pred - y)^2)/sum((y - mean(y))^2))
  
  results <- matrix(c(c, MAE, r_squared), nrow = 1)
  colnames(results) <- c("Correlation", "MAE", "R-Squared")
  return(results)
}


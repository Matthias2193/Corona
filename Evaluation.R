#Function to evaluate the models
eval_func <- function(pred, y, model_name, v = NULL,scale_value = 1){
  pred = pred*scale_value
  y = y*scale_value
  #Correlation
  c <- cor(pred, y)
  
  #Mean Absoulte Error
  MAE <- mean(abs(pred - y))
  
  #R-Squared
  r_squared <- 1 - (sum((pred - y)^2)/sum((y - mean(y))^2))
  
  results <- data.frame(matrix(c(c, MAE, r_squared), nrow = 1))
  colnames(results) <- c("Correlation", "MAE", "RSquared")
  results$Model <- model_name
  if(!is.null(v)){
    results$V <- v
  }
  return(results)
}


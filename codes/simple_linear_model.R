
#Estimating Simple Linear Regression Coefficients and Confidence Intervals
Advertising = read.csv("data/Advertising.csv")


simple_linear_model <- function(data,predictor,target,level=.95){
  # predictor and target are strings of data column names
  x <- data[,predictor]
  y <- data[,target]
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  n=length(x)
  beta1 <- sum((x-x_bar)*(y-y_bar))/(sum((x-x_bar)**2))
  beta0 <- y_bar-beta1*x_bar
  
  # Confidence intervals for the estimators
  RSE <- sqrt(sum((y-beta0-beta1*x)**2)/(n-2))
  
  sx2 <- sum((x-x_bar)**2)
  
  b1_left <- beta1-qt((1+level)/2,df=n-2)*RSE*sx2**(-1/2)
  b1_right <- beta1+qt((1+level)/2,df=n-2)*RSE*sx2**(-1/2)
  
  b0_left <- beta0 - qt((1+level)/2,df=n-2)*RSE*sqrt(1/n+(x_bar**2)/sx2)
  b0_right <- beta0 + qt((1+level)/2,df=n-2)*RSE*sqrt(1/n+(x_bar**2)/sx2)
  
  confidence <- matrix(c(b0_left,b1_left,b0_right,b1_right),nrow=2)
  row.names(confidence) <- c("Intercept",predictor)
  colnames(confidence) <- paste0(100*c((1-level)/2,(1+level)/2),"%")
  return(list("Intercept"=beta0,
              predictor=beta1,
              confidence = confidence))
}

print(simple_linear_model(Advertising,"TV","Sales"))





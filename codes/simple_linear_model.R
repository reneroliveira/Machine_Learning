
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
  
  SE_b1 <- RSE*sx2**(-1/2)
  SE_b0 <- RSE*sqrt(1/n+(x_bar**2)/sx2)
  b1_left <- beta1 - qt((1+level)/2,df=n-2)*SE_b1
  b1_right <- beta1 + qt((1+level)/2,df=n-2)*SE_b1
  
  b0_left <- beta0 - qt((1+level)/2,df=n-2)*SE_b0
  b0_right <- beta0 + qt((1+level)/2,df=n-2)*SE_b0
  
  confidence <- matrix(c(b0_left,b1_left,b0_right,b1_right),nrow=2)
  row.names(confidence) <- c("Intercept",predictor)
  colnames(confidence) <- paste0(100*c((1-level)/2,(1+level)/2),"%")
  
  # Coefficients
  t0 <- beta0/SE_b0
  t1 <- beta1/SE_b1
  coefs <- matrix(c(beta0,beta1,
                    SE_b0,SE_b1,
                    t0,t1,
                    1-pt(abs(t0),df=n-2),
                    1-pt(abs(t1),df=n-2)),nrow=2,4)
  row.names(coefs) <- c("(Intercept)",predictor)
  colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(list(coefs=coefs,
              confidence = confidence))
}

simple_linear_model(Advertising,"TV","Sales")





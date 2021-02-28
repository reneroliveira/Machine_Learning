## Exercise 1.4

# Generating the dataset:


set.seed(42) #for reproducibility

N <- 20 #number of points
x_min=-10; x_max = 10; y_min=-10; y_max=10
data <- cbind(
  runif(N,x_min,x_max),
  runif(N,y_min,y_max)
)
x1 <- data[,1]
x2 <- data[,2]
a = 1.3;b=-1 # line parameters

## Sign {+1,-1} classification function (including 0 in +1 class)
classify = function(x){
  return(
    sapply(1:length(x),function(k){
    if(x[k]>=0){return(1)}
    else{return(-1)}
    })
  )
}
data <- cbind(data,classify(x2-x1*a-b)) #Appending y_n column


PLA = function(data,d=2,w0=c(0,0,0)){
  w = t(matrix(w0))
  data2=cbind(rep(1,nrow(data)),data)
  x = t(data2[,1:(d+1)])
  y_true = data2[,(d+2)]
  y_pred = classify(w %*% x)
  missed = which(y_true != y_pred)
  
  while(length(missed)!=0){
    i <- sample(missed,1)
    
    while(classify(w %*% x[,i])!=y_true[i]){
      w <- w+y_true[i]*x[,i]
    }
    y_pred <- classify(w %*% x)
    missed <- which(y_true != y_pred)
    
  }
  return (w)
  
}
t0 = Sys.time()
w = PLA(data)
delta_t=Sys.time()-t0

print(paste0("Execution Time: ",delta_t," ",attr(delta_t,"units")))
print(paste0("Real Angular Coefficient -> ",a))
print(paste0("PLA Angular Coefficient -> ",-w[2]/w[3]))
print(paste0("Real Linear Coefficient -> ",b))
print(paste0("PLA Linear Coefficient -> ",-w[1]/w[3]))

#Plotting
line_x=seq(x_min,x_max,length.out=100)
line_y=a*line_x+b

a2 = -w[2]/w[3];b2 = -w[1]/w[3]
line2_x=seq(x_min,x_max,length.out=100)
line2_y=a2*line2_x+b2

pos = which(data[,3]==1)
neg = which(data[,3]==-1)

png("figs/ex1.4_PLA.png")
plot(x1[pos],x2[pos],xlim=c(x_min,x_max),ylim=c(y_min,y_max),col="blue",xlab="",ylab="")
par(new=TRUE)
plot(x1[neg],x2[neg],xlim=c(x_min,x_max),ylim=c(y_min,y_max),xlab="x1",ylab="x2")
abline(h=0, v=0,lty=2)
lines(line_x,line_y,type="l",col="red",xlim=c(x_min,x_max),ylim=c(y_min,y_max))
lines(line2_x,line2_y,type="l",col="green",xlim=c(x_min,x_max),ylim=c(y_min,y_max))
dev.off()

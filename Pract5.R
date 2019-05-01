data = data.frame(c(1,1,1,1),c(0,0,1,1),c(0,1,0,1),c(0,1,1,1))
names(data) = c("X0","X1","X2","Y")
write.csv(data,file = "Bool.csv")


X_data<- read.csv(file = "Bool.csv")[,2:4]
Y_data<- read.csv(file = "Bool.csv")[,5]
Output = perceptron(X_data,Y_data,1)

perceptron <- function(X,Y,Eta){
  weights = rep(0,ncol(X))
  Conditions = rep(FALSE,nrow(X))
  #while(all(Conditions)!=TRUE){
  for(j in 1:100){
    print("hi")
    for(i in 1:nrow(X)){
      True_value = Y[i]
      Obtained_value = sum(weights*X[i,])
      Obtained_value = ifelse(Obtained_value<0,-1,1)

      weights_diff = Eta*(True_value-Obtained_value)*X[i,]
      weights = weights+weights_diff
    }
  }
  
  return(weights)
  }


data = data.frame(c(1,1,1,1),c(0,0,1,1),c(0,1,0,1),c(0,0,0,1))
names(data) = c("X0","X1","X2","Y")
write.csv(data,file = "Bool.csv")


X_data<- read.csv(file = "Bool.csv")[,2:4]
Y_data<- read.csv(file = "Bool.csv")[,5]
Output = perceptron(X_data,Y_data,1)

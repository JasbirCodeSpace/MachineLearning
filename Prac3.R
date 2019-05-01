library(MASS)
fix(Boston)
attach(Boston)

train_size = floor(0.70*nrow(Boston))
train_size

set.seed(1223)
train_index = sample(seq_len(nrow(Boston)),size = train_size) 

train_sample = Boston[train_index,]
test_sample = Boston[-train_index,]

#3(a)
Gradient_Decent = function(X,Y,iter,Beta,alpha=0.1){
  
  Result = data.frame()
  m = nrow(X)
  
  for(i in 1:iter) {
    
    J <- (1/(2*m))*sum(((Beta %*% t(X))-t(Y))^2)
    
    Beta <- Beta -(alpha/m)*(((Beta %*% t(X))-t(Y)) %*% as.matrix(X))
    
    Result =rbind(Result, c(J,Beta))
  }
  
  return(Result)
  
}


frame_data = Scaling(Boston)
names(frame_data) = names(Boston)
row = nrow(frame_data)
col = ncol(frame_data)

X = matrix(1,nrow = row)
X  = cbind(X,frame_data["lstat"])

Y = frame_data[,ncol(frame_data)]
X
Y
beta<-matrix(c(0,0),nrow=1,byrow = FALSE)
iter=10000
frame = Gradient_Decent(X,Y,iter,beta,0.1)

names =c("J(cost function)")
for(i in 1:ncol(beta)){
  names= c(names,c((i-1)))
}
names(frame) = c(names)


plot(c(1:iter),frame[,1],xlab = "Iterations",ylab = "Cost Function")
write.csv(frame,"Gradient-Boston.csv")

#or

lm.fit = lm(medv~lstat,data=frame_data)
lm.fit



#3(b)

model = lm(medv~lstat,train_sample)
predict_model = predict(model,test_sample)
predict_model
#(a)
plot(predict_model,test_sample[,"medv"],xlab ="Predicted Value",ylab = "Actual Value",main = "(a)")
#(b)
plot(Boston[,"medv"],Boston[,"lstat"],xlab = "MEDV",ylab = "LSTAT",main = "(b)")







#3(c)

model = lm(medv~.-age,train_sample)
predict_model = predict(model,test_sample)
predict_model

error <- function(y,yhat){
  return(sum((y-yhat)^2)/length(y))
}
error(test_sample[,"medv"],predict_model)







#3(d)

install.packages("neuralnet")
library(neuralnet)
Stand <- function(X){
  diff  = X - mean(X)
  std = sd(X)
  return(diff/std)
}

Scaling <- function(data_frame){
  new_data_frame = data.frame(1:nrow(data_frame))
  
  for(i in 1:ncol(data_frame)){
    new_data_frame = cbind(new_data_frame,Stand(data_frame[,i]))
  }
  
  return(new_data_frame[,2:ncol(new_data_frame)])
}

New_Boston = Scaling(Boston)
names(New_Boston) = names(Boston)
attach(New_Boston)

neural_network = neuralnet(medv~.,data=New_Boston,hidden = 3,act.fct = "logistic",linear.output = FALSE)
plot(neural_network)

predict_model = compute(neural_network,test_sample)
predict_model$net.result

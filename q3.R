library(MASS)
data(Boston)

attach(Boston)

partition = function(X=Boston){
  N = as.double(readline(prompt = "Enter the percentage for training sample : "))
  print(N)
  train_size = floor(N*nrow(X))
  set.seed(1223)
  
  index = sample(seq_len(nrow(X)),size = train_size)
  
  return(index)
}

train_index = partition()

train_sample = Boston[train_index,]
test_sample = Boston[-train_index,]


#################################### (a) #####################################

Gradient_decent <- function(X,Y,Beta,iter,alpha=0.1) {
  Result = data.frame()
  m = nrow(X)
  
  for(i in 1:iter){
    J  = (1/2*m)*sum((Beta%*%t(X)-t(Y))^2)
    Beta = Beta - (alpha/m)*((Beta%*%t(X)-t(Y))%*%as.matrix(X))
    Result = rbind(Result,c(J,Beta))
    
  }
  
  return(Result)
  
}

Boston = data.frame(scale(Boston))
X = matrix(1,nrow = nrow(Boston))
X = cbind(X,Boston["lstat"])

Y = Boston[,"medv"]

Beta = matrix(c(0,0),nrow = 1)

alpha =0.1
iter = 10000

Result = Gradient_decent(X,Y,Beta,iter,alpha)
fix(Result)

names(Result) = c("J","Beta 0 ","Beta 1")

write.csv(Result,"Result.csv")
plot(c(1:iter),Result[,1],xlab = "Iteration",ylab = "Cost Function")

beta_final = Result[10000,2:3]

original_medv = as.vector(Boston["medv"])
predicted_medv = as.vector(as.matrix(beta_final)%*%as.matrix(t(X)))

plot(predicted_medv,t(original_medv))
#3(b)
model = lm(medv~lstat,train_sample)
print(model)
summary(model)

predicted_model = predict(model,test_sample)
View(predicted_model)

plot(predicted_model,test_sample[,"medv"],xlab="Predicted medv values",ylab="Original medv values",col="RED",main = "")
plot(Boston[,"medv"],Boston[,"lstat"],xlab = "medv values",ylab="lstat values",col="green")


#3(c)
model = lm(medv~.-age,train_sample)
print(model)
summary(model)

predicted_model = predict(model,test_sample)
View(predicted_model)

error = function(Y,Yhat){
    error = sum((Y-Yhat)^2)/(length(Y))
    return(error)
}

cat("Error is : ",error(test_sample[,"medv"],predicted_model))

summary(test_sample[,"medv"],predicted_model$fitted.values)


#3(d)

Std = function(X){
  return((X-mean(X))/sd(X))
  
}

Scaling = function(data_frame){
  new_dataframe = data.frame(1:nrow(data_frame))
  for(i in 1:ncol(data_frame)){
    new_dataframe = cbind(new_dataframe,Std(data_frame[,i]))
    
  }
  
  return(new_dataframe[,-1])
}

data_frame = Scaling(Boston)
names(data_frame) = names(Boston)

train_sample = Scaling(train_sample)
names(train_sample) = names(Boston)

test_sample = Scaling(test_sample)
names(test_sample) = names(Boston)

library(neuralnet)
model = neuralnet(medv~.,data = train_sample,hidden = 3,linear.output = T)
print(model)
plot(model)


cat("predicted values : \n")
print(model$response)
print(model$result.matrix)

nn.results = compute(model,test_sample)

results = data.frame(actual=test_sample$medv,prediction=nn.results$net.result)
print(results)

roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
td = confusionMatrix(table(actual,prediction))
print(td$overall["Accuracy"]*100)


data("iris")
attach(iris)

iris_new = cbind(iris[,1:4],class.ind(iris[,5]))


index  = partition(iris_new) 

train_sample = data.frame(Scaling(iris_new[index,1:4]))
train_sample = cbind(train_sample,iris_new[index,5:7])
names(train_sample)=names(iris_new)

test_sample=data.frame(Scaling(iris_new[-index,1:4]))
test_sample = cbind(test_sample,iris_new[-index,5:7])
names(test_sample)=names(iris_new)


model = neuralnet(setosa+versicolor+virginica~.,train_sample,hidden = 3,linear.output = FALSE,act.fct = function(x) {1/(1 + exp(-x))})
print(model)
plot(model)

model$response
model$result.matrix

Predict = compute(model,test_sample[,1:4])
Prob = Predict$net.result
idx <- apply(Prob, 1, which.max)
predicted <- c('setosa', 'versicolor', 'virginica')[idx]
confusionMatrix(table(predicted, iris[-index,5]))


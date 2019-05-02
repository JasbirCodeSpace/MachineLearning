library(MASS)
data("Boston")

train_size = floor(0.7*nrow(Boston))

data <- as.data.frame(lapply(Boston,function(X){X = (X-mean(X))/sd(X)}))

train_index = sample(1:nrow(data), size = train_size)

train_set = data[train_index,]
test_set = data[-train_index,]

X <- matrix(c(1),nrow = nrow(train_set))
X <- cbind(X, train_set[,"lstat"])

Y <- matrix(train_set[,"medv"])


normal_eq(X,Y)
normal_eq_reg(X, Y, 0.3)


normal_eq = function(X, Y) {
  
  t = (t(X)%*%X)
  
  BETA = solve(t)%*%(t(X)%*%Y)
  
  T1 = matrix(1, nrow = nrow(test_set))
  T1 = cbind(T1,test_set[,"lstat"])
  
  predicted_values = t(BETA) %*% t(T1)
  
  plot(test_set[,"lstat"], predicted_values)
  
}



normal_eq_reg = function(X, Y, lambda) {
  
  L = diag(1, nrow = nrow(t(X)), ncol = ncol(X))
  L[1,1] = 0
  
  t = ((t(X)%*%X) + lambda*L)
  
  BETA = solve(t)%*%(t(X)%*%Y)
  T1 = matrix(1, nrow = nrow(test_set))
  T1 = cbind(T1,test_set[,"lstat"])
  predicted_values = t(BETA) %*% t(T1)
  plot(test_set[,"lstat"], predicted_values,col="RED")
  
}






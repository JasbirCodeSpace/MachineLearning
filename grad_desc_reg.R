library(MASS)
data("Boston")

train_size = floor(0.7*nrow(Boston))

data <- standerdize(Boston)

train_index = sample(1:nrow(data), size = train_size)

train_set = data[train_index,]
test_set = data[-train_index,]

X <- matrix(c(1),nrow = nrow(train_set))
X <- cbind(X, train_set[,"lstat"])

Y <- matrix(train_set[,"medv"])

BETA <- matrix(runif(2, 0, 1), nrow = 1)

RESULT  = grdient_descent_reg(X, Y, BETA, 0.2, 100, 0.3)



grdient_descent_reg <- function(X, Y, BETA, alpha, iter, lambda) {
  
  result = data.frame()
  
  no_of_samples = nrow(X)
  no_of_features = ncol(X)-1
  
  alpha = 0.1
  lambda = 0.2
  
  for (i in 1:iter) {
    
    E <- ((BETA%*%t(X)) - t(Y))
    
    cost = 1/(2*no_of_samples)*(E%*%t(E))
    
    BETA[,1] = BETA[,1] - (alpha/no_of_samples)*(E%*%X[,1])

    BETA[,-1] = BETA[,-1]*(1-(alpha*lambda)/no_of_samples) - (alpha/no_of_samples)*(E%*%X[,-1])
    
    result = rbind(result, c(cost, BETA))
  }
  
  name <- c("COST", c(1:ncol(BETA)))
  names(result) = name
  
  return(result)
  
}



standerdize = function(X) {
  
  for (i in 1:ncol(X)) {
    X[,i] = X[,i] - mean(X[,i])/sd(X[,i])
  }
  
  return(X)
}


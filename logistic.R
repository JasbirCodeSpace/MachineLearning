iris_new = iris[1:100,]

partition = function(X){
  N = as.double(readline(prompt = "Enter the percentage for training sample : "))
  print(N)
  train_size = floor(N*nrow(X))
  set.seed(1223)
  
  index = sample(seq_len(nrow(X)),size = train_size)
  
  return(index)
}

index = partition(iris_new)
train_sample = iris_new[index,]
test_sample = iris_new[-index,]
logic.model = glm(Species~.,data=train_sample,family = binomial(link = "logit"))
logic.model

summary(logic.model)

predicted = predict(logic.model,test_sample,type = "response")
predicted

predicted <- ifelse(predicted > 0.5, "versicolor", "setosa")
predicted

result = confusionMatrix(table(as.vector(predicted),as.vector(test_sample$Species)))
result$overall["Accuracy"]

result = table(as.vector(predicted),as.vector(test_sample$Species))
sum(diag(result))/sum(result)

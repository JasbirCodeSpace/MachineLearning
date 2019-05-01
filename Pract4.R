setwd('C:\\Users\\JASBIR-PC\\Desktop\\1223\\1223\\1223\\Programs')
Iris_Data = read.csv("iris-data.csv",header = FALSE)
Iris_Data = Iris_Data[1:100,]
names(Iris_Data) = c("sepal_length","sepal_width","petal_length","petal_width","species")


train_size = floor(0.50*nrow(Iris_Data))
train_size

set.seed(1223)
train_index = sample(1:nrow(Iris_Data),size = train_size) 

train_sample = Iris_Data[train_index,]
test_sample = Iris_Data[-train_index,]
train_label = train_sample[,5]

distance <- function(train,test){
  dist =0;
  for(i in 2:ncol(train)-1){
    dist = dist+(train[,i]-test[,i])^{2}
  }
  return(sqrt(dist))
}

KNN<-function(test,N){
dist_array = data.frame()
for(i in 1:nrow(train_sample)){
dist_array[i,1] = distance(train_sample[i,],test[1,])
dist_array[i,2] = train_index[i]

}
dist_array = dist_array[order(dist_array$V1),]
print(dist_array)
labels = data.frame()
for(i in 1:N){
  labels[i,1] = Iris_Data[dist_array[i,2],5]
  
}
table_data = table(labels)
new_label = names(table_data)[which(table_data == max(table_data))]
return(new_label)
}

test_data = test_sample[37,]
new_label = KNN(test_data,25)
test_data[,5] = new_label
#4(b)

logistic_model <- glm (species ~ ., data = train_sample, family = binomial)
summary(logistic_model)

predicted_model <- predict(logistic_model,newdata = test_sample,type = "response")

table(train_sample$species, predicted_model > 0.5)

predicted_model <- ifelse(predicted_model > 0.5,"Setosa","Versicolor")

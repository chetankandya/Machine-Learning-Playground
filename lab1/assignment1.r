library(kknn)
missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

my_crossentropy=function(ans, proba){
  return(-sum(ans*log(proba+1e-15)))
}

# Read the data
setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Final Labs/lab1")
data = read.csv("optdigits.csv", header=FALSE)
data$V65 <- as.factor(data$V65)

# Split the data
n = dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]


# Create a model, fit and display the results
Model = kknn(V65~., train, test, k=30, kernel = "rectangular")
fit = fitted(Model)
table(test$V65, fit)
missclass(test$V65, fit)

Model = kknn(V65~., train, train, k=30, kernel = "rectangular")
fit = fitted(Model)
table(train$V65, fit)
missclass(train$V65, fit)

# Finding the 2 best cases of digit 8
probabilities = predict(Model, type="prob")
indexes_of_eights = which(train$V65==8)
predictions_of_eights = probabilities[indexes_of_eights, "8"]
comb <- cbind(indexes_of_eights, predictions_of_eights)
sorted_predictions_of_eights = comb[order(comb[,2]),]

values <- tail(sorted_predictions_of_eights[,1], n=2)
values <- append(values, head(sorted_predictions_of_eights[,1], n=3))

print(values)
for (value in values)
{
  print(value)
  print(probabilities[value,"8"])
  d <- matrix(unlist(train[value,1:64]), nrow = 8, ncol=8, byrow = TRUE)
  heatmap(d, Colv=NA, Rowv=NA)
  #line <- readline()
}

training_misclassification_errors <- rep(NA, 0)
validation_misclassification_errors <- rep(NA, 0)
for (my_k in 1:30)
{
  Model = kknn(V65~., train, valid, k=my_k, kernel = "rectangular")
  fit = fitted(Model)
  validation_misclassification_errors <- c(validation_misclassification_errors, missclass(valid$V65, fit))
  Model = kknn(V65~., train, train, k=my_k, kernel = "rectangular")
  fit = fitted(Model)
  training_misclassification_errors <- c(training_misclassification_errors, missclass(train$V65, fit))
}

plot(validation_misclassification_errors, col="blue", xlim=c(0,30), ylim=c(0,0.06), xlab="K-value", ylab="Misclassification error")
points(training_misclassification_errors, col="red")

validation_misclassification_errors[6]
Model = kknn(V65~., train, test, k=1, kernel = "rectangular")
fit = fitted(Model)
missclass(test$V65, fit)
Model = kknn(V65~., train, test, k=2, kernel = "rectangular")
fit = fitted(Model)
missclass(test$V65, fit)
Model = kknn(V65~., train, test, k=3, kernel = "rectangular")
fit = fitted(Model)
missclass(test$V65, fit)
Model = kknn(V65~., train, test, k=4, kernel = "rectangular")
fit = fitted(Model)
missclass(test$V65, fit)

result <- data.frame(as.integer(valid$V65==0), as.integer(valid$V65==1), 
                     as.integer(valid$V65==2), as.integer(valid$V65==3), 
                     as.integer(valid$V65==4), as.integer(valid$V65==5), 
                     as.integer(valid$V65==6), as.integer(valid$V65==7), 
                     as.integer(valid$V65==8), as.integer(valid$V65==9))

test_crossentropy_errors <- rep(NA, 0)
for (my_k in 1:30)
{
  Model = kknn(V65~., train, valid, k=my_k, kernel = "rectangular")
  probabilities = predict(Model, type="prob")
  test_crossentropy_errors <- c(test_crossentropy_errors, my_crossentropy(result, probabilities))
}

plot(test_crossentropy_errors, col="blue", xlab="K-value", ylab="Cross entropy error")
# The optimal K is 6

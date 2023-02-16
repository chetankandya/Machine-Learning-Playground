setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/TDDE01-master/Exam_2019-01-16")
data = read.csv("Influenza.csv", header=TRUE)

scaled_data = data.frame(scale(subset(data, select = -c(Mortality))))
Mortality = data$Mortality
scaled_data = cbind(scaled_data, Mortality)

n = dim(scaled_data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))

train = scaled_data[id,]
test = scaled_data[-id,]


#Special for lasso
# is that alpha=1
library(glmnet)
fit_lasso = glmnet(as.matrix(train[-9]),
                   train$Mortality,
                   alpha=1,
                   family="poisson")

plot(fit_lasso, xvar="lambda")


cv_fit = cv.glmnet(as.matrix(train[-9]),
                    train$Mortality,
                    alpha=1,
                    family="poisson",
                    nfolds=10)
# By looking at this plot
# if the dots are within the grey bars,
# it means that it is NOT statistically
# significantly better and vice versa
plot(cv_fit)
# optimal lambda
log(cv_fit$lambda.min)
# 9 features used
coef(cv_fit, s="lambda.min")



newfit = glmnet(as.matrix(train[-9]),
                train$Mortality,
                alpha=1,
                family="poisson",
                lambda=cv_fit$lambda.min)

yhat = predict(newfit, as.matrix(test[-9]))
plot(test$Mortality, yhat, col="blue")

mse = mean((test$Mortality-yhat)^2)
mse

#TREE

data = read.csv("Influenza.csv", header=TRUE, sep = ",", stringsAsFactors = TRUE)

n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))

train = data[id,]
test = data[-id,]



model.tree <- tree(Mortality ~ ., data=data)
model.tree.cv <- cv.tree(model.tree, FUN=prune.tree, )
plot(model.tree.cv)

model.tree.pruned <- prune.tree(model.tree, best = 10)
#Plots the pruned tree with 3 leaves
plot(model.tree.pruned) 
text(model.tree.pruned)


missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#fitting the decision tree for training data
fit_train1 = tree(Mortality ~ . , data=train)
plot(fit_train1)
prediction_train_1 = predict(fit_train1, train, type="vector")
mc_train1 = missclass(prediction_train_1, train$Mortality)
mc_train1

summary(fit_train1)

#Studying the tree upto 50 leaves
training_score = rep(0, 9)
validation_score = rep(0, 9)

for (i in 2:9){
  pruned_tree = prune.tree(fit_train1, best=i)
  pred = predict(pruned_tree, newdata = test, type="tree")
  training_score[i] = deviance(pruned_tree)
  validation_score[i] = deviance(pred)
}

plot(2:9, training_score[2:9], type="b", col="red", ylim=c(8000, 12000))+
  points(2:9, validation_score[2:9], type="b", col="blue")

min_deviance=min(validation_score[2:9])
optimal_leaves=which(validation_score[1:9] == min_deviance)
optimal_leaves


prediction_test = predict(fit_train1, test)
mse = missclass(prediction_test, test$Mortality)
mse


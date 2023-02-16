library(tree)
#install.packages("MLmatrics")
missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Final Labs/lab2")
data = read.csv("bank-full.csv", header=TRUE, sep = ";", stringsAsFactors = TRUE)
data = subset(data, select=c(-duration))

n = dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.4))
train=data[id,]

id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.3))
validation=data[id2,]

id3=setdiff(id1,id2)
test=data[id3,]

#fitting the decision tree for training data
fit_train1 = tree(y ~ . , data=train)
plot(fit_train1)
summary(fit_train1)
prediction_train_1 = predict(fit_train1, train, type="class")
mc_train1 = missclass(prediction_train_1, train$y)
mc_train1

fit_train2 = tree(y ~ . , data=train, control=tree.control(nrow(train), minsize=7000))
plot(fit_train2)
summary(fit_train2)
prediction_train_2 = predict(fit_train2, train, type="class")
mc_train2 = missclass(prediction_train_2, train$y)
mc_train2

fit_train3 = tree( y ~ . , data=train, control=tree.control(nrow(train), mindev=0.0005))
plot(fit_train3)
summary(fit_train3)
prediction_train_3 = predict(fit_train3, train, type="class")
mc_train3 = missclass(prediction_train_3, train$y)
mc_train3

#fitting the decision tree for Validation data
prediction_valid_1 = predict(fit_train1, validation, type="class")
table(prediction_valid_1, validation$y)
mc_valid1 = missclass(prediction_valid_1, validation$y)
mc_valid1

prediction_valid_2 = predict(fit_train2, validation, type="class")
table(prediction_valid_2, validation$y)
mc_valid2 = missclass(prediction_valid_2, validation$y)
mc_valid2

prediction_valid_3 = predict(fit_train3, validation, type="class")
table(prediction_valid_3, validation$y)
mc_valid3 = missclass(prediction_valid_3, validation$y)
mc_valid3

#Studying the tree upto 50 leaves
training_score = rep(0, 50)
validation_score = rep(0, 50)

for (i in 2:50){
  pruned_tree = prune.tree(fit_train3, best=i)
  pred = predict(pruned_tree, newdata = validation, type="tree")
  training_score[i] = deviance(pruned_tree)
  validation_score[i] = deviance(pred)
}
plot(2:50, training_score[2:50], type="b", col="red", ylim=c(8000, 12000))+
  points(2:50, validation_score[2:50], type="b", col="blue")

min_deviance=min(validation_score[2:50])
optimal_leaves=which(validation_score[1:50] == min_deviance)
optimal_leaves

#Optimal number of leaves = 22
finalTree=prune.tree(fit_train3, best=22)
Yfit=predict(finalTree, newdata=validation, type="class")
table(validation$y, Yfit)

summary(finalTree)
plot(finalTree)
text(finalTree, pretty=0)
#Variables actually used in tree construction: "poutcome", "month"
#"contact", "pdays" , "age", "day",  "balance", "housing", "job" 
#depth of tree is 9, as the number of variables = 9

#part4
#fitting the tree on optimal model
predict_test = predict(finalTree, newdata = test, type="class")
summary(predict_test)
#Calculating missclassification error
mc_test = missclass(predict_test, test$y)
mc_test

t = table(predict_test, test$y)
#Calculating accuracy
acc = sum(diag(t))/sum(t)
acc

#Calculating F1 score
recall = t[2,2]/sum(t[2,])
precision = t[2,2]/sum(t[,2])
F1 = (2*precision*recall)/(precision + recall)
F1


#Part 5
#Decision tree classification

Yfit=predict(finalTree,newdata=test)
Lfit=ifelse(Yfit[,1]>(Yfit[,2]*5),"no","yes")
missclass(test$y, Lfit)
tb = table(Actual=test$y, Predicted=Lfit)
acc = sum(diag(tb))/sum(tb)
acc
recall = tb[2,2]/sum(tb[2,])
precision = tb[2,2]/sum(tb[,2])
F1 = (2*precision*recall)/(precision + recall)
#F2 = F1_Score(test$y, Lfit)
F1


# The number of misclassified "yes"'s has gone down due to a higher penalization for misclassifying yes's

#part6

#Creating logistic regression model
lgrmodel = glm(y ~ ., data=train, family=binomial(link="logit"))
predlgr = predict(lgrmodel, newdata=test, type="response")

#Using the final tree to predict
predict_test_optimal = predict(finalTree, newdata = test, type="vector")

pi = seq(0.05, 0.95, 0.05)
index=0;
log_preds=data.frame(matrix(0, ncol = 19, nrow = 13564));
tree_preds=data.frame(matrix(0, ncol = 19, nrow = 13564));

tprs_log = c()
fprs_log = c()
tprs_tree = c()
fprs_tree = c()

for (i in seq(0.05,0.95,by=0.05)) 
{
  index=index+1
  log_preds[,index] = ifelse(predlgr > i, "yes", "no")
  tree_preds[,index] = ifelse(predict_test_optimal[,2] > i, "yes", "no")
}

for(i in 1:19) 
{
  cm=table(test$y,log_preds[,i])
  tprs_log[i]=cm[2,2]/sum(cm[2,])
  fprs_log[i]=cm[1,2]/sum(cm[1,])
} 

for(i in 1:15) 
{
  cm=table(test$y,tree_preds[,i])
  tprs_tree[i]=cm[2,2]/sum(cm[2,])
  fprs_tree[i]=cm[1,2]/sum(cm[1,])
} 

plot(fprs_log, tprs_log, main="ROC curve for Logistic  Model", type="b", col="red", xlim=c(0,0.7), ylim=c(0,1), xlab="FPR", ylab="TPR")
plot(fprs_tree, tprs_tree, main="ROC curve for Tree Model", type="b", col="blue", xlim=c(0,0.7), ylim=c(0,1), xlab="FPR", ylab="TPR")

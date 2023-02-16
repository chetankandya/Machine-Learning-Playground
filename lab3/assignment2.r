# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes
library(kernlab)
set.seed(1234567890)
data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ] # Train
va <- spam[3001:3800, ] # Validate
trva <- spam[1:3800, ] # Train validate?
te <- spam[3801:4601, ] # Test
by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <-
    ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}
filter0 <-
  ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*
         by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0
filter1 <-
  ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*
         by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1
filter2 <-
  ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va
  )*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2
filter3 <-
  ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va
  )*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3
# Questions
# 1. Which filter do we return to the user ? filter0, filter1, filter2 or 
#filter3? Why?
  # Filter 3, it contains all points.
  # 2. What is the estimate of the generalization error of the filter returned 
  #to the user? err0, err1, err2 or err3? Why?
  # err2, It has traied on both training ad validation error and is now tested, 
  #we can then assume that the model is atleast this good when also training on 
  #test data (filter3).
# 3. Implementation of SVM predictions.
kernal = rbfdot(sigma = 0.05)
sv<-alphaindex(filter3)[[1]] # Indexes of the support vectors
co<-coef(filter3)[[1]] # The coefficients
inte<- -b(filter3) # offset
k<-NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the 
  dataset.
  k2<-NULL
  for(j in 1:length(sv)){
    k2 <- c(k2, co[j] * kernal(as.vector(t(spam[sv[j],-58])), 
                               as.vector(t(spam[i,-58]))))
  }
  k<-c(k, sum(k2)+inte)
}
k
predict(filter3,spam[1:10,-58], type = "decision")




#Exam

library(kernlab)
Var <- runif(50, 0, 10)
tr <- data.frame(Var, Sin=sin(Var))

svm1 <- ksvm(Sin~.,data=tr,kernel="rbfdot",kpar=list(sigma=1),C=.1)
plot(tr[,1],predict(svm1,tr), col="blue", cex=3, ylim = c(-1.1,1.1))
points(tr, col = "red", cex=3)

svm2 <- ksvm(Sin~.,data=tr,kernel="rbfdot",kpar=list(sigma=1),C=1)
plot(tr[,1],predict(svm2,tr), col="blue", cex=3, ylim = c(-1.1,1.1))
points(tr, col = "red", cex=3)


#The C parameter tells the SVM optimization how much you want to avoid misclassifying 
#each training example. For large values of C, the optimization will choose a smaller-margin 
#hyperplane if that hyperplane does a better job of getting all the training points classified correctly. 
#Conversely, a very small value of C will cause the optimizer to look for a larger-margin separating 
#hyperplane, even if that hyperplane misclassifies more points. For very tiny values of C, you should 
#get misclassified examples, often even if your training data is linearly separable.

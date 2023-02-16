setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Final Labs/lab2")
data = read.csv("tecator.csv", header=TRUE)
data = as.data.frame(data)

n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))

data = subset(data,
              select = -c(Protein, Moisture, ï..Sample))


train = data[id,]
test = data[-id,]

train_x = subset(train, select = -c(Fat))
train_y = train$Fat

test_x = subset(test, select = -c(Fat))
test_y = test$Fat

fit1 = lm( formula = train_y ~ ., data=train_x)
summary(fit1)

train_mse = mean((fit1$residuals)^2)

train_pred =  predict(fit1, train_x)
train_mse_2 = mean((train_y - train_pred)^2)

test_pred = predict(fit1, test_x)
# Getting EXTREMELY high value? why?
test_mse = mean((test_y - test_pred)^2)

# very low mse, overfit?
train_mse
# test mse is 722, overfit on training data?
test_mse

# Question 2 & 3

# Special for lasso
# is that alpha=1
library(glmnet)
fit_lasso = glmnet(as.matrix(train_x),
                   train_y,
                   alpha=1,
                   family="gaussian")

# when lambda is ~ -0.3 we can see 3 coefficients
# that are not 0
plot(fit_lasso, xvar="lambda")

# Ridge
# special for ridge is that alpha=0
fit_ridge = glmnet(as.matrix(train_x),
                   train_y,
                   alpha=0,
                   family="gaussian")

plot(fit_ridge, xvar="lambda")

# Conclusions:
# Both ridge and lasso regression have the
# same charectaristics in that they both
# introduce a penalty parameter lambda.
# But we can see that in LASSO regression
# we penalize the fetures much harder than
# in the ridge regression. For almost
# every lambda, we can see that in LASSO, almost always
# some features are completely removd (set to zero),
# meanwhile in ridge regression we dont completely remove
# the features but just reguralize them.

# Question 5

cv_fit = cv.glmnet(as.matrix(train_x),
                   train_y,
                   alpha=1,
                   family="gaussian",
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

newfit = glmnet(as.matrix(train_x),
                train_y,
                alpha=1,
                family="gaussian",
                lambda=cv_fit$lambda.min)

yhat = predict(newfit, as.matrix(test_x), nfolds=10)
plot(test_y, yhat, col="blue")
lines(x=c(0:100), y=(0:100))




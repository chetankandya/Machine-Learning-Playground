setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Final Labs/lab1")
data = read.csv("parkinsons.csv", header=TRUE)

data = as.data.frame(scale(data))

scaled_data = subset(data, 
                       select= -c(subject., 
                                  age, 
                                  sex, 
                                  test_time,
                                  total_UPDRS))

n = dim(scaled_data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.6))
train=scaled_data[id,]
test=scaled_data[-id,]

model = lm(motor_UPDRS ~ . , data=train)

train_pred = predict(model, train)
test_pred = predict(model, test)

train_mse = mean((train$motor_UPDRS - train_pred)^2)
test_mse = mean((test$motor_UPDRS - test_pred)^2)

train_mse
test_mse

summary(model)

# Loglikelihood:
# optimizes our unknown sigma and theta
# Parameters:
#   Vector theta (size = # features)
#   sigma (a constant and we want to find the optimal value, should be positive initially)

log_likelihood = function(theta, sigma_2, x, y){
  x = as.matrix(x)
  y = as.matrix(y)
  
  n = dim(x)[1]
  
  loglik = -(n/2) * log(2*pi) - (n/2) * log(sigma_2^2) - (1/(2*sigma_2^2)) * sum((y - x %*% theta)^2)
  return (-loglik)
}

ridge = function(par, lambda, x, y){
  theta = par[1:16]
  sigma = par[17]
  return (lambda * norm(theta, type="2")^2 + log_likelihood(theta=theta,
                                                            sigma_2=sigma, 
                                                            x=x, 
                                                            y=y))
}

ridgeOpt = function(lambda, theta, sigma_2, observed_x, observed_y){
  o = optim(method="BFGS",
            fn=ridge,
            par=c(theta, sigma_2),
            x=observed_x,
            y=observed_y,
            lambda=lambda
            )
  
  return (o)
}

df = function(lambda, obs_x){
  idntymatrix = matrix(0, nrow=16,ncol=16)
  diag(idntymatrix) = lambda

  x = as.matrix(obs_x)
  
  # solve() gets the inverse
  res = (x %*% solve((t(x) %*% x) + idntymatrix)) %*% t(x)
  
  return (sum(diag(res)))
}

myPrediction = function (theta, train_data ){
  return ( as.matrix(train_data) %*% theta )
}

mytheta = rep(0, 16) # Creates a 1d vector with zeros
mysigma = 0.1 # should be positive initially

subs = subset(train, select= -c(motor_UPDRS))
test_subs = subset(test, select= -c(motor_UPDRS))

ridgeopt1 = ridgeOpt(lambda=1,
                     theta=mytheta,
                     sigma_2=mysigma,
                     observed_x=subs,
                     observed_y=train$motor_UPDRS)

ridgeopt2 = ridgeOpt(lambda=100,
                     theta=mytheta,
                     sigma_2=mysigma,
                     observed_x=subs,
                     observed_y=train$motor_UPDRS)

ridgeopt3 = ridgeOpt(lambda=1000,
                     theta=mytheta,
                     sigma_2=mysigma,
                     observed_x=subs,
                     observed_y=train$motor_UPDRS)

trainpred1 = myPrediction(ridgeopt1$par[1:16], subs)
testpred1 = myPrediction(ridgeopt1$par[1:16], test_subs)

trainpred2 = myPrediction(ridgeopt2$par[1:16], subs)
testpred2 = myPrediction(ridgeopt2$par[1:16], test_subs)

trainpred3 = myPrediction(ridgeopt3$par[1:16], subs)
testpred3 = myPrediction(ridgeopt3$par[1:16], test_subs)

mse1 = mean((train$motor_UPDRS - trainpred1)^2)
mse11 = mean((test$motor_UPDRS - testpred1)^2)

mse2 = mean((train$motor_UPDRS - trainpred2)^2)
mse22 = mean((test$motor_UPDRS - testpred2)^2)

mse3 = mean((train$motor_UPDRS - trainpred3)^2)
mse33 = mean((test$motor_UPDRS - testpred3)^2)

df1 = df(lambda=1,
         obs_x=as.vector(subs))
df2 = df(lambda=100,
         obs_x=as.vector(subs))
df3 = df(lambda=1000,
         obs_x=as.vector(subs))

# All train mse
mse1
mse2
mse3

# All test mse
mse11
mse22
mse33

train_mse
test_mse

# Degrees of freedom
df1
df2
df3




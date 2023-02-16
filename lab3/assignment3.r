library(neuralnet)

set.seed(1234567890)
Var = runif(500, 0, 10)
mydata = data.frame(Var, Sin=sin(Var))
train = mydata[1:25,] # Training
test = mydata[26:500,] # Test

#     /***********************************\
# /**************** Part 1 *********************\
#   \**************************************/

# Random initialization of the weights in the interval [-1, 1]
winit = runif(10, -1, 1)
nn = neuralnet( formula = Sin ~ Var ,data=train, hidden=10, startweights = winit)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2)
points(test, col = "blue", cex=1)

points(test$Var,predict(nn,test), col="red", cex=1)

nn$weights

# Comments on part 1:
# We get good results as expected. The predictions seem to predict
# sin(x) from x in a precise way almost perfectly

#     /***********************************\
# /**************** Part 2 *********************\
#   \**************************************/
linear <- function(x) x

relU <- function(x) max(0, x)

softplus <- function(x) log(1 + exp(x))

# Activation function
sigmoid = function(x) (1 / (1 + exp(-x)))

nn2 = neuralnet(formula = Sin ~ Var ,
                data=train,
                hidden=10, 
                startweights = winit,
                act.fct=linear)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2, main="linear")
points(test, col = "blue", cex=1)

points(test[,1],predict(nn2,test), col="red", cex=1)

nn3 = neuralnet(formula = Sin ~ Var ,
                data=train,
                hidden=10,
                startweights = winit,
                act.fct=relU)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2, main="RelU")
points(test, col = "blue", cex=1)

points(test$Var,predict(nn3,test), col="red", cex=1)

nn4 = neuralnet(formula = Sin ~ Var ,
                data=train,
                hidden=10,
                startweights = winit,
                act.fct=softplus)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train, cex=2, main="Softplus")
points(test, col = "blue", cex=1)

points(test$Var,predict(nn4,test), col="red", cex=1)

#     /***********************************\
# /**************** Part 3 *********************\
#   \**************************************/

set.seed(1234567890)
# Create 500 points in the interval [0, 50]
Var = runif(500, 0, 50)
# Apply sin function to each point
mydata = data.frame(Var, Sin=sin(Var))

test = mydata[1:500,]

# Plot of the training data (black), test data (blue), and predictions (red)
plot(test, cex=2, main="interval [0, 50]", xlim=c(0,50), ylim=c(-10, 1))
# points(test, col = "blue", cex=1)
pred = predict(nn, test)
points(test$Var,pred, col="red", cex=1)

#     /***********************************\
# /**************** Part 4 *********************\
#   \**************************************/

pred
nn$weights

# Calculations for part 4


# from input layer to the 10 neurons
# > nn$weights
# [[1]]
# [[1]][[1]]
# [,1]       [,2]       [,3]       [,4]      [,5]       [,6]      [,7]      [,8]       [,9]      [,10]
# [1,]  3.3046893 -0.3271777  0.4043669 -0.7669631 11.955191 -0.2057986 -6.472200  7.904868 -0.5708964 0.04342472 <- Bias
# [2,] -0.8033506 -0.8324709 -0.1499125 -0.8256431 -1.802597  0.7993943  3.082136 -2.320714  0.1543628 0.76288667 <- Weights

# From the 10 neurons to the output layer with 1 neuron
# [[1]][[2]]
# [,1]
# [1,]  0.7993476 <- Bias, only one bias since we only have one neuron in the output layer
# [2,] -4.9649102 w_0
# [3,] -4.8295057 w_1
# [4,] 22.1703831 w_2
# [5,] -5.5590648 w_3
# [6,] -4.3747945 w_4
# [7,]  0.3489759 w_5
# [8,] -0.7224382 w_6
# [9,]  1.8612110 w_7
# [10,] -9.4390597  w_8
# [11,]  0.4400295  w_9

# Get weights and biases for 1st hidden layer
b = nn$weights[[1]][[1]][1,]
w = nn$weights[[1]][[1]][2,]
b
w
neurons = c(rep(0, 10))
neurons

output = c(rep(0, 25))
output
w2 = nn$weights[[1]][[2]][2:11, ]
w2
b2 = nn$weights[[1]][[2]][1]
b2
# for (i in 1:10){
#   neurons_1st_hidden[i] = sum(train$Var) * w[i] + b[i]
# }
# 
# neurons_1st_hidden
# after_activation = sigmoid(neurons_1st_hidden)
# after_activation

for (i in 1:25){
  for (j in 1:10){
    neurons[j] = sigmoid(train$Var[i] * w[j] + b[j])
  }
  for (j in 1:10){
    output[i] = output[i] + neurons[j] * w2[j]
  }
  output[i] = output[i] + b2
}
output


# Code below tests for a 
# custom point for better
# understanding, in this
# case x
b = nn$weights[[1]][[1]][1,]
w = nn$weights[[1]][[1]][2,]
b
w

x = 2000
neurons = c(rep(0, 10))
neurons
output = 0
w2 = nn$weights[[1]][[2]][2:11, ]
w2
b2 = nn$weights[[1]][[2]][1]
b2
for (j in 1:10){
  neurons[j] = sigmoid(x * w[j] + b[j])
  print(sigmoid(x * w[j] + b[j]))
} 
for (j in 1:10){
  output = output + neurons[j] * w2[j]
}
output = output + b2
output

#     /***********************************\
# /**************** Part 5 *********************\
#   \**************************************/

set.seed(1234567890)
Var = runif(500, 0, 10)
train = data.frame(Sin=sin(Var), Var)


winit = runif(10, -1, 1)
nn = neuralnet(formula= Var ~ Sin, data=train, hidden=10, startweights=winit,
               threshold=0.1)

plot(train, cex=2, main="Predict x from sin(x)") #ylim=c(0, 10)

pred = predict(nn, train)
points(train$Sin, pred, col="red", cex=1)


#part 6
set.seed(1234567890)
Var <- runif(50, 0, 10)
trva <- data.frame(Var, Sin=sin(Var))
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation

va

# Random initializaiton of the weights in the interval [-1, 1]
set.seed(12345)
winit <- runif(10, -1, 1)

mse_values = c()

for(i in 1:10) {
  nn <- neuralnet(Sin ~ Var, tr, threshold=i/1000, hidden=10, startweight=winit)
  predictions = predict(nn, va)
  mse = mean((predictions-va$Sin)^2)
  mse_values = c(mse_values, mse)
}

plot(1:10, mse_values)
best_threshold = which.min(mse_values)/1000

# reset weights
set.seed(12345)
winit <- runif(10, -1, 1)

# final nn
final_nn = neuralnet(Sin ~ Var, tr, threshold=best_threshold, hidden=10, startweight=winit)
predictions = predict(final_nn, va)
mse = mean((predictions-va$Sin)^2)

# Plot validation data (blue) and predictions on validation data (red)
plot(va, col = "blue", cex=1)
points(va[,1], predict(final_nn, va), col="red", cex=1)




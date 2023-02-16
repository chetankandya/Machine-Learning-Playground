library(ggplot2)
setwd("C:/Users/hp/Desktop/LiU Sem I/Machine Learning/Final Labs/lab2")
data = read.csv("communities.csv", header=TRUE)

# Part 1
scaled_data = subset(data, select = -c(ViolentCrimesPerPop))
scaled_data = as.data.frame(scale(scaled_data))

my_eigen = eigen(cov(scaled_data))
proportion_of_variations = my_eigen$values/sum(my_eigen$values)*100
sprintf("%2.3f",proportion_of_variations)
sum((proportion_of_variations)[1:35]) # The first 35 PC's will obtain a 95.2446% variance of the original datas variance
proportion_of_variations[1] # 25.02494%
proportion_of_variations[2] # 16.93082%


# Part 2
pca <- princomp(scaled_data)
summary(pca)
# Also here 35 principle components are needed inorder to obtain at least 95% of variance in the data
# and 25.01699% for PC1 and 16.93597% for PC2

loadings = (as.data.frame(pca$loadings[,1])) # Get PC1's loading scores (all together called eigenvector)
loadings["feature"] <- as.factor(rownames(loadings))
rownames(loadings) <- NULL # Reset row names to indexes
colnames(loadings) <- c("value","feature")

ggplot(loadings, aes(x=feature, y=value)) + geom_point()
sorted_indexes = order((loadings$value))
loadings[tail(sorted_indexes, 5),]
loadings[head(sorted_indexes, 5),]


loadings = (as.data.frame(pca$loadings[,2])) # Get PC1's loading scores (all together called eigenvector)
loadings["feature"] <- as.factor(rownames(loadings))
rownames(loadings) <- NULL # Reset row names to indexes
colnames(loadings) <- c("value","feature")

sorted_indexes = order((loadings$value))
loadings[tail(sorted_indexes, 5),]
loadings[head(sorted_indexes, 5),]


df <- cbind(data.frame(pca$scores[,1:2]), data$ViolentCrimesPerPop)
colnames(df) <- c("PC1","PC2", "ViolentCrimesPerPop")

ggplot(df, aes(x=PC1, y=PC2, color=ViolentCrimesPerPop)) + geom_point() +
  scale_color_gradient2(low = "blue", mid = "white",high = "red", midpoint = 0.5)#mean(df$ViolentCrimesPerPop))


# Part 3
scaled_data = as.data.frame(scale(data)) # Everything scaled, including ViolentCrimesPerPop
n = dim(scaled_data)[1]
set.seed(12345)
id = sample(1:n, floor(n*0.5))
train=scaled_data[id,]
test=scaled_data[-id,]

model = lm(ViolentCrimesPerPop~., data = train) # Create the linear regression model
summary(model)

preds = predict(model, subset(train, select = -c(ViolentCrimesPerPop)))
mse_df <- data.frame(pred = preds, actual = train$ViolentCrimesPerPop)
mean((mse_df$actual - mse_df$pred)^2) # MSE
# Seems like a "reasonable" model not to good but not awful

preds = predict(model, subset(test, select = -c(ViolentCrimesPerPop)))
mse_df <- data.frame(pred = preds, actual = test$ViolentCrimesPerPop)
mean((mse_df$actual - mse_df$pred)^2) # MSE

# Part 4
train_error_list <- rep(NA, 0)
test_error_list <- rep(NA, 0)
test_x=subset(test, select= -c(ViolentCrimesPerPop))
test_y=test$ViolentCrimesPerPop

cost_func = function(theta, x, y){
  curr_train_error <- (mean((y-(theta%*%t(x)))^2))
  curr_test_error <- (mean((test_y-(theta%*%t(test_x)))^2))
  .GlobalEnv$test_error_list <- c(.GlobalEnv$test_error_list, curr_test_error)
  .GlobalEnv$train_error_list <- c(.GlobalEnv$train_error_list, curr_train_error)
  return(curr_train_error)
}

mytheta = rep(0, 100) # Creates a 1d vector with zeros
set.seed(12345)
new = optim(method="BFGS",
            fn=cost_func,
            par=c(mytheta),
            x=subset(train, select= -c(ViolentCrimesPerPop)),
            y=train$ViolentCrimesPerPop
)

train_error_list_new <- train_error_list
test_error_list_new <- test_error_list
plot(train_error_list_new,
     col="blue",
     xlab="Iteration",
     ylab="Loss",
     type = "l",
     lty = 1,
     xlim=c(500,4000),
     ylim=c(min(train_error_list_new), 3))
points(test_error_list_new, col="red", type = "l", lty = 1)
legend("topleft",
       c("Test loss","Training loss"),
       fill=c("red","blue"))

head(order(test_error_list_new), n=1)
test_error_list_new[head(order(test_error_list_new), n=1)]
train_error_list_new[head(order(test_error_list_new), n=1)]


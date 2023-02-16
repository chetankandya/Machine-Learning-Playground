#Part 1
data = read.csv("C:/Users/hp/OneDrive/Desktop/LiU Sem I/Machine Learning/Lab/Lab 1/lab1/pima-indians-diabetes.csv", header=FALSE)

data = subset(data, select= -c(V1,V3,V4,V5,V6,V7))

Age=data$V8
Plasma=data$V2
Diabetes=data$V9

cor(data)

plot(Age, Plasma, col=ifelse( Diabetes == 1 ,"red", "blue") )

#Diabetes cannot be easily classified on the basis of age and plasma, as there is no clear dividing line.

#Training a Logistic regression model with y=Diabetes(target) and x1= Plasma conc, x2= Age as features.
Model <- glm(Diabetes ~ Age+Plasma, data=data, family = binomial(link="logit"))
#Predicting the values
pred = predict(Model, type = "response")
#Setting the threshold as 0.5 
above = as.numeric(pred>0.5)

summary(Model)

missclass=function(X,X1){
  n=length(X)
  return(1-sum(diag(table(X,X1)))/n)
}

#Generating a confusion matrix
table(Actual_Value=data$V9, Predicted_Value=above)

#Calculating the miss classification error
missclass(Diabetes, above)
summary.glm(Model)

# y = 0.0356(x1) + 0.0247(x2) is the probabilistic equation generated and used by the model
plot(Age, Plasma, col=ifelse( above == 1 ,"red", "blue"))
#The quality of classification looks good, as it can be verified from the graph.

#Part 3
#Equation for decision boundary for Class Age and Class Plasma concentration

b = coef(Model)
slope = -b[2]/b[3]
int = -b[1]/b[3]

#slope <- as.numeric(coef(Model)[2]/(-coef(Model)[3]))
#int <-as.numeric(coef(Model)[1]/(-coef(Model)[3]))
#install.packages("lattice")

plot(Age, Plasma, col=ifelse( above == 1 ,"red", "blue"))+
  abline(int , slope)

#Part 4
#Changing the threshold value to 0.2 and repeating the above steps.
above = as.integer(pred>0.2)
table(Actual_Value=data$V9, Predicted_Value=above)
missclass(Diabetes, above)
summary.glm(Model)
plot(Age, Plasma, col=ifelse( above == 1 ,"red", "blue"))


#Changing the threshold value to 0.8 and repeating the above steps.
above = as.integer(pred>0.8)
table(Actual_Value=data$V9, Predicted_Value=above)
missclass(Diabetes, above)
summary.glm(Model)
plot(Age, Plasma, col=ifelse( above == 1 ,"red", "blue"))


#Part 5
x1 = data$V2
x2 = data$V8
#z1 = x1^4
#z2 = x1^3 * x2
#z3 = x1^2 * x2^2
#z4 = x1 * x2^3
#z5 = x2^4

Model <- glm(data$V9 ~ x1 + x2 + x1^4 + x1^3*x2 + x1^2*x2^2 + x1*x2^3 + x2^4, data=data, family = binomial)
pred = predict(Model, data)
above = as.integer(pred>0.5)
table(Actual_Value=data$V9, Predicted_Value=above)
missclass(Diabetes, above)
plot(Age, Plasma, col=ifelse( above == 1 ,"red", "blue"))



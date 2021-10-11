load("EU.RData") 

# creating fitted linear model
model = lm(formula = EU$CamCom2011 ~ EU$Population2010)
model$coefficients
model$residuals

summaryMyModel = summary(model) #gives infos about residuals, coeffs, fitted values..
summaryMyModel$sigma


# Using mass package for boston housing dataset
library(MASS)
dim(Boston)
summary(Boston)

train = 1:400 # first 400 observations for training
test = -train


#1 st way
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

#2nd way
variables2 = data.frame(Boston$lstat,Boston$medv)
traindata = variables2[train,]
testdata = variables2[test,]

dim(training_data) # Boston df reduced to 400 rows of 2 cols

plot(training_data$lstat, training_data$medv, title("medv en fct de lstat"))
#its not linear so let's transform


#by looking at the log(data) plotted, we can fit a linear model
plot(log(training_data$lstat), training_data$medv)


modelLin = lm(medv ~ log(lstat), data = training_data)
summary(modelLin)

names(modelLin)
confint(modelLin, level = 0.95)


#plot + aesthetics
plot(log(training_data$lstat),training_data$medv,
xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
ylab = "Median House Value",
col = "red",
pch = 20)

abline(modelLin, col = "black", lwd =3)


# Predict what is the median values of houses with lstat= 5%, 10%, and 15%
predict(modelLin, data.frame(lstat = c(5,10,15)), interval = "prediction")

# Save the testing median values for houses (testing y) in y
y = testing_data$medv

# Compute the predicted value for this y (y hat)
y_hat = predict(modelLin, data.frame(lstat = testing_data$lstat))

# Now we have both y and y_hat for our testing data. 
# let's find the mean square error
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE

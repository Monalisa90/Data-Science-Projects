rm(list = ls())
library(ISLR)
library(class)
library(caret)
library(leaps)
library(corrplot)
library(car)
require(e1071)
library(bootstrap)
library(rpart)
library(gbm)
#install.packages("gbm")
white_wine = read.csv2("C:\\Users\\Monalisa Mishra\\Desktop\\Data Mining Project\\Wine Quality Project\\winequality-white.csv", header = TRUE)
attach(white_wine)
summary(white_wine)
white_wine = data.frame(lapply(white_wine, function(x) as.numeric(as.character(x))))
############################################################################################
# Histogram for quality
############################################################################################

hist(white_wine$quality, main = "Histogram for Quality of White Wine", xlab = "Quality", ylab = "count", col = "lightblue");

############################################################################################
# Histogram for all predictors
############################################################################################

par(mfrow = c(3,4))
hist(white_wine$fixed.acidity, main = "Histogram for fixed.acidity", xlab = "fixed.acidity", ylab = "count", col = "lightblue");
hist(white_wine$volatile.acidity, main = "Histogram for volatile.acidity", xlab = "volatile.acidity", ylab = "count", col = "lightblue");
hist(white_wine$citric.acid, main = "Histogram for citric.acid", xlab = "citric.acid", ylab = "count", col = "lightblue");
hist(white_wine$residual.sugar, main = "Histogram for residual.sugar", xlab = "residual.sugar", ylab = "count", col = "lightblue");
hist(white_wine$chlorides, main = "Histogram for chlorides", xlab = "chlorides", ylab = "count", col = "lightblue");
hist(white_wine$free.sulfur.dioxide, main = "Histogram for free.sulfur.dioxide", xlab = "free.sulfur.dioxide", ylab = "count", col = "lightblue");
hist(white_wine$total.sulfur.dioxide, main = "Histogram for total.sulfur.dioxide", xlab = "total.sulfur.dioxide", ylab = "count", col = "lightblue");
hist(white_wine$density, main = "Histogram for density", xlab = "density", ylab = "count", col = "lightblue");
hist(white_wine$pH, main = "Histogram for pH", xlab = "pH", ylab = "count", col = "lightblue");
hist(white_wine$sulphates, main = "Histogram for sulphates", xlab = "sulphates", ylab = "count", col = "lightblue");
hist(white_wine$alcohol, main = "Histogram for alcohol", xlab = "alcohol", ylab = "count", col = "lightblue");

#############################################################################################
# Box plot the check outliers
############################################################################################
# Box plot before removing outliers
############################################################################################

par(mfrow = c(3,4))
#boxplot(fixed.acidity)
boxplot(white_wine$fixed.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$volatile.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$citric.acid, horizontal = FALSE, col="slategray2", pch=19)
mtext("citric Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$residual.sugar, horizontal = FALSE, col="slategray2", pch=19)
mtext("Residual sugar", cex=0.8, side=1, line=2)
boxplot(white_wine$chlorides, horizontal = FALSE, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(white_wine$free.sulfur.dioxide, horizontal = FALSE, col="slategray2", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(white_wine$total.sulfur.dioxide,horizontal = FALSE, col="slategray2", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(white_wine$density,horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(white_wine$pH, horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(white_wine$sulphates,horizontal = FALSE, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(white_wine$alcohol,horizontal = FALSE, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

###########################################################################################
# Removing the outliers
###########################################################################################
outliers = rep(0,11)

for (i in 1:11){
  t1 <- quantile(white_wine[,i], 0.75)
  t2 <- IQR(white_wine[,i], 0.75)
  outliers[i] <- t1 + 1.5*t2
}
white_wine_index = matrix(0, 4898, 11)
for (i in 1:4898)
  for (j in 1:11){
    if (white_wine[i,j] > outliers[j]) white_wine_index[i,j] = 1
  }
w_index = apply(white_wine_index, 1, sum)
white_wine_data = cbind(w_index, white_wine)
index = rep(0)

j = 1
for (i in 1:4898){
  if (w_index[i] > 0) {index[j]= i
  j = j + 1}
  else j = j
}

new_white_wine = white_wine[-index,]

#After removing outliers, we get 4074 observations

############################################################################################
# Box plot after removing outliers
############################################################################################

par(mfrow = c(3,4))
#boxplot(fixed.acidity)
boxplot(new_white_wine$fixed.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$volatile.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$citric.acid, horizontal = FALSE, col="slategray2", pch=19)
mtext("citric Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$residual.sugar, horizontal = FALSE, col="slategray2", pch=19)
mtext("Residual sugar", cex=0.8, side=1, line=2)
boxplot(new_white_wine$chlorides, horizontal = FALSE, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(new_white_wine$free.sulfur.dioxide, horizontal = FALSE, col="slategray2", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(new_white_wine$total.sulfur.dioxide,horizontal = FALSE, col="slategray2", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(new_white_wine$density,horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(new_white_wine$pH, horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(new_white_wine$sulphates,horizontal = FALSE, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(new_white_wine$alcohol,horizontal = FALSE, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)

##########################################################################################
###########################################################################################
#correlation and feature selection based on correlation (Manual feature selection)
###########################################################################################
cor_white_wine = cor(new_white_wine)
par(mfrow =c(1,1))
corrplot(cor_white_wine, method = "number")
#cor_white_wine 
# volatile.acidity, residual.sugar, chloride, density, pH, alcohol 

###########################################################################################
# collinearity
###########################################################################################
new_white_wine_mfs = new_white_wine[,c("volatile.acidity","residual.sugar","chlorides","density","pH","alcohol","quality")]
fit1 = lm(quality~., data = new_white_wine_mfs)
summary(fit1)
vif(fit1)
which.max(vif(fit1)) # density has higher collinearity (16.7) so removed
#whitewine.revised = whitewine[,c("volatile.acidity","fixed.acidity","residual.sugar","free.sulfur.dioxide","density","pH","sulphates","alcohol","quality")]
#new_white_wine_mf = new_white_wine[,-c("density")]
new_white_wine_mfs = subset(new_white_wine_mfs,select = -c(density))

##########################################################################################
# create train and test set 
###########################################################################################
set.seed(789)
train = sample(1:nrow(new_white_wine_mfs), 0.7*nrow(new_white_wine_mfs))
white_train_mfs = new_white_wine_mfs[train,]
white_test_mfs = new_white_wine_mfs[-train,]
white_y_train_mfs = white_train_mfs$quality
white_y_test_mfs = white_test_mfs$quality

###########################################################################################
# Multiple Regression for manual Feature selection 
###########################################################################################
white_lm_mfs = lm(quality~., data = white_train_mfs)
summary(white_lm_mfs)
white_predict_lm_mfs = predict.lm(white_lm_mfs, newdata = white_test_mfs)
white_error_prediction_lm_mfs = mean((white_predict_lm_mfs - white_y_test_mfs)^2)
# The error in multiple regression is 0.5659304

###########################################################################################
# Support Vector Machine for manual Feature selection 
###########################################################################################
#data(new_white_wine_mfs)


# discretize the data
med = median(new_white_wine_mfs$quality)  # 6
quality =  ifelse(new_white_wine_mfs$quality <= med, "No", "Yes")
cbind(quality, new_white_wine_mfs$quality)

# create a new data set
svm_white <- data.frame(new_white_wine_mfs[,-c(6)], quality)

# Divide into test and train
set.seed(1289963)
test_i = sample(1:nrow(svm_white), 1/3*nrow(svm_white))
test = svm_white[test_i, ]
train = svm_white[-test_i, ]

# SVM with a linear kernel
tune.model <- tune(svm, quality~., data = train, kernel = "linear",
                   ranges = list(cost = c(0.001, 0.01, 1, 5, 10, 100)))
tune.model # best performance: 0.2426606, cost= 0.001
summary(tune.model)

bestmod <- tune.model$best.model
bestmod # Number of Support Vectors:  1324

# predict the test data
y_hat <- predict(bestmod, newdata = test)
y_true <- test$quality
accur_lin <- length(which(y_hat == y_true))/length(y_true)
accur_lin #  0.7820324

table(predict = y_hat, truth = y_true)

# SVM with a radial kernel
tune.model.rad <- tune(svm, quality~., data = train, kernel = "radial",
                   ranges = list(cost = c(0.001, 0.01, 1, 5, 10, 100)))
tune.model.rad # best performance: 0.2036032, cost= 10
summary(tune.model.rad)

bestmod <- tune.model.rad$best.model
bestmod # Number of Support Vectors:  1255

# predict the test data
y_hat <- predict(bestmod, newdata = test)
y_true <- test$quality
accur_rad <- length(which(y_hat == y_true))/length(y_true)
accur_rad #  0.8188513

table(predict = y_hat, truth = y_true)
##########################################################################################
# Automatic Feature selection (best subset selection)
##########################################################################################
regfit.full = regsubsets(quality~.,data = new_white_wine, nvmax = 11)
reg.summary = summary(regfit.full)
#An asterisk indicates that a given variable is included in the corresponding model.
#names(reg.summary)

par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",ylab=" Adjusted RSq",type="l")
max = which.max (reg.summary$adjr2) #9
points(max, reg.summary$adjr2[max], col ="red",cex =1.5, pch =24)
plot(reg.summary$cp ,xlab=" Number of Variables ",ylab=" cp",type="l")
mincp = which.min (reg.summary$cp) #9
points(mincp, reg.summary$cp[mincp], col ="red",cex =1.5, pch =25)
plot(reg.summary$bic ,xlab =" Number of Variables ",ylab=" bic",type="l")
minbic = which.min (reg.summary$bic) #8
points(minbic, reg.summary$bic[minbic], col ="red",cex =1.5, pch =25)
max = which.max (reg.summary$adjr2) #9

##


######################################################################################
# 10-fold CV for model selection
######################################################################################
set.seed (17)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k=10
set.seed (1)
folds=sample (1:k,nrow(new_white_wine),replace =TRUE)

cv.errors = matrix(NA,10,11)
for (j in 1:k) {
  best.fit = regsubsets(quality~ ., data = new_white_wine[folds != j,], nvmax = 11)
  for (i in 1:11) {
    pred = predict(best.fit, new_white_wine[folds == j, ], id = i)
    cv.errors[j, i] = mean((new_white_wine$quality[folds == j] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
rmse.cv
which.min(rmse.cv)
# 10-fold CV indicates for 9-variable model
# fixed.acidity+volatile.acidity+residual.sugar+chlorides+free.sulfur.dioxide+density+pH+sulphates+alcohol
######################################################################################
# Comparing Training error and test error for all models
######################################################################################
###Training and test set for best subset
set.seed(100)
ww_train = sample(1:nrow(new_white_wine), round(0.75*nrow(new_white_wine)))
ww_training_data = new_white_wine[ww_train,]
ww_test_data = new_white_wine[-ww_train,]
Y.train = new_white_wine$quality[ww_train]
Y.test = new_white_wine$quality[-ww_train]

## best subset linear regression
best_subset = regsubsets(quality~.,ww_training_data,nvmax=11)
subset_summary = summary(best_subset)

a = lm(quality~.,ww_training_data)
summary(a)   # 9 significant variables

select = subset_summary$outmat


# We will proceed with 9-variable model

mincp = which.min(subset_summary$cp)
features_selected = names(coef(best_subset,mincp))
features_selected = features_selected[-1]

#"fixed.acidity","volatile.acidity","residual.sugar","chlorides","free.sulfur.dioxide",
#"density","pH","sulphates","alcohol"

final_ww_model = new_white_wine[,c("quality",features_selected)]

c = cor(final_ww_model)
par(mfrow =c(1,1))
corrplot(c,method = "number")  
fit2 = lm(quality~., data = final_ww_model)
summary(fit2)
vif(fit2)
# density is highly correlated residual sugar & alcohol and its variance inflation factor is high = 43.680487
# So we have totally 8 predictors + response (quality)
final_ww_model = final_ww_model[,-c(7)]
c = cor(final_ww_model)
corrplot(c,method = "number")
##########################################################################################
# Create Training and Test set for final White Wine Model
###########################################################################################
set.seed(789)
train = sample(1:nrow(final_ww_model), 0.75*nrow(final_ww_model))
white_train_afs = final_ww_model[train,]
white_test_afs  = final_ww_model[-train,]
white_y_train_afs = final_ww_model$quality[train]
white_y_test_afs = final_ww_model$quality[-train]

###########################################################################################
# Multiple Regression for Automatic Feature selection (best subset selection)
###########################################################################################
white_lm_afs = lm(quality~., data = white_train_afs) #RSE = 0.7446
white_predict_lm_afs = predict.lm(white_lm_afs, newdata = white_test_afs)
summary(white_predict_lm_afs)
white_error_prediction_lm_afs = mean((white_predict_lm_afs - white_y_test_afs)^2)
# The error in multiple regression is 0.5771251
white_error_prediction_lm_afs   #0.5771251

###########################################################################################
# Decision Trees
###########################################################################################

##########Regression Tree################
model.control = rpart.control(minsplit = 5,xval = 10,cp=0)
reg_tree = rpart(quality~.,data = white_train_afs,method = "anova",control = model.control)
pred = predict(reg_tree, white_test_afs)
mean((pred-white_y_test_afs)^2) # 0.6452813

# Pruning the Regression Tree
plot(reg_tree$cptable[,4],type="o",main = "Cp for model selection",ylab = "cv error")
min_cp=which.min(reg_tree$cptable[,4])
prune_fit = prune(reg_tree,cp=reg_tree$cptable[min_cp,1])

# Test MSE for pruned tree
yhat = predict(prune_fit, white_test_afs)
reg_tree_err = mean((yhat-white_y_test_afs)^2) #0.5439929

#######Classification Tree#############
table(final_ww_model$quality)

# 1 - Bad, 2 - Good
final_ww_model$wine_quality[final_ww_model$quality<=6 ] = 1
final_ww_model$wine_quality[final_ww_model$quality>=7] = 2
final_ww_model$wine_quality = as.factor(final_ww_model$wine_quality)

table(final_ww_model$wine_quality)

# taking density out
Classification_model = final_ww_model[,c("wine_quality",features_selected[-6])]

# Divide the classification model into training and test set
set.seed(100)
classif_train = sample(1:nrow(Classification_model), round(0.75*nrow(Classification_model)))
classif_training_data = Classification_model[classif_train,]
classif_test_data = Classification_model[-classif_train,]
Y.train = Classification_model$wine_quality[classif_train]
Y.test = Classification_model$wine_quality[-classif_train]

model.control = rpart.control(minsplit = 5,xval = 10,cp=0)
wine_tree_fit = rpart(wine_quality~., data =classif_training_data,method = "class",control = model.control)

## Predict with test data
table(predict(wine_tree_fit, classif_test_data, type = "class"),Y.test)

# Pruning the Classification tree
plot(wine_tree_fit$cptable[,4],type="o",main = "Cp for model selection",ylab = "cv error")
min_cp=which.min(wine_tree_fit$cptable[,4])
prune_fit = prune(wine_tree_fit,cp=wine_tree_fit$cptable[min_cp,1])

# Prediction with pruned tree
table(predict(prune_fit, classif_test_data, type = "class"),Y.test)
# 27.6% accuracy rate
yhat_class = predict(prune_fit, classif_test_data)
class_tree_err = mean((yhat_class-as.numeric(classif_test_data$wine_quality))^2) #0.8045777
###############################################################################
# Boosting for regression tree
##############################################################################
set.seed(1)
par(mfrow =c(1,1))
reg_boost = gbm(quality~.,data=white_train_afs, distribution = "gaussian",
                n.trees = 5000,interaction.depth = 4,shrinkage = 0.1,
                verbose = F)
summary(reg_boost)

yhat.boost = predict(reg_boost, newdata = white_test_afs,n.trees = 5000)
reg_boost_err = mean((yhat.boost-white_y_test_afs)^2)# 0.4458032

###############################################################################
# Boosting for Classification tree
##############################################################################
set.seed(1)
# 0 - bad,1-good
boost_training = classif_training_data
boost_training$wine_quality = as.numeric(classif_training_data$wine_quality)-1 
boost_test = classif_test_data
boost_test$wine_quality = as.numeric(classif_test_data$wine_quality)-1


class_boost = gbm(wine_quality~.,data=boost_training,distribution = "bernoulli",
                  n.trees = 5000,interaction.depth = 4,shrinkage = 0.1)

summary(class_boost)
class_pred = predict(class_boost,newdata = boost_test,n.trees = 5000,type = "response")
class_boost_err = mean((class_pred - boost_test$wine_quality)^2) # 0.1207259

###############################################################################
# Support Vector Machine
##############################################################################

# Divide the classification model into training and test set
set.seed(100)
classif_train = sample(1:nrow(Classification_model), round(0.75*nrow(Classification_model)))
classif_training_data = Classification_model[classif_train,]
classif_test_data = Classification_model[-classif_train,]
Y.train = Classification_model$wine_quality[classif_train]
Y.test = Classification_model$wine_quality[-classif_train]

# We know that if quality >=7 its marked as GOOD(2) else its BAD(1)
cbind(Classification_model$wine_quality, final_ww_model$quality)

# SVM with a linear kernel
# we use tune() function to perform cross validation - by default 10 fold CV
tune.model <- tune(svm, wine_quality~., data = classif_training_data, kernel = "linear",
                   ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
tune.model # cost = 0.01 which is the lowest cross validation error , best performance = 0.2431265

summary(tune.model)

bestmod <- tune.model$best.model
bestmod
# Number of Support Vectors:  1511
# predict the test data
y_hat <- predict(bestmod, newdata = classif_test_data)
y_true <- Y.test
accur_lin <- length(which(y_hat == y_true))/length(y_true)
accur_lin #  0.7917485

table(predict = y_hat, truth = y_true)

#########################################################################################
# SVM with a radial kernel
# we use tune() function to perform cross validation - by default 10 fold CV
tune.model.rad <- tune(svm, wine_quality~., data = classif_training_data, kernel = "radial",
                   ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
tune.model.rad # cost = 10 which is the lowest cross validation error , best performance = 0.1960077

summary(tune.model.rad)

bestmod <- tune.model.rad$best.model
bestmod
# Number of Support Vectors:  1429
# predict the test data
y_hat <- predict(bestmod, newdata = classif_test_data)
y_true <- Y.test
accur_rad <- length(which(y_hat == y_true))/length(y_true)
accur_rad #  0.8447937
svm_err = 1 - accur_rad

table(predict = y_hat, truth = y_true)

#########################################################################################

##############################################################################
# Error and Accuracy Plotting
##############################################################################

reg_error = c(white_error_prediction_lm_afs,reg_tree_err,reg_boost_err)
reg_models = c("Multiple Reg", 'Reg Tree', "Boost RT")
reg_err_tb = data.frame(reg_models,reg_error)
plot(reg_err_tb$reg_models,reg_err_tb$reg_error,main = 'Error Rates for Regression Models', xlab = "Models", ylab = "Error")

class_error = c(class_tree_err,class_boost_err,svm_err)
class_models = c("Classification Tree","Boost CT", "SVM")
class_err_tb = data.frame(class_models,class_error)
plot(class_err_tb$class_models,class_err_tb$class_error,main = 'Error Rates for Classification Models', xlab = "Models", ylab = "Error")
class_err_tb$class_error

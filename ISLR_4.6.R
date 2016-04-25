#ISLR chapter 4.6 Lab = Stock Market Data:

library(ISLR)
library(dplyr)
library(ggplot2)
library(ranger)
library(xgboost)
library(glmnet)
library(LiblineaR)
library(MASS)
library(ggplot2)

data("Smarket")

Smarket$Volume <- scale(Smarket$Volume)

train <- Smarket[Smarket$Year %in% c(2001, 2002, 2003, 2004),]
test <- Smarket[Smarket$Year %in% c(2005),]

train_feat <- train[,c(2:7, 9)]

plot(train$Lag1)
cor(train$Lag1, train$Today)
plot(train$Volume, type = "b")



#we want to predict if it goes up or down:

model_rf <- ranger(Direction ~ Lag1 + Lag2 + Volume,
                   data = train,
                   write.forest = TRUE,
                   importance = "impurity")
#48.3 OOB error


model_rf2 <- randomForest::randomForest(Direction ~ Lag1 + Lag2 + Volume,
                                        data = train)
plot(model_rf2)

mean(predict(model_rf, test)$prediction == test$Direction)
#accuracy is around with all predictors 0.492% bad!
#but if we limit to lag1 + lag2 + Volume we get around 0.51% good!

#how to do best subset selection using CV?
# we can just try looking at model importance first:
model_rf$variable.importance #in this case doesn't tell us much,

#let's try maybe permutations?
expand.grid(0:1, 0:1, 0:1, 0:1, 0:1, 0:1) -> perms

sapply(2: dim(perms)[1], function(i) {
    as.numeric(perms[i,]) == 1  -> coeff #
    coeff[length(coeff) + 1] <- TRUE #add the last element.
    train_feat[,coeff] -> temp_train
    model <- ranger(Direction ~ ., data = temp_train, num.trees = 100)
    model$prediction.error }) -> errors

which.min(errors) + 1 -> best_perm #you add one here since you start from 2:
min(errors)

perms[best_perm,] == 1 #the columns selected
names(train_feat)[perms[best_perm,] == 1] #the best feautures!!!

#~~~~~~~~
#xgboost:
#~~~~~~~~

X <- model.matrix(Direction ~ Lag1 + Lag2, data = train)[,-1]
X_test <- model.matrix(Direction ~ Lag1 + Lag2, data = test)[,-1]
y <- as.numeric(train$Direction) - 1
y_test <- as.numeric(test$Direction) - 1

    
nrounds <- 100
cv.error <- xgb.cv(data = X, label = y, nrounds = nrounds,
                   objective = "binary:logistic",
                   eval_metric = "logloss",
                   eval_metric = "error",
                   max_depth = 1,
                   eta = 0.01,
                   nfold = 30)


plot(cv.error$test.error.mean)
plot(cv.error$test.error.mean + 2*cv.error$train.error.std)
plot(cv.error$test.logloss.mean)

min(cv.error$test.error.mean)

model_xgb <- xgboost(data = X, label = y, nrounds = 1,
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     eval_metric = "error",
                     max_depth = 2,
                     eta = 0.01)

mean(1*(predict(model_xgb, X_test) > 0.5) == y_test)

#54.7 accuracy - it seems like growing more trees is not improving anything.

#~~~~~~~~~~~~~~~~~~~~
#logistic regression:
#~~~~~~~~~~~~~~~~~~~~

model_glm <- cv.glmnet(X, as.factor(y), family = "binomial",
                       type.measure = "class")

model_lin <- LiblineaR(X, as.factor(y), type = 5, cross = 10)

#hardly more than random guessing it seems:

model_glm <- glm(Direction ~ poly(Lag1,2) + Lag2,
                 data = train, family = binomial)

predict(model_glm, newdata = test, type = "response") -> temp

mean(1*(temp > 0.5) == y_test)
#0.4761905 accuracy - bad! and also not the same as in the ISLR book,
#this is because of Volume - very weirdddddddddd.

#0.5595238 if you eliminate Volume

# 0.5714286 if we add Lag as a second degree poly.

#some visualization:
plotly::ggplotly(
ggplot(train, aes(x = Lag1, y = Lag2)) +
    geom_point(aes(color = Direction))
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Quadratic discriminant analysis:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
model_qda <- qda(Direction ~ Lag1 + Lag2,
                 data = train)

mean(predict(model_qda, test)$class == test$Direction)
#0.5992063 this is impressive - much better than everything else.
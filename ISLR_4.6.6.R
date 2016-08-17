#ISLR 4.6.6
#An Application to CAravan Insurance Data:
library(ISLR)
library(dplyr)
library(glmnet)
library(caret)
library(xgboost)
library(ROCR)

data(Caravan)

X = model.matrix(Purchase ~ ., data = Caravan)
y = Caravan$Purchase

Caravan %>% count(Purchase)
#only 6% percent of the people buy insurance.

#~~~~~~~~~~~~~~~~~~~~~~~~
#reg logistic regression:
#~~~~~~~~~~~~~~~~~~~~~~~~

model_lasso <- cv.glmnet(X, y, nfolds = 3,
                         family = "binomial", type.measure = "auc")

plot(model_lasso)
model_lasso$cvm %>% max
#0.7394179


X = model.matrix(Purchase ~ (.)^2, data = Caravan) #add interactions.
model_lasso <- cv.glmnet(X, y, nfolds = 3,
                         family = "binomial", type.measure = "auc")


#~~~~~~~~~~~~~~~~~~
#gradient boosting:
#~~~~~~~~~~~~~~~~~~
y_xgb <- as.numeric(y) - 1
y_xgb_test <- y_xgb[-sam]
y_xgb_train <- y_xgb[sam]

xgb.cv(data = X, label = y_xgb,
       nrounds = 200,
       #booster = "gblinear",
       objective = "binary:logistic",
       nfold = 10,
       stratified = TRUE,
       eta = 0.1,
       alpha = 1,
       max_depth = 3,
       metrics = c("auc"),
       #early.stop.round = 5,
       maximize = TRUE) -> cv.error

plot(cv.error$test.auc.mean)
#plot(cv.error$test.logloss.mean)

max(cv.error$test.auc.mean)
which.max(cv.error$test.auc.mean)

#gblinear: 0.738
#gbtree_1: 0.766
#gbtree_2: 0.772
#gbtree_3: 0.763


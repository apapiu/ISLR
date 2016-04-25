#ISLR 4.6.6
#An Application to CAravan Insurance Data:
library(ISLR)
library(dplyr)
library(glmnet)
library(caret)
library(xgboost)
library(ROCR)

data(Caravan)

X <- as.matrix(Caravan[,-86])
y <- Caravan$Purchase

Caravan %>% count(Purchase)
#only 6% percent of the people buy insurance.
set.seed(213)
sam <- sample(1:dim(Caravan)[1], dim(Caravan)[1]*0.8)
train <- Caravan[sam,] 
test <- Caravan[-sam,]
X_train <- X[sam,]
X_test <- X[-sam,]
y_train <- y[sam]
y_test <- y[-sam]

#~~~~~~~~~~~~~~~~~~~~
#logistic regression:
#~~~~~~~~~~~~~~~~~~~~

model_lasso <- cv.glmnet(X_train, y_train, nfolds = 10,
                         family = "binomial")

plot(model_lasso)
#interestingly using all the features gives significantly higher Binomial Deviance

model_lasso_2 <- cv.glmnet(X_train, y_train, nfolds = 10,
                         family = "binomial", type.measure="auc")

plot(model_lasso_2)

max(model_lasso_2$cvm)

predict(model_lasso, X_test,
        s = model_lasso$lambda.min,
        type = "response") -> preds

confusionMatrix(1*(preds > 0.18),  as.numeric(y_test) - 1)

pos <- preds[y_test == "Yes"]
neg <- preds[y_test == "No"]

mean(replicate(10000, sample(pos,1) > sample(neg,1))) #approximating AUC

#computing AUC using the package ROCR
pred = prediction(preds, y_test)
roc = performance(pred, "tpr", "fpr")
plot(roc)

performance(pred, "auc")@y.values


#how abut logloss?

preds_df <- cbind(1-preds, preds)
y_df <- data.frame(y_test)

y_mat <- model.matrix(~ 0 + y_test, y_df)

MultiLogLoss(y_mat, preds_df)

#now let's perturb things a bit:
preds[preds < 0.01] <- preds[preds < 0.01] + 0.01

#~~~~~~~~~~~~~~~~~~
#gradient boosting:
#~~~~~~~~~~~~~~~~~~
y_xgb <- as.numeric(y) - 1
y_xgb_test <- y_xgb[-sam]
y_xgb_train <- y_xgb[sam]


nrounds = 25
xgb.cv(data = X_train, label = y_xgb_train,
       nrounds = nrounds, objective = "binary:logistic",
       nfold = 5,
       stratified = TRUE,
       eta = 0.3,
       max_depth = 2,
       metrics = c("auc", "logloss")) -> cv.error

plot(cv.error$test.auc.mean)
#plot(cv.error$test.logloss.mean)

max(cv.error$test.auc.mean)
which.max(cv.error$test.auc.mean)

#0.770 with depth = 2, nrounds = 25

model_xgb <- xgboost(data = X_train, label = y_xgb_train,
                     nrounds = nrounds, objective = "binary:logistic",
                     eta = 0.3,
                     max_depth = 2)

predict(model_xgb, X_test) -> preds_xgb


pred = prediction(preds_xgb, y_test)
roc = performance(pred, "tpr", "fpr")
plot(roc)

performance(pred, "auc")@y.values #this gives AUC.



#~~~~
#knn:
#~~~~

model_knn <- 

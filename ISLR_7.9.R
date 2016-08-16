#Chapter 7
#Exercise 6 p 299
head(Auto)

library(GGally)
ggpairs(Auto[,1:6])

plot(Auto[,1:6])


model = glm(mpg ~ cylinders + weight + acceleration + displacement+ year + as.factor(origin) , data = Auto)
cv.glm(Auto, model, K = 5)$delta %>% sqrt()
#3.441586

replicate(n = 20,
cv.glm(Auto,
       glm(mpg ~ cylinders +
                 ns(log(displacement), 3) +
                 ns(weight, 3) +
                 ns(horsepower, 3) + 
                 acceleration + 
                 ns(year, 3) +
                 as.factor(origin), data = Auto),
       K = 5)$delta %>% sqrt()) %>% mean()

#2.82 with added quadratic terms - impressive


#adding interactions - still can't get better than non-linear model:
Auto %>% 
    mutate(origin = as.factor(origin)) -> Auto

X = model.matrix(mpg ~ (. - name)^3 + ns(log(displacement), 3) +
                     ns(weight, 3), data = Auto)
model = cv.glmnet(X, Auto$mpg)
model$cvm %>% min %>% sqrt

formula = mpg ~ .  - name - origin + as.factor(origin) - 1
X = model.matrix(formula, Auto)


#random forests:
replicate(20, {
model = ranger(formula, data = Auto);
model$prediction.error %>% sqrt()}) %>% mean()

#with a random forest: 2.7072


#~~~~~~~~~~
#ex. 10 page 300:
head(College)
View(College)

College %>% 
    select(Outstate, Enroll, Top10perc, Top25perc, P.Undergrad, Books, Personal, PhD) %>% 
    ggpairs() #this is a bit slow.


College %>% 
    select(Outstate, Enroll, Top10perc, Top25perc, P.Undergrad, Books, Personal, PhD, Expend, Grad.Rate) %>% 
    plot()

#hmm Expend seems highly correlated:

College %>% 
    ggplot(aes(x = log(Expend), y = Outstate)) +
    geom_point() +
    geom_smooth(method = "lm") +
    #facet_wrap(~ Private) +
    geom_smooth(method = "lm", aes(color = Private)) +
    facet_wrap( ~ Private)

#there seems to be an interaction between Private and Expend.
#Let's try a model just with Expend:

model = glm(Outstate ~ Expend, data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()
#3010.183 rmse cv error



College %>% 
    ggplot(aes(x = Expend, y = Outstate)) +
    geom_point() +
    geom_smooth(method = "lm")

hist(predict(model, College) - College$Outstate)
sd(predict(model, College) - College$Outstate) 
mean(predict(model, College) - College$Outstate)
#RMSE is ~ the sd of errors since mean is zero

#a look at the residuals:
College$preds = predict(model, College)
College$residuals = College$Outstate - College$preds

plot(College$preds, College$residuals)

#oh silly me you can do residual diagnostics using plot:
plot(model)


#let's do log transforms, quadratic, bs etc:

model = glm(Outstate ~ log(Expend), data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()

#2623.929

plot(model)
#much better


College %>% 
    ggplot(aes(x = log(Expend), y = Outstate)) +
    geom_point() +
    geom_smooth(method = "lm")

College %>% 
    mutate(preds = predict(model, College)) %>% 
    ggplot(aes(x = Expend, y = Outstate)) +
    geom_point() +
    geom_point(aes(y = preds), color = "red") +
    geom_smooth()


#using splines:
model = glm(Outstate ~ bs(Expend, 4), data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()
plot(model, which = 1)

#2556.926 - even better than log - similar results with a 3rd degree poly




College %>% 
    mutate(preds = predict(model, College)) %>% 
    ggplot(aes(x = log(Expend), y = Outstate)) +
    geom_point() +
    geom_point(aes(y = preds), color = "red")

#adding the Private dummy var:

model = glm(Outstate ~ ns(Expend, 4) + Private, data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()
plot(model)

#2204.969


X = model.matrix( ~ Private*(ns(Expend, 3) + ns(Room.Board, 3),data = College)
                  
model = glm(Outstate ~ Private*(ns(Expend, 3) +
                                ns(Room.Board, 3) +
                                ns(Grad.Rate, 3)), data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()


model = glm(Outstate ~ Private*ns(Expend, 3)*ns(Room.Board, 3)*ns(Grad.Rate, 3), data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()


#glmnet on all possible interactions - this blows up fast...
X = model.matrix(Outstate ~ 
                     Private*ns(Expend, 2)*ns(Room.Board, 2)*
                     ns(Grad.Rate, 2)*ns(S.F.Ratio,2)*
                     ns(perc.alumni, 2)*
                     ns(Top10perc, 2)*
                     ns(PhD, 2)*
                     ns(Terminal, 2),
                 data = College)
model = cv.glmnet(X, College$Outstate)
model$cvm %>% min %>% sqrt

#and you still don't have the same results as the tree based models.

#how about limit yourself to three way interactions?

X = model.matrix(Outstate ~ Private*(ns(Expend, 3) +
                                         ns(Room.Board, 2) + 
                                         ns(Grad.Rate, 2) +
                                         ns(S.F.Ratio, 2) + 
                                         ns(perc.alumni, 2) + 
                                         ns(Top10perc, 2) + 
                                         ns(PhD, 2) +
                                         ns(Terminal, 2))^3,
                 data = College)
model = cv.glmnet(X, College$Outstate)
model$cvm %>% min %>% sqrt






#2124.559 with the added interaction
#use Private*ns(Expend, 4) to also get the intercept to be different,  Private:ns(Expend, 4) also works

College %>% 
    mutate(preds = predict(model, College)) %>% 
    ggplot(aes(x = Expend, y = Outstate)) +
    geom_point() +
    geom_point(aes(y = preds), color = "red")

#so this does as expected: run a new model on each categorical value.

#~~~~~~~~~~~~~~~~
#let's try a different feature:
#


model = glm(Outstate ~  Top25perc*Private, data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()


College %>% 
    mutate(preds = predict(model, College)) %>% 
    ggplot(aes(x = Top25perc, y = Outstate, col = Private)) +
    geom_point() +
    geom_smooth(method = "lm", aes(col = Private)) +
    geom_point(aes(y = preds), color = "red")



#let's try all the features:

College = select(College, - preds, -residuals)

X = model.matrix( ~ )

model = glm( Outstate ~ .,
             data = College)
cv.glm(College, model, K = 5)$delta %>% sqrt()

#1999.077

#now add all the 2-way interactions interactions:

X = model.matrix(Outstate ~ (.)^2 - Expend + ns(Expend, 3) -
                                    Room.Board + ns(Room.Board, 3), data = College)

model = cv.glmnet(X, College$Outstate)
min(model$cvm) %>% sqrt()

#~~~~~~~~~~~~~~~~~
#random forest:

model = ranger(Outstate ~ ., data = College,
               importance = "impurity",
               write.forest = TRUE)
model$prediction.error %>% sqrt()

#1725.988

model$variable.importance %>% sort %>% barplot()

preds = predict(model, College)$predictions

College %>% 
    mutate(preds = preds) %>% 
    ggplot(aes(x = Expend, y = Outstate)) +
    geom_point() +
    #geom_smooth(method = "lm", aes(col = Private)) +
    geom_point(aes(y = preds), color = "red")


#xgboost:
library(xgboost)

X = sparse.model.matrix(Outstate ~ (.)^2, data = College)

errors = xgb.cv(data = X,
                label = College$Outstate,
                booster = "gblinear",
                max_depth = 1,
                eta = 0.3,
                alpha = 0.3,
                nrounds = 400,
                nfold = 10)

errors$test.rmse.mean %>% min

#gblinear < gbtree(1) < gbtree(2) < gbtree(3)
#btree(1) - gblinear = non-linear features
#gbtree(2) - gbtree(1) = two-way interactions
#gblinear is an additive model, gbtree(1) is an additive model (no interactions)

#feature engineering:

College %>% 
    mutate(accep_rate = Accept/ Apps) -> College

model = ranger( Outstate ~ ., data = College)
model$prediction.error %>% sqrt

#1705.694 - accep rate does help a bit.

#so adding quadratic terms or splines and interactions can help linear models, especially when using
#the lasso but it's hard to get as good as tree ensembles in terms of prediction.

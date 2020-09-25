library(mlr)


wine= read.csv("05_wine_EX1.csv")



library(corrplot)

corWine=cor(wine)

corrplot(corWine,method = "circle",type = "lower")

wine$Year=wine$FrancePop=NULL

## Make Learning Task

wine.task= makeRegrTask(data=wine, target = "Price")
wine.learner=makeLearner("regr.lm")
wine.model=train(wine.learner,wine.task)

model= getLearnerModel(wine.model)

summary(model)



##LASSO REGRESSION (in azure it will be called as L1 and L2 regularaisation)
## Least absolute shrinkage
install.packages("glmnet")
library(glmnet)
glmnet.learnerLASSO=makeLearner("regr.glmnet", alpha= 1, predict.type = "response")
glmnet.modelLasso= train(glmnet.learnerLASSO, wine.task)

glmnetLassoModel= getLearnerModel(glmnet.modelLasso)

install.packages("plotmo")
library(plotmo) # for plot_glmnet
plot_glmnet(glmnetLassoModel)                             # default colors
plot_glmnet(glmnetLassoModel, label=5)

## RIDGE REGRESSION

glmnet.learnerRIDGE=makeLearner("regr.glmnet", alpha= 0, predict.type = "response")
glmnet.modelRIDGE= train(glmnet.learnerRIDGE, wine.task)

glmnetRIDGEModel= getLearnerModel(glmnet.modelRIDGE)

plot_glmnet(glmnetRIDGEModel)                             # default colors
plot_glmnet(glmnetRIDGEModel, label=5)


library(mlr)


diamonds= read.csv("05_Diamonds.csv")

diamondsCor=diamonds[6:11]
diamondsCor$carat=diamonds$carat

library(corrplot)

cor=cor(diamondsCor)

corrplot(cor,method = "circle",type = "lower")

diamondsCor$x=diamondsCor$y=diamondsCor$z=NULL
diamondsCor$color=diamonds$color
diamondsCor$cut=diamonds$cut
diamondsCor$clarity=diamonds$clarity

## Make Learning Task

diamonds.task= makeRegrTask(data=diamondsCor, target = "price")
diamonds.learner=makeLearner("regr.lm")
diamonds.model=train(diamonds.learner,diamonds.task)

model= getLearnerModel(diamonds.model)

summary(model)

glmnet.learnerDiaLASSO=makeLearner("regr.glmnet", alpha= 1, predict.type = "response")
glmnet.modelDiaLasso= train(glmnet.learnerDiaLASSO, diamonds.task)

glmnetDiaLassoModel= getLearnerModel(glmnet.modelDiaLasso)

install.packages("plotmo")
library(plotmo) # for plot_glmnet
plot_glmnet(glmnetDiaLassoModel)                             # default colors
plot_glmnet(glmnetDiaLassoModel, label=5)

glmnet.learnerDiaRidge=makeLearner("regr.glmnet", alpha= 0, predict.type = "response")
glmnet.modelDiaRIDGE= train(glmnet.learnerDiaRidge, diamonds.task)

glmnetDiaRIDGEModel= getLearnerModel(glmnet.modelDiaRIDGE)

install.packages("plotmo")
library(plotmo) # for plot_glmnet
plot_glmnet(glmnetDiaRIDGEModel)                             # default colors
plot_glmnet(glmnetDiaRIDGEModel, label=5)

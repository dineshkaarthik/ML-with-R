library(help = "datasets")
iris = iris
names(iris)

########### Decision Trees ############


## installing rpart package for CART
## install.packages("rpart")
## install.packages("rpart.plot")


## loading the library
library(rpart)
library(rpart.plot)


## setting the control parameter inputs for rpart
r.ctrl = rpart.control(minsplit=10, minbucket = 3, cp = 0, xval = 10)


## calling the rpart function to build the tree
m1 <- rpart(formula = Species ~ ., data = iris, method = "class", control = r.ctrl)

m1


#Plot the tree
rpart.plot(m1)


## to find how the tree performs and derive cp value
printcp(m1)
plotcp(m1)


## Predicting classes and probabilities
class <- predict(m1, iris, type="class")
prob <- predict(m1, iris)

class
prob


iris$predicted.Dtree = class

## Confusion Matrix
library(caret)

confusionMatrix(iris$Species,iris$predicted.Dtree)




########### Random Forest ############
remove(iris)
iris = iris

library(randomForest)

## Calling syntax to build the Random Forest
RF <- randomForest(Species ~ ., data = iris, 
                   ntree=301, mtry = 2, nodesize = 3,
                   importance=TRUE)

print(RF)

plot(RF)

RF$err.rate

RF$importance

#Tune RF
tRF <- tuneRF(x = iris[,-5], 
              y=Species,
              mtryStart = 2, 
              ntreeTry=301, 
              stepFactor = 1, 
              improve = 0.000001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 3, 
              importance=TRUE
)

## Predicting prob and class
class <- predict(tRF, iris, type="class")
prob <- predict(tRF, iris, type="prob")

iris$predicted.RF = class

## Confusion Matrix
library(caret)

confusionMatrix(iris$Species,iris$predicted.RF)


########### Neural Networks ############
remove(iris)
iris = iris

library(neuralnet)

nn1 <- neuralnet(formula = Species ~ . , 
                 data = iris, 
                 hidden = 4,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)


plot (nn1)

## Predicting prob
pred <- predict(nn1, iris)

round(pred,digits = 3)






                           
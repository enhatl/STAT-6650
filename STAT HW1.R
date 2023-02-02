library(tree)
library(ISLR)
attach(Carseats)

# a
set.seed(1)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
car.train = Carseats[train, ]
car.test = Carseats[-train, ]

# b
car.tree = tree(Sales ~., data = car.train)
plot(car.tree)
text(car.tree, pretty =0)
summary(car.tree)
car.pred = predict(car.tree, newdata = car.test)
mean((car.pred - car.test$Sales)^2)

# c
set.seed(1)
car.cv = cv.tree(car.tree)
par(mfrow = c(1,2))
plot(car.cv$size, car.cv$dev, type = "b")
plot(car.cv$k, car.cv$dev, type = "b")
par(mfrow = c(1,1))
prune.car = prune.tree(car.tree, best = 7)
plot(prune.car)
text(prune.car, pretty =0)
predict.prune = predict(prune.car, newdata = car.test)
mean((predict.prune - car.test$Sales)^2)

# d
library(randomForest)
set.seed(1)
bag.car = randomForest(Sales ~., data = Carseats, subset = train, mtry = 10, importance= TRUE)
bag.car
predict.bag = predict(bag.car, newdata = car.test)
mean((predict.bag - car.test$Sales)^2)
importance(bag.car)
varImpPlot(bag.car)

# e
set.seed(1)
rf.car1 = randomForest(Sales ~., data = Carseats, subset = train, mtry =1, importance = TRUE)
set.seed(1)
pred.rf1 = predict(rf.car1, newdata = car.test)
mean((pred.rf1 - car.test$Sales)^2)
varImpPlot((rf.car1))
set.seed(1)
rf.car2 = randomForest(Sales ~., data = Carseats, subset = train, mtry = 2, importance= TRUE)
set.seed(1)
pred.rf2 = predict(rf.car2, newdata = car.test)
mean((pred.rf2 - car.test$Sales)^2)
varImpPlot((rf.car2))
set.seed(1)
rf.car3 = randomForest(Sales ~., data = Carseats, subset = train, mtry = 3, importance= TRUE)
set.seed(1)
pred.rf3 = predict(rf.car3, newdata = car.test)
mean((pred.rf3 - car.test$Sales)^2)
varImpPlot((rf.car3))
set.seed(1)
rf.car4 = randomForest(Sales ~., data = Carseats, subset = train, mtry = 4, importance= TRUE)
set.seed(1)
pred.rf4 = predict(rf.car4, newdata = car.test)
mean((pred.rf4 - car.test$Sales)^2)
varImpPlot((rf.car4))
importance(rf.car)
varImpPlot(rf.car)
rm(list = ls())


#4. Generate a simulated two-class data set with 100 observations and two 
# features in which there is a visible but non-linear separation between the 
# two classes. Show that in this setting, a support vector machine with a 
#polynomial kernel (with degree greater than 1) or a radial kernel will 
# outperform a support vector classifier on the training data. Which technique 
# performs best on the test data? Make plots and report training and test error 
# rates in order to back up your assertions.

set.seed(1)
x = matrix(rnorm(100 *2 ), ncol = 2)
x[1:30,] = x[1:30,] + 3
x[31:60,] = x[31:60,] -3
y = c(rep(0,60),rep(1,40))
dat = data.frame(x,y=as.factor(y))
plot(x,col=y+1)

train = sample(100,80)
dat.train = dat[train,]
dat.test = dat[-train]

library(e1071)
svm.lin = svm(y ~ ., data = dat.train, kernel = 'linear', scale = FALSE)
summary(svm.lin)
train.pred = predict(svm.lin, dat.train)
test.pred = predict(svm.lin, dat.test)
mean(train.pred != dat.train$y)
mean(test.pred != dat.test$y)

svm.rad = svm(y ~ ., data = dat.train, kernel = 'radial', scale = FALSE)
summary(svm.rad)
train.pred = predict(svm.rad, dat.train)
test.pred = predict(svm.rad, dat.test)
mean(train.pred != dat.train$y)
mean(test.pred != dat.test$y)

#8. This problem involves the OJ data set which is part of the ISLR package.
# (a) Create a training set containing a random sample of 800 observations, and 
# a test set containing the remaining observations.

library(ISLR)
attach(OJ)
set.seed(2)
train = sample(dim(OJ)[1],800)
train.OJ = OJ[train,]
test.OJ = OJ[-train,]

# (b) Fit a support vector classifier to the training data using cost=0.01, with 
# Purchase as the response and the other variables as predictors. Use the 
# summary() function to produce summary statistics, and describe the results obtained.

library(e1071)
svm.linear = svm(Purchase ~., kernel = 'linear', data = train.OJ, cost=0.01)
summary(svm.linear)

# (c) What are the training and test error rates?

train.pred = predict(svm.linear,train.OJ)
table(train.OJ$Purchase,train.pred)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(svm.linear,test.OJ)
mean(test.pred != test.OJ$Purchase)

# (d) Use the tune() function to select an optimal cost. Consider values 
# in the range 0.01 to 10.

tune.OJ = tune(svm, Purchase ~., data =train.OJ, kernel = 'linear', ranges=list(cost=10^seq(-2,1, by=.5)))
summary(tune.OJ)

# (e) Compute the training and test error rates using this new value for cost.

newsvm.linear = svm(Purchase ~., kernel = 'linear', data = train.OJ, cost=tune.OJ$best.parameters$cost)
train.pred = predict(newsvm.linear, train.OJ)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(newsvm.linear, test.OJ)
mean(test.pred != test.OJ$Purchase)

# (f) Repeat parts (b) through (e) using a support vector machine with a 
# radial kernel. Use the default value for gamma.

svm.radial = svm(Purchase ~., kernel = 'radial', data = train.OJ)
summary(svm.radial)

train.pred = predict(svm.radial,train.OJ)
table(train.OJ$Purchase,train.pred)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(svm.radial,test.OJ)
mean(test.pred != test.OJ$Purchase)

tune.OJ = tune(svm, Purchase ~., data =train.OJ, kernel = 'radial', ranges=list(cost=10^seq(-2,1, by=.5)))
summary(tune.OJ)

newsvm.radial = svm(Purchase ~., kernel = 'radial', data = train.OJ, cost=tune.OJ$best.parameters$cost)
train.pred = predict(newsvm.radial, train.OJ)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(newsvm.radial, test.OJ)
mean(test.pred != test.OJ$Purchase)

# (g) Repeat parts (b) through (e) using a support vector machine with a 
# polynomial kernel. Set degree=2.

svm.poly = svm(Purchase ~., kernel = 'polynomial', data = train.OJ, degree=2)
summary(svm.poly)

train.pred = predict(svm.poly,train.OJ)
table(train.OJ$Purchase,train.pred)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(svm.poly,test.OJ)
mean(test.pred != test.OJ$Purchase)

tune.OJ = tune(svm, Purchase ~., data =train.OJ, kernel = 'linear', ranges=list(cost=10^seq(-2,1, by=.5)))
summary(tune.OJ)

newsvm.poly = svm(Purchase ~., kernel = 'polynomial', data = train.OJ, cost=tune.OJ$best.parameters$cost)
train.pred = predict(newsvm.poly, train.OJ)
mean(train.pred != train.OJ$Purchase)

test.pred = predict(newsvm.poly, test.OJ)
mean(test.pred != test.OJ$Purchase)

# (h) Overall, which approach seems to give the best results on this data?



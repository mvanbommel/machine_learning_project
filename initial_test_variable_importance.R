library(glmnet)
library(BMA)
library(mgcv)
library(rpart)
library(earth)
library(randomForest)

data = read.csv('Data2016.csv', header=TRUE)

set.seed(1)

test.set = data
training.set = data

y.1.df = training.set[, ncol(training.set)]
x.1.df = training.set[, -ncol(training.set)]
y.2.df = test.set[, ncol(test.set)]
x.2.df = test.set[, -ncol(test.set)]

training.set.matrix = as.matrix(training.set)
y.1.mat = training.set.matrix[, ncol(training.set.matrix)]
x.1.mat = training.set.matrix[, -ncol(training.set.matrix)]
test.set.matrix = as.matrix(test.set)
y.2.mat = test.set.matrix[, ncol(test.set.matrix)]
x.2.mat = test.set.matrix[, -ncol(test.set.matrix)]

 
 
# OLS
ols.model = lm(y.1.df ~ ., data = x.1.df)
summary(ols.model)


# Stepwise Model
initial.model = lm(y.1.df ~ 1, data = x.1.df)
final.model = lm(y.1.df ~ ., data = x.1.df)
stepwise.selection.model = step(object=initial.model, scope=list(upper=final.model), direction='both', k=log(nrow(x.1.df)), trace=0)
summary(stepwise.selection.model)


# PPR
ppr1.model = ppr(data=x.1.df, y.1.df ~ ., nterms=1, optlevel=3) 
ppr1.weights = ppr1.model$alpha
cbind(colnames(ppr1.weights), ppr1.weights)[order(-abs(ppr1.weights)), ]


# Regression Trees
full.tree = rpart(data=training.set, Y~., method="anova", cp=0)
full.cpt = full.tree$cptable

minrow = which.min(full.cpt[, 4])
full.cplow.min = full.cpt[minrow, 1]
full.cpup.min = ifelse(minrow==1, yes=1, no=full.cpt[minrow-1, 1])
full.cp.min = sqrt(full.cplow.min * full.cpup.min)

se.row = min(which(full.cpt[,4] < full.cpt[minrow, 4] + full.cpt[minrow, 5]))
full.cplow.1se = full.cpt[se.row, 1]
full.cpup.1se = ifelse(se.row==1, yes=1, no=full.cpt[se.row-1, 1])
full.cp.1se = sqrt(full.cplow.1se * full.cpup.1se)

full.prune.min = prune(full.tree, cp=full.cp.min)
full.prune.1se = prune(full.tree, cp=full.cp.1se)

full.tree$variable.importance
full.prune.min$variable.importance
full.prune.1se$variable.importance


# MARS
earth.1 = earth(Y ~ ., data=training.set, degree=1, pmethod="backward")
evimp(earth.1)


# Random Forests
rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=18, nodesize=10, keep.forest=TRUE)
round(importance(rf.model), 2)


# BART
bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)],
          k=3, ntree=100, sigdf=3, sigquant=0.95, verbose=FALSE)
apply(bb$varcount, 2, sum)








test.set = data[, -21]
training.set = data[, -21]

training.set.matrix = as.matrix(training.set)
y.1.mat = training.set.matrix[, ncol(training.set.matrix)]
x.1.mat = training.set.matrix[, -ncol(training.set.matrix)]
test.set.matrix = as.matrix(test.set)
y.2.mat = test.set.matrix[, ncol(test.set.matrix)]
x.2.mat = test.set.matrix[, -ncol(test.set.matrix)]

# LASSO
cv.lasso = cv.glmnet(y=y.1.mat, x= x.1.mat, family="gaussian")
plot(cv.lasso)
lambda.min = cv.lasso$lambda.min
fit = glmnet(y=y.1.mat, x= x.1.mat, family="gaussian")
lbs_fun <- function(fit, ...) {
  L <- length(fit$lambda)
  x <- log(fit$lambda[L])
  y <- fit$beta[, L]
  labs <- names(y)
  text(x, y, labels=labs, ...)
}
plot(fit, xvar="lambda")
lbs_fun(fit)

fit = glmnet(y=y.1.mat, x= x.1.mat, family="gaussian", lambda = lambda.min)
round(coef(fit), 2)
 
# BMA
bma.model = bicreg(x = x.1.mat, y = y.1.mat, strict = FALSE, OR = 100)
summary(bma.model)

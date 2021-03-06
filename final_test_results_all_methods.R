library(glmnet)
library(BMA)
library(mgcv)
library(rpart)
library(earth)
library(randomForest)
library(BayesTree)
library(nnet)

data = read.csv('Data2016.csv', header=TRUE)
data = data[, c(4, 12, 18, 19, 22)]
data = data[-c(183, 571, 380), ]

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

training.error = matrix(NA, ncol=15, nrow=10)
test.error = matrix(NA, ncol=17, nrow=10)

for (i in 1:10) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  y.1.df = training.set[, ncol(data)]
  x.1.df = training.set[, -ncol(data)]
  y.2.df = test.set[, ncol(data)]
  x.2.df = test.set[, -ncol(data)]

  training.set.matrix = as.matrix(training.set)
  y.1.mat = training.set.matrix[, ncol(data)]
  x.1.mat = training.set.matrix[, -ncol(data)]
  test.set.matrix = as.matrix(test.set)
  y.2.mat = test.set.matrix[, ncol(data)]
  x.2.mat = test.set.matrix[, -ncol(data)]

  
  ###
  # OLS
  ###
  ols.model = lm(y.1.df ~ ., data = x.1.df)
  ols.predict.1 = predict(ols.model, newdata = x.1.df)
  ols.predict.2 = predict(ols.model, newdata = x.2.df)
  
  training.error[i, 1] = mean((y.1.df - ols.predict.1)^2)
  test.error[i, 1] = mean((y.2.df - ols.predict.2)^2)
  

  ###
  # Stepwise BIC
  ###
  initial.model = lm(y.1.df ~ 1, data = x.1.df)
  final.model = lm(y.1.df ~ ., data = x.1.df)
  stepwise.selection.model = step(object=initial.model, scope=list(upper=final.model), direction='both', k=log(nrow(x.1.df)), trace=0)
  stepwise.predict.1 = predict(stepwise.selection.model, newdata = x.1.df)
  stepwise.predict.2 = predict(stepwise.selection.model, newdata = x.2.df)
  
  training.error[i, 2] = mean((y.1.df - stepwise.predict.1)^2)
  test.error[i, 2] = mean((y.2.df - stepwise.predict.2)^2)
  
  
  
  ###
  # LASSO
  ###
  # I needed to extend the lambda range so that the estimates would never be at the boundary
  cv.lasso = cv.glmnet(y=y.1.mat, x= x.1.mat, family="gaussian", lambda=exp(seq(from=0, to=-10, length=200)))
  plot(cv.lasso)
  
  predict.1.1 = predict(cv.lasso, newx=x.1.mat, s=cv.lasso$lambda.min)
  predict.1.2 = predict(cv.lasso, newx=x.2.mat, s=cv.lasso$lambda.min)
  training.error[i, 3] = mean((y.1.mat - predict.1.1)^2)
  test.error[i, 3] = mean((y.2.mat - predict.1.2)^2)
  
  predict.1.1 = predict(cv.lasso, newx=x.1.mat, s=cv.lasso$lambda.1se)
  predict.1.2 = predict(cv.lasso, newx=x.2.mat, s=cv.lasso$lambda.1se)
  training.error[i, 4] = mean((y.1.mat - predict.1.1)^2)
  test.error[i, 4] = mean((y.2.mat - predict.1.2)^2)
  
  
  
  ###
  # BMA
  ###
  bma.model = bicreg(x = x.1.mat, y = y.1.mat, strict = FALSE, OR = 100)
  
  bma.predict.1 = predict(bma.model, newdata = x.1.df)$mean
  bma.predict.2 = predict(bma.model, newdata = x.2.df)$mean
  
  training.error[i, 5] = mean((y.1.mat - bma.predict.1)^2)
  test.error[i, 5] = mean((y.2.mat - bma.predict.2)^2)
  

  ###
  # PPR
  ###
  ppr1.model = ppr(data=x.1.df, y.1.df ~ ., nterms=1, optlevel=3) 
  ppr2.model = ppr(data=x.1.df, y.1.df ~ ., nterms=2, optlevel=3) 
  ppr3.model = ppr(data=x.1.df, y.1.df ~ ., nterms=3, optlevel=3) 
  
  ppr1.predict.1 = predict(ppr1.model, newdata=x.1.df)
  ppr1.predict.2 = predict(ppr1.model, newdata=x.2.df)
  ppr2.predict.1 = predict(ppr2.model, newdata=x.1.df)
  ppr2.predict.2 = predict(ppr2.model, newdata=x.2.df)
  ppr3.predict.1 = predict(ppr3.model, newdata=x.1.df)
  ppr3.predict.2 = predict(ppr3.model, newdata=x.2.df)
  
  training.error[i, 6] = mean((y.1.df - ppr1.predict.1)^2)
  test.error[i, 6] = mean((y.2.df - ppr1.predict.2)^2)
  training.error[i, 7] = mean((y.1.df - ppr2.predict.1)^2)
  test.error[i, 7] = mean((y.2.df - ppr2.predict.2)^2)
  training.error[i, 8] = mean((y.1.df - ppr3.predict.1)^2)
  test.error[i, 8] = mean((y.2.df - ppr3.predict.2)^2)
  
  
  
  ###
  # Regression Trees
  ###
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
  
  full.prediction.1 = predict(full.tree, newdata=training.set)
  full.prediction.min.1 = predict(full.prune.min, newdata=training.set)
  full.prediction.1se.1 = predict(full.prune.1se, newdata=training.set)
  full.prediction.2 = predict(full.tree, newdata=test.set)
  full.prediction.min.2 = predict(full.prune.min, newdata=test.set)
  full.prediction.1se.2 = predict(full.prune.1se, newdata=test.set)
  
  training.error[i, 9] = mean((training.set$Y - full.prediction.1)^2)
  test.error[i, 9] = mean((test.set$Y - full.prediction.2)^2)
  training.error[i, 10] = mean((training.set$Y - full.prediction.min.1)^2)
  test.error[i, 10] = mean((test.set$Y - full.prediction.min.2)^2)
  training.error[i, 11] = mean((training.set$Y - full.prediction.1se.1)^2)
  test.error[i, 11] = mean((test.set$Y - full.prediction.1se.2)^2)
  
  
  ###
  # MARS
  ###
  earth.1 = earth(Y ~ ., data=training.set, degree=1, pmethod="backward")
  earth.2 = earth(Y ~ ., data=training.set, degree=2, pmethod="backward")
  earth.3 = earth(Y ~ ., data=training.set, degree=3, pmethod="backward")
  
  predict.1.1 = predict(earth.1, newdata=training.set)
  predict.1.2 = predict(earth.1, newdata=test.set)
  predict.2.1 = predict(earth.2, newdata=training.set)
  predict.2.2 = predict(earth.2, newdata=test.set)
  predict.3.1 = predict(earth.3, newdata=training.set)
  predict.3.2 = predict(earth.3, newdata=test.set)
  
  training.error[i, 12] = mean((predict.1.1 - training.set$Y)^2)
  test.error[i, 12] = mean((predict.1.2 - test.set$Y)^2)
  training.error[i, 13] = mean((predict.2.1 - training.set$Y)^2)
  test.error[i, 13] = mean((predict.2.2 - test.set$Y)^2)
  training.error[i, 14] = mean((predict.3.1 - training.set$Y)^2)
  test.error[i, 14] = mean((predict.3.2 - test.set$Y)^2)
  
  
  ###
  # Random Forest
  ###
  rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=2, nodesize=1, keep.forest=TRUE)
  training.error[i, 15] = mean((training.set$Y - predict(rf.model, newdata=training.set))^2)
  test.error[i, 15] = mean((test.set$Y - predict(rf.model, newdata=test.set))^2)
  
  
  ###
  # BART
  ###
  bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)], x.test=test.set[, -ncol(test.set)],
            k=3, ntree=100, sigdf=1, sigquant=0.8, verbose=FALSE)
  test.error[i, 16] = mean((test.set$Y - bb$yhat.test.mean)^2)
  
  
  ###
  # Neural Nets
  ###
  unscaled.x1 = training.set[, -ncol(training.set)]
  unscaled.x2 = test.set[, -ncol(test.set)]
  
  y.1 = training.set[, ncol(training.set)]
  y.2 = test.set[, ncol(test.set)]
  
  rescale <- function(x1,x2){
    for(col in 1:ncol(x1)){
      a <- min(x2[,col])
      b <- max(x2[,col])
      x1[,col] <- (x1[,col]-a)/(b-a)
    }
    x1
  }
  
  x.1 = rescale(unscaled.x1, unscaled.x1)
  x.2 = rescale(unscaled.x2, unscaled.x1)
  
  run.nn <- function(siz, dec){
    MSE.final <- 9e99
    #  check <- MSE.final
    for(i in 1:100){
      nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=siz, decay=dec, maxit=500, trace=FALSE)
      MSE <- nn$value/nrow(x.1)
      if(MSE < MSE.final){ 
        MSE.final <- MSE
        nn.final <- nn
      }
      #    check <- c(check,MSE.final)
    }
    #  check
    nn.final
  }
  
  nn <- run.nn(3, 0.01)
  test.error[i, 17] = mean((y.2 - predict(nn, x.2))^2)
}


#colnames(training.error) = c('ols', 'allsubBIC', 'allsubAIC', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'gam', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'MARS*')
#colnames(test.error) = c('ols', 'allsubBIC', 'allsubAIC', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'gam', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'MARS*')

colnames(training.error) = c('ols', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'RF')
colnames(test.error) = c('ols', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'RF', 'BART', 'Nnet')

#save(test.error, file='final_method_comparison_1.RData')
load('final_method_comparison_1.RData')

remove.index = c(4, 7, 8, 9, 11)
test.error = test.error[, -remove.index]

##
# Root - MSPE Results
##
boxplot(sqrt(test.error), main='Root MSPEs',
        xlab="Variable Selection Method", ylab="Root MSPE")

mspe.means = apply(test.error, 2, mean)

root.test.error.min = apply(X=sqrt(test.error), MARGIN=1, FUN=min)
root.test.error.scaled = sqrt(test.error) / root.test.error.min
boxplot(x=root.test.error.scaled, main="Relative Root MSPEs", 
        xlab="Variable Selection Method", ylab="Root MSPE relative to best", ylim=c(1,1.3))


##
# Root - MSE Results
##
boxplot(sqrt(training.error), main='Root MSEs',
        xlab="Variable Selection Method", ylab="Root MSE")

root.training.error.min = apply(X=sqrt(training.error), MARGIN=1, FUN=min)
root.training.error.scaled = sqrt(training.error) / root.training.error.min
boxplot(x=root.training.error.scaled, main="Relative Root MSEs", 
        xlab="Variable Selection Method", ylab="Root MSE relative to best")




data = read.csv('Data2016.csv', header=TRUE)

include = c(4, 12, 18, 19)
test = c(1, 5, 10, 13, 15, 20)

run.results = matrix(0, nrow = 7, ncol=23)

index = 1
for (num in 0:1) {
  combos = combn(test, num)

  for (ci in 1:ncol(combos)) {
    print(index)
    vars = c(include, combos[, ci], 22)
    subset = data[, vars]
    
    if (1 %in% vars){
      run.results[index, 1] = 1
    }
    if (5 %in% vars){
      run.results[index, 2] = 1
    }
    if (10 %in% vars){
      run.results[index, 3] = 1
    }
    if (13 %in% vars){
      run.results[index, 4] = 1
    }
    if (15 %in% vars){
      run.results[index, 5] = 1
    }
    if (20 %in% vars){
      run.results[index, 6] = 1
    }
    
    
    run.results[index, 7:ncol(run.results)] = all_methods(subset)
    index = index + 1
  }

}


#save(run.results, file='group3_variable_test_results.RData')

run.ranks = cbind(run.results[, 1:6], apply(apply(run.results[, -c(1:6)], 2, order), 2, order))


all_methods = function(data) {
  set.seed(1)
  cv.labels = cut(seq(1,nrow(data)),breaks=5,labels=FALSE)
  cv.labels = sample(cv.labels, length(cv.labels))
  
  training.error = matrix(NA, ncol=15, nrow=5)
  test.error = matrix(NA, ncol=17, nrow=5)
  
  n.var = ncol(data) - 1
  
  for (i in 1:5) {
    print(i)
    
    test.set = data[which(cv.labels == i), ]
    training.set = data[which(cv.labels != i), ]
    
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
    cv.lasso = cv.glmnet(y=y.1.mat, x= x.1.mat, family="gaussian", lambda=exp(seq(from=0, to=-7, length=200)))
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
    rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=round(0.75*n.var), nodesize=10, keep.forest=TRUE)
    training.error[i, 15] = mean((training.set$Y - predict(rf.model, newdata=training.set))^2)
    test.error[i, 15] = mean((test.set$Y - predict(rf.model, newdata=test.set))^2)
    
    
    ###
    # BART
    ###
    bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)], x.test=test.set[, -ncol(test.set)],
              k=3, ntree=100, sigdf=3, sigquant=0.95, verbose=FALSE)
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
    
    nn <- run.nn(1, 0.025)
    test.error[i, 17] = mean((y.2 - predict(nn, x.2))^2)
  }
  
  
  #colnames(training.error) = c('ols', 'allsubBIC', 'allsubAIC', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'gam', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'MARS*')
  #colnames(test.error) = c('ols', 'allsubBIC', 'allsubAIC', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'gam', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'MARS*')
  
  colnames(training.error) = c('ols', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'RF')
  colnames(test.error) = c('ols', 'stepBIC', 'lassoMIN', 'lasso1SE', 'bma', 'ppr-1', 'ppr-2', 'ppr-3', 'Full Tree', 'Min Tree', '1SE Tree', 'MARS 1', 'MARS 2', 'MARS 3', 'RF', 'BART', 'Nnet')
  
  return(apply(test.error, 2, mean))
}
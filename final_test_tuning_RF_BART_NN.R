library(nnet)
library(randomForest)
library(BayesTree)

data = read.csv('Data2016.csv', header=TRUE)
data = data[, c(4, 12, 18, 19, 22)]

# Neural Nets
# Round 1
size = c(1, 3, 5, 10)
decay = c(0.01, 0.05, 0.1, 0.25, 1)

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPR <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)
MSE <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)

nets = 20
for (r in 1:10) {
  print(r)
  
  test.set = data[which(cv.labels == r), ]
  training.set = data[which(cv.labels != r), ]
  
  x.b1 <- training.set[, -ncol(training.set)]
  xr <- rescale(x.b1,x.b1)
  yr <- training.set$Y
  x.b2 <- test.set[, -ncol(test.set)]
  xp <- rescale(x.b2, x.b1)
  yp <- test.set$Y
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in size){
    print(s)
    for(d in decay){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((yp - p.min)^2)
      MSE[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

save(MSPE, file='nnet_final_tuning_1_MSPE.RData')

siz.dec <- paste(MSPR[,1],MSPR[,2])

x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)

best <- apply(X=MSPR[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPR[,-c(1:2)])/best)), use.cols=FALSE, names=siz.dec, ylim=c(1,1.4))


# Round 2
size = c(1, 3, 5, 10)
decay = c(0.0001, 0.001, 0.01, 0.025, 0.05)

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPR <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)
MSE <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)

nets = 20
for (r in 1:10) {
  print(r)
  
  test.set = data[which(cv.labels == r), ]
  training.set = data[which(cv.labels != r), ]
  
  x.b1 <- training.set[, -ncol(training.set)]
  xr <- rescale(x.b1,x.b1)
  yr <- training.set$Y
  x.b2 <- test.set[, -ncol(test.set)]
  xp <- rescale(x.b2, x.b1)
  yp <- test.set$Y
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in size){
    print(s)
    for(d in decay){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((yp - p.min)^2)
      MSE[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

save(MSPE, file='nnet_final_tuning_2_MSPE.RData')

siz.dec <- paste(MSPR[,1],MSPR[,2])

x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec, ylim=c(3.5, 5))

best <- apply(X=MSPR[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPR[,-c(1:2)])/best)), use.cols=FALSE, names=siz.dec, ylim=c(1,1.1))

MSPR.mean = apply(MSPR[,-c(1,2)], 1, mean)
View(cbind(MSPR[,c(1,2)], MSPR.mean, order(order(MSPR.mean))))

# Round 3
size = c(10, 15, 20, 30)
decay = c(0.0005, 0.001, 0.01)

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPR <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)
MSE <- matrix(NA, nrow=length(size)*length(decay), ncol=10+2)

nets = 20
for (r in 1:10) {
  print(r)
  
  test.set = data[which(cv.labels == r), ]
  training.set = data[which(cv.labels != r), ]
  
  x.b1 <- training.set[, -ncol(training.set)]
  xr <- rescale(x.b1,x.b1)
  yr <- training.set$Y
  x.b2 <- test.set[, -ncol(test.set)]
  xp <- rescale(x.b2, x.b1)
  yp <- test.set$Y
  # Set counter for storage of results
  qq <- 1
  # Cycle over all parameter values
  for(s in size){
    print(s)
    for(d in decay){
      MSPR[qq,1:2] <- c(s,d)
      MSE[qq,1:2] <- c(s,d)
      # Run nnet and get MSPE and MSE from run
      MSEmin <- 9e99
      for(i in 1:nets){
        nn <- nnet(y=yr, x=xr, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
        MS <- nn$value/nrow(xr)
        if(MS < MSEmin){ 
          MSEmin <- MS
          p.min <-predict(nn, newdata=xp)
        }
      }
      # Save results in new column of matrix
      MSPR[qq, r+2] <- mean((yp - p.min)^2)
      MSE[qq, r+2] <- MSEmin
      # Increment counter for next row
      qq <- qq + 1
    }
  }
}

#save(MSPR, file='nnet_final_tuning_3_MSPE.RData')

siz.dec <- paste(MSPR[,1],MSPR[,2])

x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)
boxplot.matrix(x=sqrt(MSPR[,-c(1,2)]), use.cols=FALSE, names=siz.dec)

best <- apply(X=MSPR[,-c(1,2)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPR[,-c(1:2)])/best)), use.cols=FALSE, names=siz.dec, ylim=c(1,1.2))




# Random Forests
# Round 1
nodes = c(5, 10, 15, 20)
m = c(1,2,3,4)

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPE = matrix(NA, nrow=length(nodes)*length(m), ncol=12)

for (i in 1:10) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (j in 1:length(nodes)) {
    for (k in 1:length(m)) {
      MSPE[count,1:2] <- c(nodes[j], m[k])
      rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=m[k], nodesize=nodes[j], keep.forest=TRUE)
      MSPE[count, i+2] = mean((test.set$Y - predict(rf.model, newdata=test.set))^2)
      count = count + 1
    }
  }
}

save(MSPE, file='rf_final_tuning_1_MSPE.RData')

(MSPE.mean <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=mean)))
(MSPE.min <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=min)))
(MSPE.max <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=max)))

index <- paste(MSPE[,1],MSPE[,2])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPE[,-c(1:2)]), use.cols=FALSE, names=index, las=2,
               main="Root MSPEs from RF")


# Round 2
nodes = c(1,2,3,4,5,6,7,8,9)
m = c(2,3)

set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPE = matrix(NA, nrow=length(nodes)*length(m), ncol=12)

for (i in 1:10) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (j in 1:length(nodes)) {
    for (k in 1:length(m)) {
      MSPE[count,1:2] <- c(nodes[j], m[k])
      rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=m[k], nodesize=nodes[j], keep.forest=TRUE)
      MSPE[count, i+2] = mean((test.set$Y - predict(rf.model, newdata=test.set))^2)
      count = count + 1
    }
  }
}

#save(MSPE, file='rf_final_tuning_2_MSPE.RData')

(MSPE.mean <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=mean)))
(MSPE.min <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=min)))
(MSPE.max <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=max)))

index <- paste(MSPE[,1],MSPE[,2])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPE[,-c(1:2)]), use.cols=FALSE, names=index, las=2,
               main="Root MSPEs from RF")


# Round 3
nodes = c(1,3,5,9)
m = c(2,3)

set.seed(2)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

MSPE = matrix(NA, nrow=length(nodes)*length(m), ncol=12)

for (i in 1:10) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (j in 1:length(nodes)) {
    for (k in 1:length(m)) {
      MSPE[count,1:2] <- c(nodes[j], m[k])
      rf.model = randomForest(data=training.set, Y~., importance=TRUE, ntree=500, mtry=m[k], nodesize=nodes[j], keep.forest=TRUE)
      MSPE[count, i+2] = mean((test.set$Y - predict(rf.model, newdata=test.set))^2)
      count = count + 1
    }
  }
}

#save(MSPE, file='rf_final_tuning_3_MSPE.RData')

(MSPE.mean <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=mean)))
(MSPE.min <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=min)))
(MSPE.max <- cbind(MSPE[,1:2], apply(X=MSPE[,-c(1:2)], MARGIN=1, FUN=max)))

index <- paste(MSPE[,1],MSPE[,2])
x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPE[,-c(1:2)]), use.cols=FALSE, names=index, las=2,
               main="Root MSPEs from RF")






# BART
# Round 1
set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

k = c(1,2,3)
ntree = c(100,250,500)
sigdf = c(3,6,9)
sigquant = c(.90,.95,.99)

MSPE = matrix(NA, nrow=length(k)*length(ntree)*length(sigdf)*length(sigquant), ncol=15)

for (i in 1:1) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (a in 1:length(k)) {
    print(a)
    for (b in 1:length(ntree)) {
      for (c in 1:length(sigdf)) {
        for (d in 1:length(sigquant)) {
          MSPE[count,1:4] <- c(k[a], ntree[b], sigdf[c], sigquant[d])
          bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)], x.test=test.set[, -ncol(test.set)],
                            k=k[a], ntree=ntree[b], sigdf=sigdf[c], sigquant=sigquant[d], verbose=FALSE)
          MSPE[count, i+4] = mean((test.set$Y - bb$yhat.test.mean)^2)
          count = count + 1
        }
      }
    }
  }
}

MSPE = MSPE[, 1:5]
save(MSPE, file='bart_final_tuning_1_MSPE.RData')

(MSPE.mean <- cbind(MSPE[,1:4], mean(MSPE[,-c(1:4)])))
(MSPE.min <- cbind(MSPE[,1:4], min(MSPE[,-c(1:4)])))
(MSPE.max <- cbind(MSPE[,1:4], max(MSPE[,-c(1:4)])))

x11()
par(mfrow=c(2,2))
boxplot(sqrt(MSPE[,-c(1:4)]/MSPE.min[,5])~MSPE[,1], main="k")
boxplot(sqrt(MSPE[,-c(1:4)]/MSPE.min[,5])~MSPE[,2], main="ntree")
boxplot(sqrt(MSPE[,-c(1:4)]/MSPE.min[,5])~MSPE[,3], main="sigdf")
boxplot(sqrt(MSPE[,-c(1:4)]/MSPE.min[,5])~MSPE[,4], main="sigquant")

MSPEk3 = MSPE[which(MSPE[,1]==3), ]
(MSPEk3.min <- cbind(MSPEk3[,1:4], min(MSPEk3[,-c(1:4)])))
(MSPEk3.mean <- cbind(MSPEk3[,1:4], (MSPEk3[,-c(1:4)])))
x11()
par(mfrow=c(2,2))
boxplot(sqrt(MSPEk3[,-c(1:4)]/MSPEk3.min[,5])~MSPEk3[,2], main="ntree")
boxplot(sqrt(MSPEk3[,-c(1:4)]/MSPEk3.min[,5])~MSPEk3[,3], main="sigdf")
boxplot(sqrt(MSPEk3[,-c(1:4)]/MSPEk3.min[,5])~MSPEk3[,4], main="sigquant")


# Round 2
set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=5,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

k = c(3)
ntree = c(100)
sigdf = c(1,2,3,4,5)
sigquant = c(.80,.85,.90)

MSPE = matrix(NA, nrow=length(k)*length(ntree)*length(sigdf)*length(sigquant), ncol=9)

for (i in 1:5) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (a in 1:length(k)) {
    print(a)
    for (b in 1:length(ntree)) {
      for (c in 1:length(sigdf)) {
        for (d in 1:length(sigquant)) {
          MSPE[count,1:4] <- c(k[a], ntree[b], sigdf[c], sigquant[d])
          bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)], x.test=test.set[, -ncol(test.set)],
                    k=k[a], ntree=ntree[b], sigdf=sigdf[c], sigquant=sigquant[d], verbose=FALSE)
          MSPE[count, i+4] = mean((test.set$Y - bb$yhat.test.mean)^2)
          count = count + 1
        }
      }
    }
  }
}

save(MSPE, file='bart_final_tuning_2_MSPE.RData')

labels = paste(MSPE[,1],MSPE[,2],MSPE[,3],MSPE[,4])

x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPE[,-c(1:4)]), use.cols=FALSE, names=labels)

best <- apply(X=MSPE[,-c(1:4)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPE[,-c(1:4)])/best)), use.cols=FALSE, names=labels, ylim=c(1,1.03))

MSPE.mean = apply(MSPE[,-c(1:4)], 1, mean)
cbind(MSPE[, c(1:4)], MSPE.mean, order(order(MSPE.mean)))



# Round 3
set.seed(1)
cv.labels = cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
cv.labels = sample(cv.labels, length(cv.labels))

k = c(3)
ntree = c(100)
sigdf = c(1,2,3)
sigquant = c(.6, .7, .8)

MSPE = matrix(NA, nrow=length(k)*length(ntree)*length(sigdf)*length(sigquant), ncol=14)

for (i in 6:10) {
  print(i)
  
  test.set = data[which(cv.labels == i), ]
  training.set = data[which(cv.labels != i), ]
  
  count = 1
  for (a in 1:length(k)) {
    print(a)
    for (b in 1:length(ntree)) {
      for (c in 1:length(sigdf)) {
        for (d in 1:length(sigquant)) {
          MSPE[count,1:4] <- c(k[a], ntree[b], sigdf[c], sigquant[d])
          bb = bart(y.train=as.numeric(training.set[, ncol(training.set)]), x.train=training.set[, -ncol(training.set)], x.test=test.set[, -ncol(test.set)],
                    k=k[a], ntree=ntree[b], sigdf=sigdf[c], sigquant=sigquant[d], verbose=FALSE)
          MSPE[count, i+4] = mean((test.set$Y - bb$yhat.test.mean)^2)
          count = count + 1
        }
      }
    }
  }
}

save(MSPE, file='bart_final_tuning_3_MSPE.RData')

labels = paste(MSPE[,1],MSPE[,2],MSPE[,3],MSPE[,4])

x11(pointsize=6)
boxplot.matrix(x=sqrt(MSPE[,-c(1:4)]), use.cols=FALSE, names=labels)

best <- apply(X=MSPE[,-c(1:4)], MARGIN=2, FUN=min)
x11(pointsize=6)
boxplot.matrix(x=sqrt(t(t(MSPE[,-c(1:4)])/best)), use.cols=FALSE, names=labels, ylim=c(1,1.03))

MSPE.mean = apply(MSPE[,-c(1:4)], 1, mean)
cbind(MSPE[, c(1:4)], MSPE.mean, order(order(MSPE.mean)))

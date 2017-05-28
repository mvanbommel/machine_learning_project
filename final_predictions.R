library(randomForest)

data = read.csv('Data2016.csv', header=TRUE)
data = data[, c(4, 12, 18, 19, 22)]
#data = data[-c(183, 571, 380), ]

test.data = read.csv('Data2016test.csv', header = TRUE)
test.data = test.data[, c(4, 12, 18, 19)]

###
# Random Forest
###
set.seed(1)
rf.model = randomForest(data=data, Y~., importance=TRUE, ntree=500, mtry=2, nodesize=1, keep.forest=TRUE)
plot(rf.model, main='Final Model')
predictions = predict(rf.model, newdata=test.data)

write.table(predictions, file='predictions.csv', row.names=FALSE, col.names=FALSE)

par(mfrow = c(1,2))

# Y
summary(data$Y)
summary(predictions)

hist(data$Y, xlim=c(420, 500), main='Training Output')
hist(predictions, xlim=c(420, 500), main='Predicted Test Output')


# X12
summary(data$X12)
summary(test.data$X12)

plot(data$X12, data$Y, main='Training Set')
plot(test.data$X12, predictions, main='Test Set')


# X19
summary(data$X19)
summary(test.data$X19)

plot(data$X19, data$Y, main='Training Set')
plot(test.data$X19, predictions, main='Test Set')


# X4
summary(data$X4)
summary(test.data$X4)

plot(data$X4, data$Y, main='Training Set')
plot(test.data$X4, predictions, main='Test Set')


# X18
summary(data$X18)
summary(test.data$X18)

plot(data$X18, data$Y, main='Training Set')
plot(test.data$X18, predictions, main='Test Set')



o1 = which(data$X18 > 70 & data$Y > 450)
p1 = which(test.data$X18 > 70 & predictions > 450)

o2 = which.min(data$X18)
p2 = which(test.data$X18 < 30)

# X12
summary(data$X12)
summary(test.data$X12)

plot(data$X12, data$Y)
points(data$X12[o1], data$Y[o1], col='blue', pch=19)
points(data$X12[o2], data$Y[o2], col='red', pch=19)
plot(test.data$X12, predictions)
points(test.data$X12[p1], predictions[p1], col='blue', pch=19)
points(test.data$X12[p2], predictions[p2], col='red', pch=19)


# X19
summary(data$X19)
summary(test.data$X19)

plot(data$X19, data$Y)
points(data$X19[o1], data$Y[o1], col='blue', pch=19)
points(data$X19[o2], data$Y[o2], col='red', pch=19)
plot(test.data$X19, predictions)
points(test.data$X19[p1], predictions[p1], col='blue', pch=19)
points(test.data$X19[p2], predictions[p2], col='red', pch=19)


# X4
summary(data$X4)
summary(test.data$X4)

plot(data$X4, data$Y)
points(data$X4[o1], data$Y[o1], col='blue', pch=19)
points(data$X4[o2], data$Y[o2], col='red', pch=19)
plot(test.data$X4, predictions)
points(test.data$X4[p1], predictions[p1], col='blue', pch=19)
points(test.data$X4[p2], predictions[p2], col='red', pch=19)


# X18
summary(data$X18)
summary(test.data$X18)

plot(data$X18, data$Y, main='Training Set')
#points(data$X18[o1], data$Y[o1], col='blue', pch=19)
#points(data$X18[o2], data$Y[o2], col='red', pch=19)
plot(test.data$X18, predictions, main='Test Set')
points(test.data$X18[p1], predictions[p1], col='blue', pch=19)
points(test.data$X18[p2], predictions[p2], col='red', pch=19)




plot(data$X12, data$X19)
points(data$X12[o1], data$X19[o1], col='blue', pch=19)
points(data$X12[o2], data$X19[o2], col='red', pch=19)
plot(test.data$X12, test.data$X19)
points(test.data$X12[p1], test.data$X19[p1], col='blue', pch=19)
points(test.data$X12[p2], test.data$X19[p2], col='red', pch=19)

plot(data$X18, data$X4)
points(data$X18[o1], data$X4[o1], col='blue', pch=19)
points(data$X18[o2], data$X4[o2], col='red', pch=19)
plot(test.data$X18, test.data$X4)
points(test.data$X18[p1], test.data$X4[p1], col='blue', pch=19)
points(test.data$X18[p2], test.data$X4[p2], col='red', pch=19)



library(rgl)
plot3d(data$X12, data$X19, data$Y, xlim=c(min(data$X12, test.data$X12)), ylim=c(min(data$X19, test.data$X19)), zlim=c(min(data$Y, predictions)))
points3d(test.data$X12, test.data$X19, predictions, col='blue')

plot3d(data$X18, data$X4, data$Y, xlim=c(min(data$X18, test.data$X18)), ylim=c(min(data$X4, test.data$X4)), zlim=c(min(data$Y, predictions)))
points3d(test.data$X18, test.data$X4, predictions, col='blue')


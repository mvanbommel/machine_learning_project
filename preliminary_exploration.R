data = read.csv('Data2016.csv', header=TRUE)
test.data = read.csv('Data2016test.csv', header = TRUE)

# Check for missing data
any(is.na(data))
any(is.null(data))

any(is.na(test.data))
any(is.null(test.data))


# Examine the distribution of the inputs, and compare the distributions of
# the training and test set
for (col.id in 1:20){
  x11()
  par(mfrow=c(2, 1))
  
  data.col = data[, col.id]
  test.data.col = test.data[, col.id]
  hist(data.col, xlim=c(min(data.col, test.data.col), max(data.col, test.data.col)), main=paste0(col.id), xlab='Value')
  hist(test.data.col, xlim=c(min(data.col, test.data.col), max(data.col, test.data.col)), main=paste0(col.id), xlab='Value')
}

# X21 is categorical, so examine the counts of each category
col.id = 21
x11()
par(mfrow=c(2, 1))
data.col = data[, col.id]
test.data.col = test.data[, col.id]
barplot(table(data.col), main=paste0(col.id))
barplot(table(test.data.col), main=paste0(col.id))


# From the histograms, 3 variables are discrete and 3 are highly right skewed
discrete.variables = c(7, 17, 21)
right.skewed = c(5, 13, 14)
x11()
boxplot(test.data[, right.skewed])
boxplot(log(test.data[, right.skewed]))


# Examine the linear correlations
correlations = round(cor(data[, -21]), 2)
correlations

y.correlations = correlations[, 21]
y.correlations = y.correlations[order(-abs(as.numeric(y.correlations)))]
y.correlations

# Examine X21 vs Y since it is categorical
boxplot(Y~X21, data=data, main="Correlation of Y with X21", ylab='Y', xlab='X21')


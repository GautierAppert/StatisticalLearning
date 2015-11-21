#####################################################
#                STOCK MARKET RETURNS               #
#         Statistical Learning - Homework 2         #
#         ENSAE ParisTech - 3A Data Science         #
#         Gautier APPERT & Guillaume SALHA          #
#                  Nov 16th, 2015                    #
#####################################################





##### k-NN and Cross-Validation #####





# Read Data

library(class)
data(iris)
head(iris)
summary(iris)





# Training set and test set

train <- iris[c(1:30,51:80,101:130),1:5]
test <- iris[c(31:50,81:100,131:150),1:5]





# k Nearest Neighbors

pred <- knn(train[,1:4], test[,1:4], train[,5], k = 3)

# Display the confusion matrix

table(pred,test[,5])





# 5-fold cross-validation to select k
# from the set {1,...,10}

fold <- sample(rep(1:5,each=18)) # Create groups
cvpred <- matrix(NA,nrow=90,ncol=10) # Initialize matrix
for (k in 1:10){
    for (v in 1:5){
        sample1 <- train[which(fold!=v),1:4] # Train
        sample2 <- train[which(fold==v),1:4] # Test
        class1 <- train[which(fold!=v),5] # Truth
        cvpred[which(fold==v),k] <- knn(sample1,sample2,class1,k=k) # kNN
    }
}

class <- as.numeric(train[,5])

# Display misclassification rates for k=1:10

apply(cvpred,2,function(x) sum(class!=x))





# Monte-Carlo 5-fold cross-validation

monteCarlo <- function(n){
  result <- matrix(0,n,10)
  class <- as.numeric(train[,5])
  for(i in 1:n){
    fold <- sample(rep(1:5,each=18)) # Create groups
    cvpred <- matrix(NA,nrow=90,ncol=10) # Initialize matrix
    for (k in 1:10){
      for (v in 1:5){
        sample1 <- train[which(fold!=v),1:4] # Train
        sample2 <- train[which(fold==v),1:4] # Test
        class1 <- train[which(fold!=v),5] # Truth
        cvpred[which(fold==v),k] <- knn(sample1,sample2,class1,k=k) # kNN
      }
    }
    result[i,] <- apply(cvpred,2,function(x) sum(class!=x))
  }
  colMeans(result)
}

# Application

monteCarlo(100)



##### Predicting stock market returns #####





# Read Data

library(DMwR)
library(quantmod)
data(GSPC)




# Implementation of the Ti indicator of the exercise

T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10){
  v <- apply(HLC(quotes), 1, mean)
  r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
  for (x in 1:n.days) r[, x] = Next(Delt(v, k = x), x)
  x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x <
  -tgt.margin]))
  if (is.xts(quotes))
  xts(x, time(quotes))
  else x
}





# Candlestick graphs of stock quotes

candleChart(last(GSPC, "3 months"), theme = "white", TA = NULL)

avgPrice <- function(p) apply(HLC(p), 1, mean)

addAvgPrice <- newTA(FUN = avgPrice, col = 1, legend = "AvgPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")
get.current.chob<-function(){quantmod:::get.current.chob()}
candleChart(last(GSPC, "3 months"), theme = "white", TA = "addAvgPrice(on=1)")
candleChart(last(GSPC, "3 months"), theme = "white", TA = "addT.ind();addAvgPrice(on=1)")





# Candlestick graphs with median

medianPrice <- function(p) apply(HLC(p), 1, median)

addMedianPrice <- newTA(FUN = medianPrice, col = 1, legend = "MedianPrice")
addT.ind <- newTA(FUN = T.ind, col = "red", legend = "tgtRet")

candleChart(last(GSPC, "3 months"), theme = "white", TA = "addT.ind();addMedianPrice(on=1)")





# Some financial technical indicators

library(TTR)
myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myAroon <- function(x) aroon(x[, c("High", "Low")])$oscillator
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myChaikinVol <- function(x) Delt(chaikinVolatility(x[, c("High","Low")]))[, 1]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myEMV <- function(x) EMV(x[, c("High", "Low")], x[, "Volume"])[,2]
myMACD <- function(x) MACD(Cl(x))[, 2]
myMFI <- function(x) MFI(x[, c("High", "Low", "Close")],x[, "Volume"])
mySAR <- function(x) SAR(x[, c("High", "Close")])[, 1]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,1]





# Random Forest

data(GSPC)
library(randomForest)

data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

set.seed(1234)

rf <- buildModel(data.model,method="randomForest",
training.per = c(start(GSPC),index(GSPC["1999-12-31"])),
ntree=50, importance=T)





# Importance of variables

varImpPlot(rf@fitted.model, type = 1, pch = 19, lcolor='blue')

importance(rf@fitted.model)





# Random Forest with the 8 most important variables

data.model <- specifyModel(T.ind(GSPC) ~ 
myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myCLV(GSPC) +
myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC))

set.seed(1234)

rf <- buildModel(data.model,method="randomForest",
training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
ntree=50, importance=T)

varImpPlot(rf@fitted.model, type = 1, pch = 19, lcolor='blue')





# Save training dataset

Tdata.train <- as.data.frame(modelData(data.model,
               data.window=c("1970-01-02","1999-12-31")))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
              data.window=c("2000-01-01","2009-09-15"))))





# Create signal variable

Tdata.train[,1]  = trading.signals(Tdata.train[,1],0.1,-0.1)
names(Tdata.train)[1] = "signal"
summary(Tdata.train)





# kNN algorithm to predict signal on test set

# First, we determine the optimal k by
# Monte-Carlo 5-fold cross-validation

monteCarloSignal <- function(n){
  class <- as.numeric(Tdata.train[,1])
  result <- matrix(0,n,20)
  for(i in 1:n){
    fold <- c(sample(rep(1:5,each=(nrow(Tdata.train)/5))),sample(1:5,2)) # Create groups
    cvpred <- matrix(NA,nrow=nrow(Tdata.train),ncol=20) # Initialize matrix
    for (k in 1:20){
      for (v in 1:5){
        sample1 <- Tdata.train[which(fold!=v),2:9] # Train
        sample2 <- Tdata.train[which(fold==v),2:9] # Test
        class1 <- Tdata.train[which(fold!=v),1] # Truth
        cvpred[which(fold==v),k] <- knn(sample1,sample2,class1,k=k) # kNN
      }
    }
    result[i,] <- apply(cvpred,2,function(x) sum(class!=x))
  }
  colMeans(result)
}

# Application

monteCarloSignal(1) # We choose k=1

# k Nearest Neighbors

pred <- knn(Tdata.train[,2:9],Tdata.eval[,2:9], Tdata.train[,1], k = 1)

# Display the confusion matrix
Tdata.eval[,1] = trading.signals(Tdata.eval[,1],0.1,-0.1)
names(Tdata.eval)[1] = "signal"
table(pred,Tdata.eval[,1])

# Errors
1 - sum(diag(table(pred,Tdata.eval[,1])))/nrow(Tdata.eval)





# Comparison with Decision Tree

# Train the model

library(rpart)
treeSignal <- rpart(signal ~ ., data = Tdata.train, 
                    control=rpart.control(cp=0.008))
treeSignal

# Plot the tree
par(lwd=2, col="red")
plot(treeSignal, compress=TRUE)
text(treeSignal, use.n=TRUE,col="blue")

# Another way:
par(lwd=2, bg="white")
prettyTree(treeSignal,col="blue",bg="lightblue")

# Predictions

predTree <- predict(treeSignal,Tdata.eval)
rowmax <- apply(predTree,1,max)
predTree1 <- ifelse(predTree[,1]==rowmax,'s',
                    ifelse(predTree[,2]==rowmax,'h','b'))

# Errors
mean(predTree1!=Tdata.eval[,1])
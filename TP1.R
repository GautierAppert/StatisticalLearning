#####################################################
#              ALGAE BLOOM PREDICTIONS              #
#         Statistical Learning - Homework 1         #
#         ENSAE ParisTech - 3A Data Science         #
#         Gautier APPERT & Guillaume SALHA          #
#                  Nov 1st, 2015                    #
#####################################################





##### DATA VISUALIZATION


# Load data
library(DMwR)
head(algae)


# Summary of data
summary(algae)


# pH value histogram and kernel density estimation 
library(car)
op <- par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab="",
main = "Histogram of maximum pH value",ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))


# pH value QQ-Plot
qqnorm(algae$mxPH,main="Normal QQ plot of maximum pH")
par(op)


# Conditional boxplot A1/Size
library(lattice)
bwplot(size ~ a1, data=algae, ylab="River Size",xlab="Algal A1")





##### DEALING WITH MISSING DATA


# Number of observations with missing data : 16
nrow(algae[!complete.cases(algae),])


# Visualization 
library(Amelia)
missmap(algae, main="Détection des valeurs manquantes dans la base de données 'Train'", col=c("blue", "black"), legend=FALSE)


# Méthod 1: remove observations
algae1 <- na.omit(algae)
nrow(algae)
nrow(algae1)


# Method 2: use most frequent values
algae2 <- centralImputation(algae)
nrow(algae[!complete.cases(algae),])
nrow(algae2[!complete.cases(algae2),])


# Method 3: kNN approach
algae3 <- knnImputation(algae, k =10, meth = "median")
nrow(algae[!complete.cases(algae),])
nrow(algae3[!complete.cases(algae3),])





##### MODELS


# kNN approach to complete missing values and linear model on all variables
algae <- knnImputation(algae, k =10, meth = "median")
lm.a1 <- lm(a1 ~ ., data = algae[, 1:12])
summary(lm.a1)


# ANOVA
anova(lm.a1)


# Backward variable selection and final linear model 
final.lm <- step(lm.a1)
summary(final.lm)


# Decision tree
algae <- knnImputation(algae, k =10, meth = "median")
library(rpart)
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])
rt.a1


# Plot the tree
par(lwd=2, col="red")
plot(rt.a1, compress=TRUE)
text(rt.a1, use.n=TRUE,col="blue")


# Another way:
par(lwd=2, bg="lemonchiffon3")
prettyTree(rt.a1,col="navy",bg="lemonchiffon")


# Evaluation of predictions on training set
lm.predictions.a1 <- predict(final.lm, algae)
rt.predictions.a1 <- predict(rt.a1, algae)
regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[,"a1"])
regr.eval(algae[, "a1"], lm.predictions.a1, train.y = algae[,"a1"])


# Plot errors: predicted values / true values
par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a1, algae[, "a1"], main = "Linear Model",
xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae[, "a1"], main = "Regression Tree",
xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)


# Predictions on test set
summary(test.algae)
test.algae <- knnImputation(test.algae, k =10, meth = "median")
lm.predictions.a1 <- predict(final.lm,test.algae) 
rt.predictions.a1 <- predict(rt.a1,test.algae) 


# Evaluation of predictions on test set, using true A1 values from algae.sols
true.a1 <- algae.sols[,1]
regr.eval(true.a1, rt.predictions.a1)
regr.eval(true.a1, lm.predictions.a1)


# Repeat previous steps from A2,...,A7 variables
finalSummary <- matrix(0,14,3)
rownames(finalSummary) <- paste(c("Reg","Tree"),paste("A",rep(1:7,each=2),sep=''))
i <- 1
for(var in 1:7){
    # Linear Model
    linMod <- lm(algae[,11+var] ~ ., data=algae[, 1:11])
    finalLinMod <- step(linMod)
    # Decision Tree
    decTree <- rpart(algae[,11+var] ~ ., data = algae[, 1:11])
    # Predictions
    linPred <- predict(finalLinMod,test.algae) 
    treePred <- predict(decTree,test.algae) 
    # Comparison
    finalSummary[i,] <- regr.eval(algae.sols[,var], linPred)[1:3]
    finalSummary[i+1,] <- regr.eval(algae.sols[,var], treePred)[1:3]
    i <- i+2
}
finalSummary
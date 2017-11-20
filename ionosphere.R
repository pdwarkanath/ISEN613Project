library(class)
library(MASS)
library(e1071)
library(tree)
library(randomForest)

# Read Data

setwd("H:/ISEN613Project-master")
df <- read.csv("ionosphere.csv", header = FALSE)
colnames(df)[35] <- "SignalType"

# Find missing values

MissingVals <- apply(df, 2, is.na)
which(MissingVals==TRUE)

# PCA

xdf <- df[,1:34]

# second predictor is a constant 0 column. Cannot calculate variance. Hence removed

xdf <- xdf[,-2]
prOut <- prcomp(xdf, scale = TRUE)



# Scree plot

prvar <- prOut$sdev^2

pve <- prvar/sum(prvar)

plot(pve, type = "b", ylab = "Proportion of Variance Explained", xlab = "Principal Components", main = "Scree Plot")
plot(cumsum(pve), type = "b", ylab = "Proportion of Variance Explained", xlab = "Principal Components", main = "Cumulative of PVE")

z <- prOut$x[,1:5]
k = 5

cvAll <- function(cvCount) {
  
  # Creating training and test data sets
  
  testFactor <- as.integer(nrow(z)/k)
  if(cvCount==k){
    testSample <- ((cvCount-1)*testFactor+1):nrow(z)
    } else {
  testSample <- ((cvCount-1)*testFactor+1):((cvCount)*testFactor)
  }
  
  ztrain <- z[-testSample,]
  ztest <- z[testSample,]
  signalTrain <- df$SignalType[-testSample]
  signalTest <- as.character(df$SignalType[testSample])
  
  # KNN
  
  knnPreds <- as.character(knn(ztrain,ztest,signalTrain,k=1))
  predCheck <- cbind(knnPreds,signalTest)
  colnames(predCheck)[1] <- "knn"
  knnAccuracy <- mean(predCheck[,"knn"] == predCheck[,"signalTest"])
  
  # Cleaning data for other methods
  
  ztrain <- data.frame(ztrain,signalTrain)
  ztest <- as.data.frame(ztest)
  
  # Logistic Regression
  
  logAccuracy <- rep(0,20)
  threshold <- rep(0,20)
  logPredCheck <- as.character(signalTest)
  
  for(i in 1:20){
    threshold[i] <- 0.5 +i*0.02
    logFit <- glm(signalTrain~.,data = ztrain, family = "binomial")
    logProbs <- predict(logFit,ztest,type = "response")
    logPreds <- rep("b",nrow(ztest))
    logPreds[logProbs>threshold[i]] = "g"
    logPredCheck <- cbind(logPredCheck, logPreds)
    colnames(logPredCheck)[1] <- "signalTest"
    colnames(logPredCheck)[length(colnames(logPredCheck))] <- paste0("Logistic",i)
    logAccuracy[i] <- mean(logPredCheck[,paste0("Logistic",i)] == logPredCheck[,"signalTest"])
    
  }
  plot(threshold, logAccuracy, type = 'b')
  predCheck <- cbind(as.character(logPredCheck[,paste0("Logistic",which.max(logAccuracy))]),predCheck)
  colnames(predCheck)[1] <- "Logistic"
  logAccuracy <- logAccuracy[which.max(logAccuracy)]
  
  
  # LDA
  
  
  ldaFit <- lda(signalTrain~.,data=ztrain)
  ldaPreds <- predict(ldaFit,ztest)
  
  
  predCheck <- cbind(as.character(ldaPreds$class),predCheck)
  colnames(predCheck)[1] <- "lda"
  ldaAccuracy <- mean(predCheck[,"lda"] == predCheck[,"signalTest"])
  
  # QDA
  
  
  qdaFit <- qda(signalTrain~.,data=ztrain)
  ztest <- as.data.frame(ztest)
  qdaPreds <- predict(qdaFit,ztest)
  predCheck <- cbind(as.character(qdaPreds$class),predCheck)
  colnames(predCheck)[1] <- "qda"
  qdaAccuracy <- mean(predCheck[,"qda"] == predCheck[,"signalTest"])
  
  # Tree
  
  
  
  # Random Forest
  
  p <- ncol(z)
  set.seed(1)
  bagMod <- randomForest(signalTrain~.,data=ztrain,mtry=p,ntree=100,importance=TRUE)
  bagPreds <- predict(bagMod, ztest)
  predCheck <- cbind(as.character(bagPreds),predCheck)
  colnames(predCheck)[1] <- "Bagging"
  bagAccuracy <- mean(predCheck[,"Bagging"] == predCheck[,"signalTest"])
  
  set.seed(1)
  rfMod <- randomForest(signalTrain~.,data=ztrain,mtry=2,ntree=100,importance=TRUE)
  rfPreds <- predict(rfMod, ztest)
  predCheck <- cbind(as.character(rfPreds),predCheck)
  colnames(predCheck)[1] <- "Random Forest"
  rfAccuracy <- mean(predCheck[,"Random Forest"] == predCheck[,"signalTest"])
  
  
  # SVM
  
  
  i <- -3:2
  costs <- 10^i
  gammas <- seq(0.5,5,by = 0.5)
  degrees <- i[5:6]
  
  set.seed(1)
  tuneOutSVC = tune(svm,signalTrain~.,data=ztrain, kernel="linear",ranges=list(cost=costs))
  bestModSVC <- tuneOutSVC$best.model
  svcPreds <- predict(bestModSVC, ztest)
  predCheck <- cbind(as.character(svcPreds),predCheck)
  colnames(predCheck)[1] <- "SVC"
  svcAccuracy <- mean(predCheck[,"SVC"] == predCheck[,"signalTest"])
  
  set.seed(1)
  tuneOutRadial = tune(svm,signalTrain~.,data=ztrain, kernel="radial",ranges=list(cost=costs,gamma=gammas))
  bestModRadial <- tuneOutRadial$best.model
  svmRadialPreds <- predict(bestModRadial, ztest)
  predCheck <- cbind(as.character(svmRadialPreds),predCheck)
  colnames(predCheck)[1] <- "SVM Radial"
  svmRadialAccuracy <- mean(predCheck[,"SVM Radial"] == predCheck[,"signalTest"])
  
  set.seed(1)
  tuneOutPoly = tune(svm,signalTrain~.,data=ztrain, kernel="polynomial",ranges=list(cost=costs,degree=degrees))
  bestModPoly <- tuneOutPoly$best.model
  svmPolyPreds <- predict(bestModPoly, ztest)
  predCheck <- cbind(as.character(svmPolyPreds),predCheck)
  colnames(predCheck)[1] <- "SVM Poly"
  svmPolyAccuracy <- mean(predCheck[,"SVM Poly"] == predCheck[,"signalTest"])
  
  
  
  accuracyTable <- c(svmPolyAccuracy, svmRadialAccuracy, svcAccuracy, rfAccuracy, bagAccuracy, qdaAccuracy, ldaAccuracy, logAccuracy, knnAccuracy)
  errorTable <- 1 - accuracyTable
  names(errorTable) <- colnames(predCheck)[-length(colnames(predCheck))]
  
  return(errorTable)
}

cvCount <- 1:k
cvErrors <- lapply(cvCount, cvAll)
cvErrorTable <- data.frame()

for(j in 1:k){
  cvErrorsUnlisted <- unlist(cvErrors[j])
  cvErrorTable <- rbind(cvErrorTable,cvErrorsUnlisted)
}

names(cvErrorTable) <- names(unlist(cvErrors[1]))
meancverrors <- apply(cvErrorTable,2, mean)

library(class)
library(MASS)

setwd("C:/Users/pdwarkanath/Downloads/613/Geographical Original of Music/Geographical Original of Music")
rawData <- read.csv("default_features_1059_tracks.csv", header = FALSE)

countryLookup <- read.csv("coords2country.csv", header = TRUE)

colnames(rawData)[69] ="Lat"
colnames(rawData)[70] ="Long"
rawData <- merge(rawData,countryLookup, by = c("Lat","Long"))

xdf <- rawData[,3:70]

prOut <- prcomp(xdf, scale = TRUE)

# Scree plot

prvar <- prOut$sdev^2

pve <- prvar/sum(prvar)

plot(pve, type = 'b', ylab = "Proportion of Variance Explained", xlab = "Principal Components", main = "Scree Plot")
plot(cumsum(pve), type = 'b', ylab = "Proportion of Variance Explained", xlab = "Principal Components", main = "Cumulative of PVE")


# 18 components explain 80% of variance

z <- prOut$x[,1:18]


# KNN

set.seed(1)
train <- sample(nrow(z), as.integer(0.8*nrow(z)))
ztrain <- z[train,]
ztest <- z[-train,]

countryTrain <- rawData$Country[train]
countryTest <- as.character(rawData$Country[!train])


knnPreds <- as.character(knn(ztrain,ztest,countryTrain,k=5))

predCheck <- cbind(knnPreds,countryTest)

colnames(predCheck)[1] <- "knn"

knnAccuracy <- mean(predCheck[,"knn"] == predCheck[,"countryTest"])



# LDA

ztrain <- data.frame(ztrain,countryTrain)

ldaFit <- lda(countryTrain~.,data=ztrain)
ztest <- as.data.frame(ztest)
ldaPreds <- predict(ldaFit,ztest)

predCheck <- cbind(as.character(ldaPreds$class),predCheck)

colnames(predCheck)[1] <- "lda"

ldaAccuracy <- mean(predCheck[,"lda"] == predCheck[,"countryTest"])

#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
#install.packages("ggplot2")
library(ggplot2)   #probably redundant
#install.packages("labeling")
library(labeling)  #not used
#install.packages("corrplot")
library(corrplot)

# read in data
setwd("~/xtol/c3t2/parul/Task 2")
Galaxy<- read.csv(paste("./GalaxyLargeMatrix.csv", sep=""), header = TRUE)

#Basic Commands
#No. of rows
nrow(Galaxy) #12877
#No. of columns
ncol(Galaxy) #60
# Range
Range<-range(Galaxy$galaxySentiment)  #-140 997

#will return the positions of the variables that are flagged to be problematic
nzv<-nearZeroVar(Galaxy)
filteredDescr <-Galaxy[,-nzv]
dim(filteredDescr) #12877 10
str(filteredDescr)
filteredDescr$id<-NULL

#Create object containing correlation results
descrCor <-  cor(filteredDescr)

#Summary 
summary(descrCor[upper.tri(descrCor)])
corrplot(descrCor)

####### mydata attributes = 20 
# As I am working with Galaxy Sentiment, I will use all the samsung variables
df_Galaxy=Galaxy[,grepl("^samsung*",names(Galaxy))]
#removing samsunggalaxy because this factor also appears in the (to be) merged nzv matrix
df_Galaxy$samsunggalaxy<-NULL

# I added the variables I got in descrCor to the above df to create 
#the list of variables that I feel are meaningful for this excercise
Galaxydata<- cbind(filteredDescr,df_Galaxy)
# add back in sentiment (missed grepl statement)
Galaxydata <- cbind(Galaxydata,Galaxy$galaxySentiment)
names(Galaxydata)[ncol(Galaxydata)] <- 
      names(Galaxy)[ncol(Galaxy)]

  names(Galaxydata)
  ncol(Galaxydata)

  #####Plots and Graphs
  ggplot(Galaxydata,
                aes(
                             y=galaxySentiment,
                                      x=samsunggalaxy
                                    )
                ) + geom_point() + stat_smooth(method ="lm", se = F)

  # greg's stuff

  # create a df of sentiment only (for histogram)
  sentimentFactor <- data.frame(as.numeric(Galaxydata[,ncol(Galaxydata)]))
  # remove zeroes
  sentimentFactorNonZero <- subset(data.frame(sentimentFactor), sentimentFactor != 0)

  # remove outliers (> +/- 3 stdevs)
  sentimentFactorNonZero <- (
                               subset(
                                          data.frame(sentimentFactorNonZero),
                                              sentimentFactorNonZero < 3*sd(sentimentFactorNonZero[,1]) &
                                                        sentimentFactorNonZero > -3*sd(sentimentFactorNonZero[,1])
                                                      )
                               )

  # reset the df index, needed for charting
  sentimentFactorNonZero <- data.frame(sentimentFactorNonZero[,1])

  ggplot(sentimentFactorNonZero,
                aes(
                             x = data.frame(sentimentFactorNonZero)
                                    )
                ) + geom_histogram(binwidth = 10)


  # parul's stuff

  names(data.frame(sentimentFactor))
  ggplot(Galaxydata,
                aes(x=iphone,
                             y=samsunggalaxy)) + 
  geom_point() + stat_smooth(method ="lm", se = F)




#ggplot(Galaxydata, aes(galaxySentiment, samsunggalaxy)) +
#  geom_jitter() 

ggplot(Galaxydata, aes(samsungdispos,samsungdisneg)) +
      geom_point(size=1) + geom_smooth() +
        ggtitle("Samsung Display")

    ggplot(Galaxydata, aes(samsungcampos,samsungcamneg)) +
          geom_point(size=1) + geom_smooth() +
            ggtitle("Samsung Camera")

        ggplot(Galaxydata, aes(samsungperpos,samsungperneg)) +
              geom_point(size=1) + geom_smooth() +
                ggtitle("Samsung Performance")


                  

            dev.off()

            ggplot(Galaxydata, aes(x=samsungdispos, y=samsungdisneg, 
                                                      color=galaxySentiment)) + 
  geom_point() + 
    geom_smooth()
ggplot(Galaxydata, aes(x=samsunggalaxy, y=iphone, 
                                              color=galaxySentiment)) + 
  geom_point() + 
    geom_smooth()
ggplot(iphonedata, aes(x=samsunggalaxy, y=iphone, 
                                              color=iphoneSentiment)) + 
  geom_point() +
    geom_smooth()


ggplot(Galaxydata, aes(x=samsunggalaxy, y=iphone, 
                                              color=galaxySentiment)) + 
  geom_smooth()

ggplot(iphonedata, aes(x=samsunggalaxy, y=iphone, 
                                              color=iphoneSentiment)) + 
  geom_smooth()

dev.off()

##################################

install.packages("arules")
library(arules)

##Discretize Samsung Galaxy Sentiment and create a vector containing discretized levels
disfixed7 <- discretize(Galaxydata$galaxySentiment, "fixed", categories= c(-Inf, -40, -10, -1, 1, 10, 40, Inf))
summary(disfixed7)
str(disfixed7)

#Insert the vector into the dataset
Galaxydata$galaxySentiment <- disfixed7

set.seed(170)
##Create Data Partition
ptr=0.7
inTrain <- createDataPartition(y = Galaxydata$galaxySentiment, p=ptr, list=FALSE)

Galaxy_training <- Galaxydata[ inTrain,]
Galaxy_testing <- Galaxydata[-inTrain,]
nrow(Galaxy_training)
nrow(Galaxy_testing)

##trainControl
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 1)

######## KNN ############

##train                   
GalaxyknnFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "knn",
                                            trControl=ctrl, preProcess = "scale")
GalaxyknnFit

##predict
GalaxyknnPredict <- predict(GalaxyknnFit,Galaxy_testing)
table(GalaxyknnPredict)

##Confusion Matrix
confusionMatrix(GalaxyknnPredict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxyknnPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

######### Random Forest ##########
install.packages("randomForest")
library(randomForest)

##Random Forest Model 10 fold cross validation
rfFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "rf",
                              tuneLength = 10, trControl=ctrl, preProcess = c("center", "scale"))

rfFit

##predict
GalaxyrfPredict <- predict(rfFit, newdata = Galaxy_testing)
table(GalaxyrfPredict)

##Confusion Matrix
confusionMatrix(GalaxyrfPredict, Galaxy_testing$galaxySentiment)

##Resample
print(postResample(pred=GalaxyrfPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

###### SVM ############
#install.packages("e1071")
library("e1071")

GalaxysvmFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "svmRadial",
                                      tuneLength = 10, trControl=ctrl, preProcess = c("center", "scale"))
GalaxysvmFit
grid <- expand.grid(sigma = c(.1,.2,.3),
                                        C = c(1,2,4))

GalaxysvmFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "svmRadial",
                                      tuneGrid=grid, trControl=ctrl, preProcess = c("center", "scale"))
GalaxysvmFit

##predict
GalaxysvmPredict <- predict(GalaxysvmFit, newdata = Galaxy_testing)
table(GalaxysvmPredict)

##Confusion Matrix
confusionMatrix(GalaxysvmPredict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxysvmPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

######## C5.0 ########
install.packages("C50")
library(C50)
library(mlbench)

GalaxyC50Fit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "C5.0",
                                      tuneLength = 10, trControl=ctrl)
GalaxyC50Fit
GalaxyC50grid <- expand.grid( .winnow = FALSE, .trials=c(10,20,30), .model=c("tree"))
GalaxyC50Fit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "C5.0",
                                      tuneGrid=GalaxyC50grid, trControl=ctrl)

##predict
GalaxyC50Predict <- predict(GalaxyC50Fit, Galaxy_testing)
table(GalaxyC50Predict)

##Confusion Matrix
confusionMatrix(GalaxyC50Predict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxyC50Predict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret)
#install.packages("ggplot2")
library(ggplot2)   #probably redundant
#install.packages("labeling")
library(labeling)  #not used
#install.packages("corrplot")
library(corrplot)

# read in data
setwd("~/xtol/c3t2/parul/Task 2")
Galaxy<- read.csv(paste("./GalaxyLargeMatrix.csv", sep=""), header = TRUE)

#Basic Commands
#No. of rows
nrow(Galaxy) #12877
#No. of columns
ncol(Galaxy) #60
# Range
Range<-range(Galaxy$galaxySentiment)  #-140 997

#will return the positions of the variables that are flagged to be problematic
nzv<-nearZeroVar(Galaxy)
filteredDescr <-Galaxy[,-nzv]
dim(filteredDescr) #12877 10
str(filteredDescr)
filteredDescr$id<-NULL

#Create object containing correlation results
descrCor <-  cor(filteredDescr)

#Summary 
summary(descrCor[upper.tri(descrCor)])
corrplot(descrCor)

####### mydata attributes = 20 
# As I am working with Galaxy Sentiment, I will use all the samsung variables
df_Galaxy=Galaxy[,grepl("^samsung*",names(Galaxy))]
#removing samsunggalaxy because this factor also appears in the (to be) merged nzv matrix
df_Galaxy$samsunggalaxy<-NULL

# I added the variables I got in descrCor to the above df to create 
#the list of variables that I feel are meaningful for this excercise
Galaxydata<- cbind(filteredDescr,df_Galaxy)
# add back in sentiment (missed grepl statement)
Galaxydata <- cbind(Galaxydata,Galaxy$galaxySentiment)
names(Galaxydata)[ncol(Galaxydata)] <- 
      names(Galaxy)[ncol(Galaxy)]

  names(Galaxydata)
  ncol(Galaxydata)

  #####Plots and Graphs
  ggplot(Galaxydata,
                aes(
                             y=galaxySentiment,
                                      x=samsunggalaxy
                                    )
                ) + geom_point() + stat_smooth(method ="lm", se = F)

  # greg's stuff

  # create a df of sentiment only (for histogram)
  sentimentFactor <- data.frame(as.numeric(Galaxydata[,ncol(Galaxydata)]))
  # remove zeroes
  sentimentFactorNonZero <- subset(data.frame(sentimentFactor), sentimentFactor != 0)

  # remove outliers (> +/- 3 stdevs)
  sentimentFactorNonZero <- (
                               subset(
                                          data.frame(sentimentFactorNonZero),
                                              sentimentFactorNonZero < 3*sd(sentimentFactorNonZero[,1]) &
                                                        sentimentFactorNonZero > -3*sd(sentimentFactorNonZero[,1])
                                                      )
                               )

  # reset the df index, needed for charting
  sentimentFactorNonZero <- data.frame(sentimentFactorNonZero[,1])

  ggplot(sentimentFactorNonZero,
                aes(
                             x = data.frame(sentimentFactorNonZero)
                                    )
                ) + geom_histogram(binwidth = 10)


  # parul's stuff

  names(data.frame(sentimentFactor))
  ggplot(Galaxydata,
                aes(x=iphone,
                             y=samsunggalaxy)) + 
  geom_point() + stat_smooth(method ="lm", se = F)




#ggplot(Galaxydata, aes(galaxySentiment, samsunggalaxy)) +
#  geom_jitter() 

ggplot(Galaxydata, aes(samsungdispos,samsungdisneg)) +
      geom_point(size=1) + geom_smooth() +
        ggtitle("Samsung Display")

    ggplot(Galaxydata, aes(samsungcampos,samsungcamneg)) +
          geom_point(size=1) + geom_smooth() +
            ggtitle("Samsung Camera")

        ggplot(Galaxydata, aes(samsungperpos,samsungperneg)) +
              geom_point(size=1) + geom_smooth() +
                ggtitle("Samsung Performance")


                  

            dev.off()

            ggplot(Galaxydata, aes(x=samsungdispos, y=samsungdisneg, 
                                                      color=galaxySentiment)) + 
  geom_point() + 
    geom_smooth()
ggplot(Galaxydata, aes(x=samsunggalaxy, y=iphone, 
                                              color=galaxySentiment)) + 
  geom_point() + 
    geom_smooth()
ggplot(iphonedata, aes(x=samsunggalaxy, y=iphone, 
                                              color=iphoneSentiment)) + 
  geom_point() +
    geom_smooth()


ggplot(Galaxydata, aes(x=samsunggalaxy, y=iphone, 
                                              color=galaxySentiment)) + 
  geom_smooth()

ggplot(iphonedata, aes(x=samsunggalaxy, y=iphone, 
                                              color=iphoneSentiment)) + 
  geom_smooth()

dev.off()

##################################

install.packages("arules")
library(arules)

##Discretize Samsung Galaxy Sentiment and create a vector containing discretized levels
disfixed7 <- discretize(Galaxydata$galaxySentiment, "fixed", categories= c(-Inf, -40, -10, -1, 1, 10, 40, Inf))
summary(disfixed7)
str(disfixed7)

#Insert the vector into the dataset
Galaxydata$galaxySentiment <- disfixed7

set.seed(170)
##Create Data Partition
ptr=0.7
inTrain <- createDataPartition(y = Galaxydata$galaxySentiment, p=ptr, list=FALSE)

Galaxy_training <- Galaxydata[ inTrain,]
Galaxy_testing <- Galaxydata[-inTrain,]
nrow(Galaxy_training)
nrow(Galaxy_testing)

##trainControl
ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 1)

######## KNN ############

##train                   
GalaxyknnFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "knn",
                                            trControl=ctrl, preProcess = "scale")
GalaxyknnFit

##predict
GalaxyknnPredict <- predict(GalaxyknnFit,Galaxy_testing)
table(GalaxyknnPredict)

##Confusion Matrix
confusionMatrix(GalaxyknnPredict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxyknnPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

######### Random Forest ##########
install.packages("randomForest")
library(randomForest)

##Random Forest Model 10 fold cross validation
rfFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "rf",
                              tuneLength = 10, trControl=ctrl, preProcess = c("center", "scale"))

rfFit

##predict
GalaxyrfPredict <- predict(rfFit, newdata = Galaxy_testing)
table(GalaxyrfPredict)

##Confusion Matrix
confusionMatrix(GalaxyrfPredict, Galaxy_testing$galaxySentiment)

##Resample
print(postResample(pred=GalaxyrfPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

###### SVM ############
#install.packages("e1071")
library("e1071")

GalaxysvmFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "svmRadial",
                                      tuneLength = 10, trControl=ctrl, preProcess = c("center", "scale"))
GalaxysvmFit
grid <- expand.grid(sigma = c(.1,.2,.3),
                                        C = c(1,2,4))

GalaxysvmFit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "svmRadial",
                                      tuneGrid=grid, trControl=ctrl, preProcess = c("center", "scale"))
GalaxysvmFit

##predict
GalaxysvmPredict <- predict(GalaxysvmFit, newdata = Galaxy_testing)
table(GalaxysvmPredict)

##Confusion Matrix
confusionMatrix(GalaxysvmPredict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxysvmPredict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

######## C5.0 ########
install.packages("C50")
library(C50)
library(mlbench)

GalaxyC50Fit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "C5.0",
                                      tuneLength = 10, trControl=ctrl)
GalaxyC50Fit
GalaxyC50grid <- expand.grid( .winnow = FALSE, .trials=c(10,20,30), .model=c("tree"))
GalaxyC50Fit <- train(galaxySentiment ~ ., data = Galaxy_training, method = "C5.0",
                                      tuneGrid=GalaxyC50grid, trControl=ctrl)

##predict
GalaxyC50Predict <- predict(GalaxyC50Fit, Galaxy_testing)
table(GalaxyC50Predict)

##Confusion Matrix
confusionMatrix(GalaxyC50Predict, Galaxy_testing$galaxySentiment)

##postResample
print(postResample(pred=GalaxyC50Predict, obs=as.factor(Galaxy_testing[,"galaxySentiment"])))

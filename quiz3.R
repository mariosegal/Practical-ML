install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)


test <- subset(segmentationOriginal,Case="Test")
train <- subset(segmentationOriginal,Case="Train")

test1 <- data.frame(TotalIntenCh2=c(23000),FiberWidthCh1=c(10),PerimStatusCh1=c(2))
print(model$finalModel)
predict(model,newdata=test1[1,])

#PS PS PS not possible


#3
install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
newdata = as.data.frame(t(colMeans(olive)))

library(tree)
model1 <- tree(Area~.,data=olive)
model1
predict(model1,newdata)

#answer = 2.875


#4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
model2 <- train(chd~age+alcohol+tobacco+obesity+typea+ldl,data=trainSA,method="glm",family="binomial")
summary(model2)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd,predict(model2,trainSA,type="raw"))
missClass(testSA$chd,predict(model2,testSA,type="raw"))


set.seed(13234)
model2a <- glm(chd~age+alcohol+tobacco+obesity+typea+ldl,data=trainSA,family="binomial")
summary(model2a)
missClass(trainSA$chd,predict(model2a,trainSA,type="response"))
missClass(testSA$chd,predict(model2a,testSA,type="response"))


#answer 0.27 0.31

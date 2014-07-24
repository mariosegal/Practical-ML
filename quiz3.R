install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)
library(rattle)

test <- subset(segmentationOriginal,Case="Test")
train <- subset(segmentationOriginal,Case="Train")

set.seed(125)
tree1 <- train(Class~.,data=train[-c(1:2)],method="rpart")
plot(tree1$finalModel);text(tree1$finalModel,pretty=2)
fancyRpartPlot(tree1$finalModel)
test1 <- data.frame(TotalIntenCh1=c(23000),FiberWidthCh1=c(10))
print(tree1$finalModel)
predict(tree1,newdata=test1)

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


#answer train 0.27  test 0.31


#5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y )
vowel.test$y <- as.factor(vowel.test$y )



set.seed(33833)
rf <- train(y~.,method="rf",data=vowel.train,importance=T)
vi1 <- varImp(rf,type=2)
vi1$var <- row.names(vi1)
vi1[order(vi1$Overall),]

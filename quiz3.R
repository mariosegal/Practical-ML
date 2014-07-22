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

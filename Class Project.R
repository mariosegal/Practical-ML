### Practical ML Project
### Mario Segal

#Load Libraries
require(caret)
require(rattle)

#1 Read Data
columns <- c("character","factor","date","Date","character","factor",rep("numeric",153),"factor")

train_raw <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=T,strip.white=T,stringsAsFactors=F)
test_raw <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=T,strip.white=T,stringsAsFactors=F)

#the data was imported as character in many cases and that is incorrect, let's fix that
#for now colums 6-159 should be numeric
train <- train_raw
bad <- which(sapply(train,is.character))
train[bad[4:36]] <- lapply(train[bad[4:36]],as.numeric)

test <- test_raw
bad1 <- which(sapply(test,is.character))
#no need to convert the test


#Do some visual exploration of the data
library(ggplot2); library(gridExtra)
vars <- setdiff(which(sapply(train, function(x) sum(!is.na(x))>0)),1:8)
pdf("Exploratory Charts v1.pdf",width=11,height = 8)
for (i in 1:length(vars)) {
    ch <- ggplot(train,aes_string(x=names(train)[160],fill=names(train)[160],y=names(train)[vars[i]]))+theme_bw()+
      geom_jitter(color="blue",position = position_jitter(width = .1),size=1)+theme(legend.position="none")+ 
      geom_boxplot(alpha=0.5)
 print(ch)
}
dev.off()
#I can see some variables do seem to discriminate among the classes

#There is a lot of missing data
round(100*table(sapply(train,function(x) sum(is.na(x))))/19622,2)

# most varibles are either complete or mostly missing
#I will chose to take the ones with full data
#It turns out the summary variables were very sparse and I am supressing those
full_vars <- which(sapply(train,function(x) sum(is.na(x)))==0)
full_vars <- full_vars[-(1:7)]

train_full <- subset(train,,full_vars)
nearZeroVar(train_full)  #good they have no NAs and have proper variance

#I will use PCA to reduce the dimensions, I think the analysis could work without it as well
#From my experience, some people use PCA on groups of variables to reduce them - I will do that for each sensor
#as it makes sense to me they will be different
groups <- c("_belt","_arm","_dumbbell","_forearm")

#Check for highly correlated matrices as per Prof. Leek in the lecture
M <- abs(cor(train_full[which(grepl(groups[1],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see a lot of correlation for belt

M <- abs(cor(train_full[which(grepl(groups[2],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see some  correlation for arm

M <- abs(cor(train_full[which(grepl(groups[3],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see some  correlation for dumbbell

M <- abs(cor(train_full[which(grepl(groups[4],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see very littel for forearm

#create PC's for each sensor
pc_belt <- preProcess(train_full[which(grepl(groups[1],names(train_full)))],method="pca")
pc_belt1 <- predict(pc_belt,train_full[which(grepl(groups[1],names(train_full)))])
names(pc_belt1) <- tolower(paste(names(pc_belt1),groups[1],sep=""))

pc_arm <- preProcess(train_full[which(grepl(groups[2],names(train_full)))],method="pca")
pc_arm1 <- predict(pc_arm,train_full[which(grepl(groups[2],names(train_full)))])
names(pc_arm1) <- tolower(paste(names(pc_arm1),groups[2],sep=""))

pc_dumbbell <- preProcess(train_full[which(grepl(groups[3],names(train_full)))],method="pca")
pc_dumbbell1 <- predict(pc_dumbbell,train_full[which(grepl(groups[3],names(train_full)))])
names(pc_dumbbell1) <- tolower(paste(names(pc_dumbbell1),groups[3],sep=""))

pc_forearm <- preProcess(train_full[which(grepl(groups[4],names(train_full)))],method="pca")
pc_forearm1 <- predict(pc_forearm,train_full[which(grepl(groups[4],names(train_full)))])
names(pc_forearm1) <- tolower(paste(names(pc_forearm1),groups[4],sep=""))

#visualize the PCs
ggplot(pc_arm1,aes(x=pc1_arm,y=pc2_arm,color=train_full$classe))+geom_point()
ggplot(pc_dumbbell1,aes(x=pc1_dumbbell,y=pc2_dumbbell,color=train_full$classe))+geom_point()+coord_cartesian(ylim=c(0,10))
ggplot(pc_belt1,aes(x=pc1_belt,y=pc2_belt,color=train_full$classe))+geom_point()
ggplot(pc_forearm1,aes(x=pc1_forearm,y=pc2_forearm,color=train_full$classe))+geom_point()+coord_cartesian(ylim=c(-5,5))
#The charts seem to group but not perfectly

#combine all the PCs into an analysis set
train_pc <- data.frame(cbind(pc_belt1,pc_arm1,pc_dumbbell1,pc_forearm1),classe=train_full$classe)

#Apply the PCS to test set;
test_pc_arm <- predict(pc_arm,newdata=test[row.names(pc_arm$rotation)])
names(test_pc_arm) <- tolower(paste(names(test_pc_arm),"_arm",sep=""))
test_pc_forearm <- predict(pc_forearm,newdata=test[row.names(pc_forearm$rotation)])
names(test_pc_forearm) <- tolower(paste(names(test_pc_forearm),"_forearm",sep=""))
test_pc_dumbbell <- predict(pc_dumbbell,newdata=test[row.names(pc_dumbbell$rotation)])
names(test_pc_dumbbell) <- tolower(paste(names(test_pc_dumbbell),"_dumbbell",sep=""))
test_pc_belt <- predict(pc_belt,newdata=test[row.names(pc_belt$rotation)])
names(test_pc_belt) <- tolower(paste(names(test_pc_belt),"_belt",sep=""))

test_pc <- data.frame(cbind(test_pc_arm,test_pc_belt,test_pc_dumbbell,test_pc_forearm))

#####Analysis ####

#check for complete cases
sum(complete.cases(train_pc)) 

#Always a good idea to start with a tree
model1 <- train(classe~.,data=train_pc,method="rpart") 
model1$finalModel   
fancyRpartPlot(model1$finalModel) #this did not work at all as it is not predicting C or E
confusionMatrix(predict(model1,train_pc),train_pc$classe)

#Lets try a more complicated approach - Random Forest that is more robust
model2 <- train(classe~.,data=train_pc,method="rf") 
model2
confusionMatrix(model2)
confusionMatrix(predict(model2,train_pc),train_pc$classe)
varImp(model2);
plot(varImp(model2),main="Random Forest Model Variable Importance")


#visualize on a fancy chart I personally like
results2 <- data.frame(actual=train_pc$classe,predicted=predict(model2,train_pc))
ggplot(results2,aes(x=actual,y=predicted,color=actual))+geom_jitter(size=0.5,alpha=0.5)+theme_bw()+theme(legend.position="none")

#predict and generate the cases for evaluation
test_pred1 <- predict(model2,test_pc)
setwd("Prediction")
pml_write_files(test_pred1)

#visualize how the top 2 PCs in variable importance separate
ggplot(pc_dumbbell1,aes(x=pc5_dumbbell,y=pc3_dumbbell,color=train_full$classe))+geom_point()+
  coord_cartesian(ylim=c(0,10))+coord_cartesian(xlim=c(-4,4),ylim=c(0,5))+theme_bw()+
  theme(legend.position="bottom")+
  guides(colour = guide_legend(title.position = "top",title.hjust=0.5))+
  scale_x_continuous("5th Dumbbell Principal Component")+
  scale_y_continuous("3rd Dumbbell Principal Component")+scale_color_discrete("Exercise Class")

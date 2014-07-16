### Practical ML Project
### Mario Segal

#Load Libraries
require(caret)

#1 Read Data

columns <- c("character","factor","date","Date","character","factor",rep("numeric",153),"factor")

train_raw <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=T,strip.white=T,stringsAsFactors=F)
test_raw <- read.csv(url("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=T,strip.white=T,stringsAsFactors=F)

#the data was imported as character in may cases and that is incorrect, let's fix that
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

#There is a lot of missing data
round(100*table(sapply(train,function(x) sum(is.na(x))))/19622,2)

# most varibles are either complete or mostly missing
#I will chose to take the ones with full data

full_vars <- which(sapply(train,function(x) sum(is.na(x)))==0)
full_vars <- full_vars[-(1:7)]

train_full <- subset(train,,full_vars)
train_full <- train_full[-c(1:7)]
groups <- c("_belt","_arm","_dumbbell","_forearm")
#may be highly correlated, first let's check that

#pairs(train_full[which(grepl(groups[1],names(train_full)))])
cor(train_full[which(grepl(groups[1],names(train_full)))])

M <- abs(cor(train_full[which(grepl(groups[1],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see a lot of correlation for belt

M <- abs(cor(train_full[which(grepl(groups[2],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see some  correlation for belt

M <- abs(cor(train_full[which(grepl(groups[3],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see some  correlation for dumbbell


M <- abs(cor(train_full[which(grepl(groups[4],names(train_full)))]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
#I see very littel for forearm


pc_belt <- preProcess(train_full[which(grepl(groups[1],names(train_full)))],method="pca")
pc_belt1 <- predict(pc_belt,train_full[which(grepl(groups[1],names(train_full)))])
names(pc_belt1) <- paste(names(pc_belt1),groups[1],sep="_")

pc_arm <- preProcess(train_full[which(grepl(groups[2],names(train_full)))],method="pca")
pc_arm1 <- predict(pc_arm,train_full[which(grepl(groups[2],names(train_full)))])
names(pc_arm1) <- paste(names(pc_arm1),groups[2],sep="_")

pc_dumbbell <- preProcess(train_full[which(grepl(groups[3],names(train_full)))],method="pca")
pc_dumbbell1 <- predict(pc_dumbbell,train_full[which(grepl(groups[3],names(train_full)))])
names(pc_dumbbell1) <- paste(names(pc_dumbbell3),groups[3],sep="_")

pc_forearm <- preProcess(train_full[which(grepl(groups[4],names(train_full)))],method="pca")
pc_forearm1 <- predict(pc_forearm,train_full[which(grepl(groups[4],names(train_full)))])
names(pc_forearm1) <- paste(names(pc_forearm4),groups[4],sep="_")




#####Analysis ####
sum(complete.cases(train[c(8:159)]))   #There are no complete cases, so we can't just work with full data
sapply(train[c(8:159)])

#I will have to drop variables or impute

#I think I have to scale the data first - then do the analysis
#it also seems the missing data is a problem, so I would likely have to impute it

#try SVD to reduce the variables
svd1 <- svd(scale(train[,c(8:159)]))

#It does not seem to me I will be able to identify them by sight, but clearly the arm measures are the most 
#discrinant by sight, and the charts are kind of cool

# I think I need to run some PCA on this or other technique to idetify the top variables


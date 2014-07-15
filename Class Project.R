### Practical ML Project
### Mario Segal


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

#It does not seem to me I will be able to identify them by sight, but clearly the arm measures are the most 
#discrinant by sight, and the charts are kind of cool

# I think I need to run some PCA on this 


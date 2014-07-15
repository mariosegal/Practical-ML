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

ggplot(train,aes_string(x=names(train)[160],fill=names(train)[160],y=names(train)[10]))+geom_boxplot()



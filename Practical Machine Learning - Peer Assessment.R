path <- getwd()
urlTrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" 
download.file(urlTrain, file.path(path, 'training.csv'))

urlTest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urlTest, file.path(path, "testing.csv"))

train <- read.csv("training.csv")
test <- read.csv("testing.csv")

dim(train)

table(train$classe)

install.packages('caret', dependencies = TRUE)
library(caret)

set.seed(123456)
trainset <- createDataPartition(train$classe, p = 0.8, list = FALSE)
Training <- train[trainset,]
Validation <- train[-trainset,]

nzvcol <- nearZeroVar(Training)
Training <- Training[, -nzvcol]

cntlength <- sapply(Training, function(x){
  sum(!(is.na(x) | x == ""))
}) 

nullcol <- names(cntlength[cntlength < 0.6 * length(Training$classe)])
descriptcol <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                 "cvtd_timestamp", "new_window", "num_window")
excludecols <- c(descriptcol, nullcol)
Training <- Training[, !names(Training) %in% excludecols]

library(randomForest)
rfModel <- randomForest(as.factor(classe) ~., data = Training, importance = TRUE, ntrees = 10)

ptraining <- predict(rfModel, Training)
confusionMatrix(table(ptraining, Training$classe))

pvalidation <- predict(rfModel, Validation)
confusionMatrix(table(pvalidation, Validation$classe))

ptest <- predict(rfModel, test)
ptest

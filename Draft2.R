Train_Test_Raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
Validation_Raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
dim(Train_Test_Raw)
dim(Validation_Raw)
str(Train_Test_Raw)
str(Validation_Raw)

Train_Test <- Train_Test_Raw[, colSums(is.na(Train_Test_Raw)) == 0]
classeTT <- Train_Test$classe
Train_Test <- select(Train_Test, -X, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)
train_test <- Train_Test[, sapply(Train_Test, is.numeric)]
train_test$classe <- classeTT

Validation <- Validation_Raw[, colSums(is.na(Validation_Raw)) == 0]
classeV <- Validation$classe
Validation <- select(Validation, -X, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)
validation <- Validation[, sapply(Validation, is.numeric)]
validation$classe <- classeV

set.seed(12321)
inTrain <- createDataPartition(train_test$classe, p = 0.75, list = FALSE)

train <- train_test[inTrain, ]
test <- train_test[-inTrain, ]

cv <- trainControl(method="cv", 5)
model <- train(classe ~ ., 
               data = train, 
               method = "rf", 
               trControl = cv, 
               ntree=250)
model

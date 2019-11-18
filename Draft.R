### DRAFT ###

# One thing that people regularly do is quantify how much of a particular activity they do, 
# but they rarely quantify how well they do it. In this project, your goal will be to use 
# data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

# Six young health participants were asked to perform one set of 10 repetitions of the 
# Unilateral Dumbbell Biceps Curl in five different fashions: 
## exactly according to the specification (Class A), 
## throwing the elbows to the front (Class B), 
## lifting the dumbbell only halfway (Class C), 
## lowering the dumbbell only halfway (Class D), 
## and throwing the hips to the front (Class E).

# Opening libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(Hmisc)
library(rpart)

# Reading data
Train_Test_Raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
Validation_Raw <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# Checking NAs
NA_col <- as.data.frame(sapply(Train_Test_Raw, function(x) sum(is.na(x))))
NA_col

# Selecting columns without NAs & no predicting relevance
Train_Test <- Train_Test_Raw[, colSums(is.na(Train_Test_Raw)) == 0]
Train_Test <- select(Train_Test, -X, -user_name, -raw_timestamp_part_1, -raw_timestamp_part_2, -cvtd_timestamp)
sum(is.na(Train_Test)) # 0

# Partition data in train & test
set.seed(12321)
inTrain <- createDataPartition(Train_Test$classe, p = 0.75, list = FALSE)

train <- Train_Test[inTrain, ]
test <- Train_Test[-inTrain, ]

# Exploring data
dim(train)
dim(test)

# Check predictor variable in both sets
plot(table(train$classe))
plot(table(test$classe))

# Further exploring
str(train)
summary(train)

# Training

cv <- trainControl(method="cv", 5)
model <- train(classe ~ ., 
               data = train, 
               method = "rf", 
               trControl = cv, 
               ntree = 250)
model

cv <- trainControl(method = "cv", 5)
model <- train(classe ~ ., 
               data = train, 
               method = "rf", 
               trControl = cv, 
               ntree=250)
model

# Testing & checking accuracy

predict <- predict(model, test)
confusionMatrix(test$classe, predict)

accuracy <- postResample(predict, test$classe)
accuracy
oose <- 1 - as.numeric(confusionMatrix(test$classe, predict)$overall[1])
oose

# Applying to validation set

result <- predict(model, testCleaned[, -length(names(testCleaned))])
result


train_noNA_NA <- as.data.frame(sapply(train_noNA, function(x) sum(is.na(x))))

glimpse(train)

unique(train$classe)
unique(train$user_name)

hist(train$accel_arm_x)
hist(train$num_window)
hist(train$roll_belt)
hist(train$pitch_belt)
hist(train$yaw_belt)
hist(train$total_accel_belt)

table(train$user_name)
table(train$classe)
table(train$new_window)
table(train$num_window)

plot(train$roll_belt, train$classe)

# Partition data

# Training model



model <- train(classe ~ ., data = train, method = "glm")

# load libraries
library(mlbench)
library(caret)

# load data
read_file <- read.csv('C:/Users/Sheikh Afaan/Desktop/Btech proj/Project/3.our_spot_checking/depression_nor.csv',header=TRUE,stringsAsFactors = F) 
str(read_file)
# rename dataset to keep code below generic
dataset <- read_file

#new
set.seed(3233)
intrain <- createDataPartition(y = dataset$Depression, p= 0.7, list = FALSE)
training <- dataset[intrain,]
testing <- dataset[-intrain,]
dim(training); 
dim(testing);
anyNA(dataset)
training[["Depression"]] = factor(training[["Depression"]])
#newends

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 3233

preProcess=c("center", "scale")

# Linear Discriminant Analysis
set.seed(seed)
lda <- train(Depression~., data=dataset, method="lda", preProc=c("center", "scale"), trControl=control)
# Logistic Regression
set.seed(seed)
glm <- train(Depression~., data=dataset, method="glm", trControl=control)
# GLMNET
set.seed(seed)
glmnet <- train(Depression~., data=dataset, method="glmnet", preProc=c("center", "scale"), trControl=control)
# SVM Radial
set.seed(seed)
svm <- train(Depression~., data=dataset, method="svmRadial", preProc=c("center", "scale"), trControl=control, fit=FALSE)
# kNN
set.seed(seed)
knn <- train(Depression~., data=dataset, method="knn", preProc=c("center", "scale"), trControl=control)


#results
results <- resamples(list(lda=lda, logistic=glm,glmnet=glmnet,
                          svm=svm, knn=knn))

summary(results)

#measuring
lda
glm
svm
knn
glmnet


#graphic results
dotplot(results)

barplot

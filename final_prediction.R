library(caret)

depression_model <- read.csv("C:/Users/Sheikh Afaan/Desktop/Project/4.our_prediction/depression_nor.csv" , header=TRUE , stringsAsFactors = F) 
str(depression_model)
head(depression_model) 

set.seed(3233)
intrain <- createDataPartition(y = depression_model$Depression, p= 0.7, list = FALSE)
training <- depression_model[intrain,]
testing <- depression_model[-intrain,]

dim(training)
dim(testing)

anyNA(depression_model)
summary(depression_model)

training[["Depression"]] = factor(training[["Depression"]])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
svm_Radial <- train(Depression ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial
summary(svm_Radial)



test_pred_Radial <- predict(svm_Radial, newdata = testing)
tab <- table(Predicted = test_pred_Radial, Actual = testing$Depression )
tab
confusionMatrix(tab)
1 - sum(diag(tab))/sum(tab)


new_depression <- data.frame("ASadness"=0,"Loconcen"=0,"Asleep"=0,"Aappet"=0,"Loenergy"=0,"Foguilt"=0,"Asbehav"=0,"Sthough"=0,"Ppains"=0,"Eactivity"=0,"Wloss"=0,"Ssupport"=0,"Etdsthin"=0,"Dmaking"=0,"Fhopilln"=0,"Addiction"=0)
predict(svm_Radial, newdata=new_depression)


library(kernlab)
install.packages("caret")
library(caret)
library(plyr)
library(e1071)


# Data(Train)
train_sal <- read.csv("E:\\Assignment\\17) Suport vector machines\\SalaryData_Train(1).csv")
str(train_sal)

View(train_sal)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

# Data(Test)
test_sal <- read.csv("E:\\Assignment\\17) Suport vector machines\\SalaryData_Test(1).csv")
str(test_sal)

View(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)


# Building model 
model1<-ksvm(train_sal$Salary~., 
             data= train_sal, kernel = "vanilladot")
model1

#Evaluating model
Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)

agreement <- Salary_prediction == test_sal$Salary
table(agreement)

prop.table(table(agreement))

# kernel = rfdot 
model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) # 85.19

# kernel = vanilladot
model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) # 84.64


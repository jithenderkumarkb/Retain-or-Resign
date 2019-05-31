#Importing Data Set

train <-read.csv("Attrition (1).csv")

#takingBackUp of the dataSet
train_BackUp <- read.csv("Attrition (1).csv")

#Getting to know the data
library(Hmisc)
library(psych)

describe(train)
str(train)

#checking for missing values

library(dplyr)
library(magrittr)

sapply(train,function(x) sum(is.na(x)))
train %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))

#Checking for correlation
install.packages("corrplot")
library(corrplot)
names(train)
t <- cor((train %>% select("Age","DailyRate","DistanceFromHome","EmployeeCount","EmployeeNumber","HourlyRate","MonthlyIncome","MonthlyRate","NumCompaniesWorked","PercentSalaryHike","StandardHours","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager")))
corrplot(t, method="number", type = "upper")

#Manipulating Data/Data Preparation

#Leaving columns that does not have impact on Y
train <- train[ , !(names(train) %in% c("EmployeeCount","Over18","StandardHours"))]

#handling/Manipulating Categorical Variables

str(train$BusinessTravel) #Factor
str(train$Department) #Factor
str(train$Education) # From Into to Order
unique(train$Education)

#Converting it to ordered DataType from INT
train$Education <- ordered(train$Education,levels = c(min(train$Education):max(train$Education)))
str(train$Education)

str(train$EducationField) #Factor
unique(train$EducationField)

str(train$EnvironmentSatisfaction) #From INT TO ORDER
unique(train$EnvironmentSatisfaction)
train$EnvironmentSatisfaction <- ordered(train$EnvironmentSatisfaction,levels = c(min(train$EnvironmentSatisfaction):max(train$EnvironmentSatisfaction)))
unique(train$EnvironmentSatisfaction)

str(train$Gender) #Factor

str(train$JobInvolvement) #From INT TO ORDER
unique(train$JobInvolvement)
train$JobInvolvement <- ordered(train$JobInvolvement,levels = c(min(train$JobInvolvement):max(train$JobInvolvement)))

str(train$JobLevel) #Fro INT TO ORDER
unique(train$JobLevel)
train$JobLevel <- ordered(train$JobLevel,levels = c(min(train$JobLevel):max(train$JobLevel)))

str(train$JobRole) #Factor

str(train$JobSatisfaction) # from INT TO ORDER
unique(train$JobSatisfaction)
train$JobSatisfaction <- ordered(train$JobSatisfaction,levels = c(min(train$JobSatisfaction):max(train$JobSatisfaction)))

str(train$MaritalStatus) #Factor

str(train$OverTime) #Factor

str(train$PerformanceRating) # From INT TO ORDER
unique(train$PerformanceRating)
train$PerformanceRating <- ordered(train$PerformanceRating,levels = c(min(train$PerformanceRating):max(train$PerformanceRating)))

str(train$RelationshipSatisfaction)  #From INT TO ORDER
unique(train$RelationshipSatisfaction)
train$RelationshipSatisfaction <- ordered(train$RelationshipSatisfaction,levels = c(min(train$RelationshipSatisfaction):max(train$RelationshipSatisfaction)))

str(train$StockOptionLevel) #From INT TO FACTOR
unique(train$StockOptionLevel)
train$StockOptionLevel <- as.factor(train$StockOptionLevel)

str(train$WorkLifeBalance) # From INT To ORDER
unique(train$WorkLifeBalance)
train$WorkLifeBalance <- ordered(train$WorkLifeBalance,levels = c(min(train$WorkLifeBalance):max(train$WorkLifeBalance)))

#Handling/Manipulating Numerical Varibales / Performing Transformations

#Checking for Skewness :
### Age : 0.41
### Daily Rate : 0
### Distance from Home : 0.96
### Employee Number : No need to see since it is primary key
### Hourly rate : -0.03
# Monthly Income : 1.37
#### CubeRoot Monthly Income : 0.6756
### Monthly Rate : 0.02
### Number of companies worked : No need to check to my knowledge.
### Percent Salary hike : 0.82 
# Totalwokring years :  1.92
### SquareRoot Total Working Hours : 0.176
### Training Times since last year : 0.55
# Years at company : 3.91
### Square Root Years at company : 0.425
### Years in current role : 0.47
# Years since last promotion : 3.59
###Cube Root Since Last promotion : .198
### Year with current manager : 0.16

#Performing sqrt for column whose skewness is greater than |1|

train$SqrtMonthlyIncome <- sqrt(train$MonthlyIncome)
skew(train$SqrtMonthlyIncome) #0.86

cubeRoot <- function(x) {
  sign(x) * abs(x)^(1/3)
}

train$CbrtMonthlyIncome <- cubeRoot(train$MonthlyIncome)
skew(train$CbrtMonthlyIncome) #.6756



#Total working years
train$SqrtTtlWorkingYrs <- sqrt(train$TotalWorkingYears)
skew(train$SqrtTtlWorkingYrs) #0.176

train$CbrtTtlWorkingYrs <- cubeRoot(train$TotalWorkingYears)
skew(train$CbrtTtlWorkingYrs) #-.395


#years in company
train$SqrtyrsinCmpny <- sqrt(train$YearsAtCompany)
skew(train$SqrtyrsinCmpny) #0.426

train$CbrtyrsinCmpny <- cubeRoot(train$YearsAtCompany)
skew(train$CbrtyrsinCmpny) # -0.367

#Year since last promotion 
train$Sqrtyrssncpromotion <- sqrt(train$YearsSinceLastPromotion)
skew(train$Sqrtyrssncpromotion) #.0737
train$Cbrtyrssncpromotion <- cubeRoot(train$YearsSinceLastPromotion)
skew(train$Cbrtyrssncpromotion) # .198

#Generating Graphs 
hist(train$YearsSinceLastPromotion)
hist(train$Cbrtyrssncpromotion)

boxplot(train$YearsSinceLastPromotion,horizontal = T)
boxplot(train$Cbrtyrssncpromotion,horizontal = T)

trainBackUp1 <- train

names(train)
#Removing Unwanted Columns like Totalwokring years, Monthly Income and et cetera
train <- train[ , !(names(train) %in% c("YearsAtCompany","CbrtyrsinCmpny","YearsSinceLastPromotion","Sqrtyrssncpromotion","SqrtMonthlyIncome","MonthlyIncome","TotalWorkingYears","CbrtTtlWorkingYrs","StandardHours"))]

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(train$Attrition, SplitRatio = 0.8)
training_Data = subset(train, split == TRUE)
test_Data = subset(train, split == FALSE)

#Analysis Part
#Performing Logistic Regression

LogisticModel <- glm(Attrition ~ . , family = "binomial" , data = training_Data)
summary(LogisticModel)
training_Data$logisticModel_preds <- predict.glm(LogisticModel,training_Data,type="response")

table(training_Data$logisticModel_preds)
training_Data$logisticModel_preds <- ifelse(training_Data$logisticModel_preds>=0.5,"Yes","No")
cm_Logistic <- table(training_Data$logisticModel_preds, training_Data$Attrition)
Accuracy_Logistic_train <- sum(diag(cm_Logistic))/sum(cm_Logistic)
Accuracy_Logistic_train #.90646
sensitivity_logistic_train <- cm_Logistic[1,1] / (cm_Logistic[1,1] + cm_Logistic[2,1])
sensitivity_logistic_train #0.9767
Specificity_logistic_train <- cm_Logistic[2,2] / (cm_Logistic[2,2] + cm_Logistic[1,2])
Specificity_logistic_train #.5421

#Performing it on test data
test_Data$logisticModel_preds <- predict.glm(LogisticModel,test_Data,type="response")
test_Data$logisticModel_preds <- ifelse(test_Data$logisticModel_preds>=0.5,"Yes","No")
cm_Logistic_test <- table(test_Data$logisticModel_preds, test_Data$Attrition)
Accuracy_Logistic_test <- sum(diag(cm_Logistic_test))/sum(cm_Logistic_test)
Accuracy_Logistic_test #.90816
sensitivity_logistic_test <- cm_Logistic_test[1,1] / (cm_Logistic_test[1,1] + cm_Logistic_test[2,1])
sensitivity_logistic_test #0.972
Specificity_logistic_test <- cm_Logistic_test[2,2] / (cm_Logistic_test[2,2] + cm_Logistic_test[1,2])
Specificity_logistic_test #.5745


#Performing Decision Tree

library(party)

OutTree <- ctree(Attrition ~ . , data = train)
training_Data$DecisionTreePreds <- predict(OutTree,training_Data)
cm_DecisionTree <- table(training_Data$DecisionTreePreds, training_Data$Attrition)
Accuracy_DecisionTree_train <- sum(diag(cm_DecisionTree))/sum(cm_DecisionTree)
Accuracy_DecisionTree_train #.8563
sensitivity_DecsionTree_train <- cm_DecisionTree[1,1] / (cm_DecisionTree[1,1] + cm_DecisionTree[2,1])
sensitivity_DecsionTree_train #0.976
Specificity_DecsionTree_train <- cm_DecisionTree[2,2] / (cm_DecisionTree[2,2] + cm_DecisionTree[1,2])
Specificity_DecsionTree_train # .236



#Performing on Test Data
test_Data$DecisionTreePreds <- predict(OutTree,test_Data)
#test_Data$DecisionTreePreds <- ifelse(test_Data$logisticModel_preds>=0.5,"Yes","No")
cm_DecisionTree_test <- table(test_Data$DecisionTreePreds, test_Data$Attrition)
Accuracy_DecisionTree_test <- sum(diag(cm_DecisionTree_test))/sum(cm_DecisionTree_test)
Accuracy_DecisionTree_test #.8571
sensitivity_DecsionTree_test <- cm_DecisionTree_test[1,1] / (cm_DecisionTree_test[1,1] + cm_DecisionTree_test[2,1])
sensitivity_DecsionTree_test # 1
Specificity_DecsionTree_test <- cm_DecisionTree_test[2,2] / (cm_DecisionTree_test[2,2] + cm_DecisionTree_test[1,2])
Specificity_DecsionTree_test # 1

#Random Forests
install.packages('randomForest')
library(randomForest)
RandomForest_Model <-  randomForest(Attrition ~ . , data = train)
training_Data$randfomForestPreds <- predict(RandomForest_Model, training_Data)
cm_randfomForest_train <- table(training_Data$randfomForestPreds, training_Data$Attrition)
Accuracy_Randomforest_train <- sum(diag(cm_randfomForest_train))/sum(cm_randfomForest_train)
Accuracy_Randomforest_train # 1
sensitivity_RandomForest_train <- cm_randfomForest_train[1,1] / (cm_randfomForest_train[1,1] + cm_randfomForest_train[2,1])
sensitivity_RandomForest_train #1
Specificity_RandomForest_train <- cm_randfomForest_train[2,2] / (cm_randfomForest_train[2,2] + cm_randfomForest_train[1,2])
Specificity_RandomForest_train # 1



#Performing on Test Data
test_Data$randfomForestPreds <- predict(RandomForest_Model,test_Data)
#test_Data$DecisionTreePreds <- ifelse(test_Data$logisticModel_preds>=0.5,"Yes","No")
cm_RandomForest_test <- table(test_Data$randfomForestPreds, test_Data$Attrition)
Accuracy_Randomforest_test <- sum(diag(cm_RandomForest_test))/sum(cm_RandomForest_test)
Accuracy_Randomforest_test # 1
sensitivity_RandomForest_test <- cm_RandomForest_test[1,1] / (cm_RandomForest_test[1,1] + cm_RandomForest_test[2,1])
sensitivity_RandomForest_test # 1
Specificity_RandomForest_test <- cm_RandomForest_test[2,2] / (cm_RandomForest_test[2,2] + cm_RandomForest_test[1,2])
Specificity_RandomForest_test # 1

#Support Vector Machine 

install.packages('e1071')
library(e1071)
#kernel - radial
SVC_radial_Model <- svm(Attrition ~ . , data = training_Data,kernel=("radial"))
training_Data$svmradialPreds <- predict(SVC_radial_Model,training_Data)
cm_svmradial_train <- table(training_Data$svmradialPreds, training_Data$Attrition)
Accuracy_svmRadial_train <- sum(diag(cm_svmradial_train))/sum(cm_svmradial_train)
Accuracy_svmRadial_train # 1
sensitivity_svmRadial_train <- cm_svmradial_train[1,1] / (cm_svmradial_train[1,1] + cm_svmradial_train[2,1])
sensitivity_svmRadial_train #1
Specificity_svmRadial_train <- cm_svmradial_train[2,2] / (cm_svmradial_train[2,2] + cm_svmradial_train[1,2])
Specificity_svmRadial_train # 1


#Performing On test data
test_Data$svmradialPreds <- predict(SVC_radial_Model,test_Data)
cm_svmradial_test <- table(test_Data$svmradialPreds, test_Data$Attrition)
Accuracy_svmRadial_test <- sum(diag(cm_svmradial_test))/sum(cm_svmradial_test)
Accuracy_svmRadial_test # 1
sensitivity_svmRadial_test <- cm_svmradial_test[1,1] / (cm_svmradial_test[1,1] + cm_svmradial_test[2,1])
sensitivity_svmRadial_test #1
Specificity_svmRadial_test <- cm_svmradial_test[2,2] / (cm_svmradial_test[2,2] + cm_svmradial_test[1,2])
Specificity_svmRadial_test # 1


#Performing Other Analysis :

#Distance From Home for Attrition

distHomeAttrition <- train %>% select(Attrition , DistanceFromHome) %>% group_by(Attrition) %>% summarise(Mean_Distance = mean(DistanceFromHome) , Median_Distance = median(DistanceFromHome))

#From the Analysis, For Employee with "NO" Attrition their distance to office from home is nearer than employee with "YES" attrition.

#Education and Income for Attrition

eduIncomeAttrition <- train_BackUp %>% select(Attrition ,Education , EducationField , MonthlyIncome) %>% group_by(Education,Attrition) %>%summarise(meanSalary = mean(MonthlyIncome))



!(names(train_BackUp)) %in% names(train)

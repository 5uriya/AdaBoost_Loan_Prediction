# AdaBoost_Loan_Prediction

Adaboost: Universal Bank Dataset
The Universal Bank dataset has 14 variables and 5000 records. Use “Personal.Loan” as target variable.
1. Import the data into R
library(vegan)
library(dummies)
library(ada)
attr = c('id', 'age', 'exp', 'inc', 'zip','family', 'ccavg', 'edu', 'mortgage','loan', 'securities', 'cd', 'online', 'cc')
# Read the data using csv file
data = read.csv(file = "UniversalBank.csv", header = TRUE, col.names = attr)
2. Drop ID & ZIP Code, exp
drop_Attr = c("id", "zip", "exp")
attr = setdiff(attr, drop_Attr)
data = data[, attr]
3. Using domain knowledge separate categorical and numeric attributes. Convert them into appropriate types.
cat_Data <- data.frame(sapply(data[,cat_Attr], as.factor))
num_Data <- data.frame(sapply(data[,num_Attr], as.numeric))
data = cbind(num_Data, cat_Data)
4. Standardize numerical data using range method
cla_Data = decostand(data[,ind_Num_Attr], "range")
5. Convert all categorical attributes into numeric
# Using dummy function, convert education and family categorical attributes into numeric attributes
edu = dummy(data$edu)
family = dummy(data$family)
cla_Data = cbind(cla_Data, edu, family)
ind_Cat_Attr = setdiff(ind_Cat_Attr, c("edu","family")) rm(edu, family)
# Using as.numeric function, convert remaining categorical attributes into numeric attributes
cla_Data = cbind(cla_Data, sapply(data[,ind_Cat_Attr],as.numeric)) rm(ind_Cat_Attr)
ind_Attr = names(cla_Data)
6. Append the target attribute
cla_Data = cbind(cla_Data, loan=data[,"loan"])
7. Split the data into train, test data sets
set.seed(123)
train_RowIDs = sample(1:nrow(cla_Data),
nrow(cla_Data)*0.6) train_Data = cla_Data[train_RowIDs,]
test_Data = cla_Data[-train_RowIDs,] 
8.Build the classification model using Adaboost:
model = ada(x = train_Data[,ind_Attr],y = train_Data$loan,iter=20, loss="logistic")
9. Predict the values using model on train and test data sets, and calculate accuracy.
# Predict on train data
pred_Train = predict(model, train_Data[,ind_Attr])
# Build confusion matrix and find accuracy
cm_Train = table(train_Data$loan, pred_Train)
accu_Train= sum(diag(cm_Train))/sum(cm_Train)
rm(pred_Train, cm_Train)
# Predict on test data
pred_Test = predict(model, test_Data[,ind_Attr])
# Build confusion matrix and find accuracy
cm_Test = table(test_Data$loan, pred_Test)
accu_Test= sum(diag(cm_Test))/sum(cm_Test)
rm(pred_Test, cm_Test)

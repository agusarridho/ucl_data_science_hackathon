require(glmnet)
require(xgboost)
setwd("~/Desktop/PEACH Data")
load("sleep_data_full.rda")
X = sleep_data_full
X$Sleep_Quality_Value = NULL
X$DateTime = NULL
X$Dinner_Time = NULL
X$Nationality = NULL
X$Current_City_of_Domicile = NULL
X$Dinner_Dried_Fruit = NULL # All values are zeros!
X$Symptom_Feedback = NULL
X$Current_Smoker = as.numeric(X$Current_Smoker) - 1
X$Alcoholic = as.numeric(X$Alcoholic) - 1
X$Outgoing_Social_Life = as.numeric(X$Outgoing_Social_Life) - 1
X$Outdoors_Social_Life = as.numeric(X$Outdoors_Social_Life) - 1
y = sleep_data_full$Sleep_Quality_Value

# City_of_Domicile_Type
City_of_Domicile_Type_UrbanTown = as.numeric(X$City_of_Domicile_Type==0)
City_of_Domicile_Type_SuburbanTown = as.numeric(X$City_of_Domicile_Type==1)
City_of_Domicile_Type_RuralTown = as.numeric(X$City_of_Domicile_Type==2)
X$City_of_Domicile_Type = NULL
X = cbind(X, City_of_Domicile_Type_UrbanTown,
      City_of_Domicile_Type_SuburbanTown,
      City_of_Domicile_Type_RuralTown)
# Income_Bracket
Income_Bracket_Poor = as.numeric(X$Income_Bracket==0)
Income_Bracket_MiddleClass = as.numeric(X$Income_Bracket==1)
Income_Bracket_Wealthy = as.numeric(X$Income_Bracket==2)
X$Income_Bracket = NULL
X = cbind(X, Income_Bracket_Poor, Income_Bracket_MiddleClass, Income_Bracket_Wealthy)
# Education
Education_None = as.numeric(X$Education==0)
Education_Primary = as.numeric(X$Education==1)
Education_Secondary = as.numeric(X$Education==2)
Education_BachelorsDegree = as.numeric(X$Education==3)
Education_PdD = as.numeric(X$Education==4)
X$Education = NULL
X = cbind(X, Education_None, Education_Primary,
          Education_Secondary, Education_BachelorsDegree,
          Education_PdD)
# Type of Work
Type_of_Work_Sedentary = as.numeric(X$Type_of_Work=="sedentary")
Type_of_Work_Active_indoors = as.numeric(X$Type_of_Work=="active_indoors")
Type_of_Work_Active_outdoors = as.numeric(X$Type_of_Work=="active_outdoors")
X$Type_of_Work = NULL
X = cbind(X, Type_of_Work_Sedentary, Type_of_Work_Active_indoors,
          Type_of_Work_Active_outdoors)

# Train glmnet
idList = unique(X$UserId)
idTrain = idList[1 : floor(length(idList)*0.6)]
idValid = idList[ceiling(length(idList)*0.6) : floor(length(idList)*0.8)]
idTest = idList[ceiling(length(idList)*0.8) : length(idList)]
Xtrain = X[X$UserId %in% idTrain, ]
yTrain = y[X$UserId %in% idTrain]
Xvalid = X[X$UserId %in% idValid, ]
yValid = y[X$UserId %in% idValid]
Xtest = X[X$UserId %in% idTest, ]
yTest = y[X$UserId %in% idTest]

# plot(Xtrain$Current_Smoker, yTrain)
# plot(Xtrain$Alcoholic, yTrain)
# plot(Xtrain$Outgoing_Social_Life, yTrain)
# plot(Xtrain$Outdoors_Social_Life, yTrain)

# train glmnet
Xtrain$UserId = NULL
Xvalid$UserId = NULL
Xtrain = as.matrix(Xtrain)
Xvalid = as.matrix(Xvalid)
yTrain = as.matrix(yTrain)
yValid = as.matrix(yValid)

fit = glmnet(Xtrain, yTrain)

lambdaList = (0:10)/10 #2^(-10:25)
fit = glmnet(Xtrain, yTrain, alpha=1, lambda=lambdaList)
predictions = predict(fit, Xvalid)
errorsList = matrix(NaN, nrow=length(lambdaList), ncol=1)
for (i in 1:length(lambdaList)) {
    errorsList[i] = mean((predictions[,i]-yValid)^2)
}
myBestFit = glmnet(XtrainTemp, yTrain, alpha=1, lambda=2^15)


print(fit)
fit$beta
# Train the model. alpha=1 means lasso
#save(X, y, file="trainData.RData")
#load("trainData.RData")
X = as.matrix(X)
y = as.matrix(y)
#save.image(v3.RData)
set.seed(1)
t1 = proc.time()
fit = cv.glmnet(X, y)
t2 = proc.time()
plot(myFit, label = TRUE)
myFit$lambda.min
coef(myFit, s="lambda.min")



myFit = cv.glmnet(as.matrix(Xtrain[,-c(1)]), as.matrix(yTrain), alpha=0.8)


xgb20 = xgboost(data = as.matrix(Xtrain[,-c(1)]), label = yTrain, nrounds = 20, params = list(objective = "reg:linear"))



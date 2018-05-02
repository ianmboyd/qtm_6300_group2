# Force the RAND seed (for consistency)
set.seed(1234)
library(readr)
library(gmodels)
library(rpart)
library(rpart.plot)


IAN = FALSE

if( IAN ){
  source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")
  #Load the data from local storage
  df = read_csv('c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/F2F_2_Wayfair/wayfair_click_stream_short.csv')
}else{
  source("/Users/wolfeb3/Dropbox/Babson/QTM6300, Data Explorations and Analytics/BabsonAnalytics.R")
  #Read and clean up the data
  df = read_csv("C:/Users/wolfeb3/desktop/wayfair_click_stream_short.csv")
}

<<<<<<< HEAD:Wayfair - Tree.R
# Clean data

#Remove un-needed columns

df$TestID = NULL;
df$sessionstartdate = NULL;
df$opid = NULL;
df$HashSKU = NULL;
#df = df[df$HadBasket == TRUE, ]
=======

#
# Format Data
#
#
df$HadBasket = as.factor(df$HadBasket)
df$HadBasket = as.logical(df$HadBasket)
#df = df[df$HadBasket == 1, ]
>>>>>>> 455b6c6f468d5f862792dc57021f901e12c5f635:wayfair_logistic_regression.R

df$HadBasket = as.factor(df$HadBasket)
df$HadBasket = as.logical(df$HadBasket)
df$HadReceiptPage = as.factor(df$HadReceiptPage)
df$HadReceiptPage = as.logical(df$HadReceiptPage)

df$testgroupname = as.factor(df$testgroupname)
df$Platform = as.factor(df$Platform)
df$VisitorType = as.factor(df$VisitorType)

# Make NULL values "other"
df$MkcName[is.na(df$MkcName)] = '(Other)'
# df$MkcName['(Other)'] = "AA_OTHER";

df$MkcName = as.factor(df$MkcName)
df$PriceBucket = as.factor(df$PriceBucket)

df$HadPDP = as.factor(df$HadPDP)
df$HadPDP = as.logical(df$HadPDP)

df$isshipsintime = as.logical(df$isshipsintime)



#
# Create derived target (abandoned cart)
#

df$drop_cart <- (df$HadReceiptPage == FALSE & df$HadBasket == TRUE)

#
# Inspect this data a bit and check for integrity
# 

convert_rate = sum(df$HadReceiptPage) / nrow(df); # % of people who checked out - 0.07426
abandon_rate = sum(df$drop_cart) / sum(df$HadBasket); # % of people who had a basket that abandoned it - 0.7402201



# Clean data

#Remove un-needed columns

df$TestID = NULL;
df$sessionstartdate = NULL;
df$opid = NULL;
df$HashSKU = NULL;



# Remove variables used to derive target
df$HadBasket = NULL
df$HadReceiptPage = NULL


# Create a test and training set
n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]


# Create the Model 
<<<<<<< HEAD:Wayfair - Tree.R
control = rpart.control(minsplit=100, minbucket=10, cp=0.001)
model = rpart(drop_cart ~ ., data=train, control = control)
rpart.plot(model)

pred = predict(model, test)
predTF = pred > .5

errorRate = sum(predTF != test$drop_cart)/nrow(test)
errorBench = benchmarkErrorRate(train$drop_cart, test$drop_cart)
errorRate
errorBench

CrossTable(predTF, test$drop_cart, expected = F, prop.r = F, proop.c = F, prop.t=F, prop.chisq=F)

#False Negative Rate: FN/(TP+FN) : 1 - sensitivity
fnr = 3954/(3564+3954) #52.6%

#False Positive Rate: FP/(FP+TB) : 1 - Specificity
fpr = 445/(445+32037) #1.37%

hist(pred)
=======
model = glm(drop_cart ~ ., data=train, family=binomial) # Generalized Linear Model
# Step the Model Down
#step_model = step(model) # Step did not remove any variables

# Run some predictions...
# 
pred = predict(model, test, type="response")

predTF = pred >.25

errorRate = sum(predTF != test$drop_cart)/nrow(test)
errorBench = benchmarkErrorRate(train$drop_cart, test$drop_cart)


CrossTable(predTF, test$drop_cart, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
#              | test$drop_cart 
#       predTF |     FALSE |      TRUE | Row Total | 
# -------------|-----------|-----------|-----------|
#        FALSE |     32004 |      3940 |     35944 | 
# -------------|-----------|-----------|-----------|
#         TRUE |       478 |      3578 |      4056 | 
# -------------|-----------|-----------|-----------|
# Column Total |     32482 |      7518 |     40000 | 
# -------------|-----------|-----------|-----------|
# 

# 
# False Negative Rate : FN/(TP+FN) : 1-Sensitivity 
fnr = 3940/(3578+3940) # 52%

# 
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
fpr = 478/(478+32004) # 2%
>>>>>>> 455b6c6f468d5f862792dc57021f901e12c5f635:wayfair_logistic_regression.R

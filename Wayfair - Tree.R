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

# Clean data

#Remove un-needed columns

df$TestID = NULL;
df$sessionstartdate = NULL;
df$opid = NULL;
df$HashSKU = NULL;
#df = df[df$HadBasket == TRUE, ]

df$HadBasket = as.factor(df$HadBasket)
df$HadBasket = as.logical(df$HadBasket)
df$HadReceiptPage = as.factor(df$HadReceiptPage)
df$HadReceiptPage = as.logical(df$HadReceiptPage)

df$testgroupname = as.factor(df$testgroupname)
df$Platform = as.factor(df$Platform)
df$VisitorType = as.factor(df$VisitorType)
df$MkcName = as.factor(df$MkcName)
df$PriceBucket = as.factor(df$PriceBucket)

df$HadPDP = as.factor(df$HadPDP)
df$HadPDP = as.logical(df$HadPDP)

df$isshipsintime = as.logical(df$isshipsintime)

# Create derived target (abandoned cart)

df$drop_cart <- (df$HadReceiptPage == FALSE & df$HadBasket == TRUE)

# Remove variables used to derive target
df$HadBasket = NULL
df$HadReceiptPage = NULL


# Create a test and training set
n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]


# Create the Model 
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

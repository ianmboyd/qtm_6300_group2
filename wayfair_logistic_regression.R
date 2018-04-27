# Force the RAND seed (for consistency)
set.seed(1234)
library(readr)
library(gmodels)


IAN = TRUE


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

df$drop_cart <- ( df$HadReceiptPage == FALSE & df$HadBasket == TRUE)

# Remove variables used to derive target
df$HadBasket = NULL
df$HadReceiptPage = NULL


# Create a test and training set
n = nrow(df)
trainingCases = sample(n, round(n*.60))

train = df[trainingCases, ]
test = df[-trainingCases, ]


# Create the Model 
model = glm(drop_cart ~ ., data=train, family=binomial) # Generalized Linear Model
# Step the Model Down
step_model = step(model)

# Run some predictions...
# 
pred = predict(model, test, type="response")
step_pred = predict(step_model, test, type="response")

predTF = pred >.5
step_predTF = step_pred  >.5


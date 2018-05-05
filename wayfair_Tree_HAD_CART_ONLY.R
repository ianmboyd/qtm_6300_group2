# Force the RAND seed (for consistency)
set.seed(1234)
library(readr)
library(gmodels)
library(rockchalk) # handy level collapsing
library(ggplot2)
library(DescTools) # using this for MAPE and RMSE below


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


#
# Format Data
#
#
df$HadBasket = as.factor(df$HadBasket)
df$HadBasket = as.logical(df$HadBasket)

df$HadReceiptPage = as.factor(df$HadReceiptPage)
df$HadReceiptPage = as.logical(df$HadReceiptPage)

df$testgroupname = as.factor(df$testgroupname)
df$Platform = as.factor(df$Platform)
df$VisitorType = as.factor(df$VisitorType)

df$MkcName[is.na(df$MkcName)] = "OTHER" # Handle observations without a product category
df$MkcName = as.factor(df$MkcName)

df$PriceBucket = as.factor(df$PriceBucket)

df$HadPDP = as.factor(df$HadPDP)
df$HadPDP = as.logical(df$HadPDP)

df$isshipsintime = as.logical(df$isshipsintime)

#
# Only look at those who had a basket
# If we looked at people who never had a basket in the first place, our results would be skewed.
# 
df = df[df$HadBasket == 1, ]

#
# Create derived target (abandoned cart)
#
df$drop_cart <- ( df$HadReceiptPage == FALSE & df$HadBasket == TRUE)

#
# Inspect this data a bit and check for integrity
# 

convert_rate = sum(df$HadReceiptPage) / nrow(df); # % of people who checked out - 0.07426
abandon_rate = sum(df$drop_cart) / sum(df$HadBasket); # % of people who had a basket that abandoned it - 0.7402201


#
# Clean data
#
#
#Remove un-needed columns

df$TestID = NULL; # Same for all observations (not interesting)
df$sessionstartdate = NULL; # Same for all observations (not interesting)
df$opid = NULL; # Non-Descriptive information 
df$HashSKU = NULL; # Random-Anonymized product information (not useful)


# Remove variables used to derive target - otherwise they will count double and influence results
df$HadBasket = NULL 
df$HadReceiptPage = NULL

#
# Look at some other parts of the data
# 
product_categories = summary(df$MkcName) # get all the levels of MkcName - there are 35 of them
price_buckets = summary(df$PriceBucket) # 15 price buckets


#
# Because there are so many levels, 
# with some of them don't have many occurrances in the set, 
# and likely aren't too influential.
#

plot(sort(product_categories)[1:25], ylab="Frequency", xlab="Rank") # 21/36 have counts ~< 500, 

# let's collapse them into a single factor
collapse_set = names(sort(product_categories)[1:21]);
df$MkcName = combineLevels(df$MkcName, levs=collapse_set, newLabel = c('OTHER'))

pie(summary(df$PriceBucket), main="Price Buckets")
min(summary(df$PriceBucket)) # 33
max(summary(df$PriceBucket)) # 4285

pie(summary(df$MkcName), main="Product Categories")
min(summary(df$MkcName)) # 46
max(summary(df$MkcName)) # 4010

# Create a test and training set
n = nrow(df)
trainingCases = sample(n, round(n*.60)) # 60/40 split

train = df[trainingCases, ]
test = df[-trainingCases, ]
################################
# Create the tree Model 
control = rpart.control(minsplit=250, minbucket=10, cp=0.005)
model = rpart(drop_cart ~ ., data=train, control = control)
rpart.plot(model)

pred = predict(model, test)
predTF = pred > .5
hist(pred)

errorRate = sum(predTF != test$drop_cart)/nrow(test)
errorBench = benchmarkErrorRate(train$drop_cart, test$drop_cart)
errorRate
errorBench

CrossTable(predTF, test$drop_cart, expected = F, prop.r = F, proop.c = F, prop.t=F, prop.chisq=F)
#
#             | test$drop_cart 
#      predTF |     FALSE |      TRUE | Row Total | 
#  -----------|-----------|-----------|-----------|
#       FALSE |      1049 |       979 |      2028 | 
#             |     0.389 |     0.130 |           | 
#  -----------|-----------|-----------|-----------|
#        TRUE |      1645 |      6542 |      8187 | 
#             |     0.611 |     0.870 |           | 
#  -----------|-----------|-----------|-----------|
#Column Total |      2694 |      7521 |     10215 | 
#             |     0.264 |     0.736 |           | 
#  -----------|-----------|-----------|-----------|
  

#False Negative Rate: FN/(TP+FN) : 1 - sensitivity
fnr = 979/(6542+979) #13.02%

#False Positive Rate: FP/(FP+TN) : 1 - Specificity
fpr = 1645/(445+1645) #78.71%

hist(pred)
################################
#
# DO RMSE
# Can't do MAPE because target is categorical
# 

RMSE = RMSE(predTF, test$drop_cart) #50.68%

MAPE = MAPE(predTF, test$drop_cart)

############################################################################


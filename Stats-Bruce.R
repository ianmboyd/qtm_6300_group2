library(rpart)
library(rpart.plot)
library(readr)
library(gmodels)
library(MLmetrics)
source("/Users/wolfeb3/Dropbox/Babson/QTM6300, Data Explorations and Analytics/BabsonAnalytics.R")
set.seed(1234)


#lookup table for converting total order value to int
keys <- c("unknown","0 - 20","20 - 40", "40 - 60","60 - 80","80 - 100","100 - 150","150 - 200","200 - 250" ,"250 - 300","300 - 400","400 - 500","500 - 600","600 - 700","700 +")
vals <- c(0, 10,30,50,70,90,125,175,225,275,350,450,550,650,700)
dflookup <- data.frame(keys,vals)

#Read and clean up the data
df = read_csv("C:/Users/wolfeb3/desktop/wayfair_click_stream_short.csv")

#Convert data
df$testgroupname <- as.factor(df$testgroupname)
df$testgroupname <- as.logical(df$testgroupname)
df$HashSKU = NULL
df$PriceBucket <- dflookup[match(df$PriceBucket,dflookup$keys, nomatch=-1),2]
df$isshipsintime <- as.factor(df$isshipsintime)
df$opid <- NULL
df$SessionCount <- as.factor(df$SessionCount)
df$lost <- (df$HadReceiptPage==FALSE & df$HadBasket==TRUE)
sum(df$lost)

df$testgroupname <- NULL
df$HadReceiptPage <- NULL
df$HadBasket<- NULL
df$VisitorType <- as.factor(df$VisitorType)
df$MkcName <- as.factor(df$MkcName)
df$PriceBucket <- as.factor((df$PriceBucket))
df$Platform <- as.factor((df$Platform))


#logistic model
#Part 2 - Partition the data using a 60-40 training-test split, and construct Na¨ive
#Bayes' predictions for the target
N = nrow(df)
trainingSize = round(N*0.6)
trainingCases = sample(N, trainingSize)
training = df[trainingCases,]
test = df[-trainingCases,]

model = glm( df$lost~ ., data = training, family = binomial)
pred = predict(model, test, type="class")



#Segment Customer Type
totalDollarsPDP <- sum(df$PriceBucket*df$HadPDP)
sum(df$HadPDP)
totalDollarsPDP

totalDollarsBasket <- sum(df$PriceBucket*df$HadBasket)
sum(df$HadBasket)
totalDollarsBasket

totalDollarsReceipt <- sum(df$PriceBucket*df$HadReceiptPage)
sum(df$HadReceiptPage)
totalDollarsReceipt

DollarsLost <- totalDollarsBasket - totalDollarsReceipt
DollarsLost

library(rpart)
library(rpart.plot)
library(readr)
library(gmodels)
library(MLmetrics)
source("/Users/wolfeb3/Dropbox/Babson/QTM6300, Data Explorations and Analytics/BabsonAnalytics.R")

#lookup table for converting total order value to int
keys <- c("unknown","0 - 20","20 - 40", "40 - 60","60 - 80","80 - 100","100 - 150","150 - 200","200 - 250" ,"250 - 300","300 - 400","400 - 500","500 - 600","600 - 700","700 +")
vals <- c(0, 10,30,50,70,90,125,175,225,275,350,450,550,650,700)
dflookup <- data.frame(keys,vals)

#Read and clean up the data
df = read_csv("C:/Users/wolfeb3/desktop/wayfair_click_stream_short.csv")

#Convert data
df$testgroupname <- as.factor(df$testgroupname)
df$testgroupname <- as.logical(df$testgroupname)
df$testgroupname <- as.factor(df$testgroupname)
df$HashSKU = NULL
df$PriceBucket <- dflookup[match(df$PriceBucket,dflookup$keys, nomatch=-1),2]
df$isshipsintime <- as.factor(df$isshipsintime)
#df$opid <- 
df$SessionCount <- as.factor(df$SessionCount)



tracebackdf$Customer_ID = NULL
#df$Customer_Session_Start_Date  <- as. (df$Custom_Session_Start_Date) 
df$Order_ID = NULL
df$Order_Product_ID = NULL
df$Product_ID = NULL
#df$Purchased_Qty
#df$Returned_Qty
df$Cancelled <- as.logical(df$Cancelled == 1)
df$Guarantee_Shown <- as.logical(df$Guarantee_Shown == 1)
df$Product_Category <- as.factor(df$Product_Category)
#df$Total_Order_Value
#df$Customer_Estimated_Delivery_Date = NULL
df$Customer_Actual_Delivery_Date = NULL
df$visitor_type_Id <- as.factor(df$Visitor_Type_ID)
df$Vistor_Type_Name = NULL
df$Platform_ID <- as.logical(df$Platform_ID == 2)
df$Platform_Name = NULL
df$Customer_Session_Start_Date = NULL
df$ShipClassName = NULL

# Some Numbers
#Order Average
OrderAverage = mean(df$Total_Order_Value)





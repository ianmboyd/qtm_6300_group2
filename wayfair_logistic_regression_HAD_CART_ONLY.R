# Force the RAND seed (for consistency)
set.seed(1234)
library(readr)
library(gmodels)
library(rockchalk) # handy level collapsing
library(ggplot2)
library(DescTools) # using this for MAPE and RMSE below


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


#
# Format Data
#
#
df$HadBasket = as.factor(df$HadBasket)
df$HadBasket = as.logical(df$HadBasket)

cart_rate = sum(df$HadBasket) / nrow(df)

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
convert_basket_holders = sum(df$HadReceiptPage) / sum(df$HadBasket) # % of people who created a basked but then converted



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
collapse_set = names(sort(product_categories)[1:10]);
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


# Create the Model 
model = glm(drop_cart ~ ., data=train, family=binomial) # Generalized Linear Model
# Step the Model Down
#step_model = step(model) # Step did not remove any variables

# Run some predictions...
# 
pred = predict(model, test, type="response")

hist(pred, main="Prediction Distribution", xlab="Prediction Confidence (%)", ylab="Number of Observations") 
# look at the distribution of predicted responses

summary(model) # look here yourself....

#Take a threshold of confidence 
predTF = pred >.5


errorRate = sum(predTF != test$drop_cart)/nrow(test)


errorBench = benchmarkErrorRate(train$drop_cart, test$drop_cart)


ct = CrossTable(predTF, test$drop_cart, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
print(ct)

# 
# False Negative Rate : FN/(FN+TP) : 1-Sensitivity 
# > 522/(522+6999)
# [1] 0.06940566

# 
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
# 2012/(2012+682)
# [1] 0.7468448

#
# DO RMSE
# Can't do MAPE because target is categoricald
# 

RMSE = RMSE(predTF, test$drop_cart)

############################################################################
############################################################################
############################################################################
############################################################################
# Let's try this again, with a more horizontal data structure
#
mm = model.matrix(~df$MkcName-1, df) # expand the product categories into a DF of booleans
new_df = cbind(df, mm) # create a new DF with all the products as new variables
new_df$MkcName = NULL # remove the source column

# Create a test and training set
n = nrow(new_df)
trainingCases = sample(n, round(n*.60)) # 60/40 split

train = new_df[trainingCases, ]
test = new_df[-trainingCases, ]

# Create the Model 
model = glm(drop_cart ~ ., data=train, family=binomial) # Generalized Linear Model
# Step the Model Down
step_model = step(model) # REMOVES a bunch of the product categories which are not useful.

summary(step_model)

# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                               1.52542    0.13529  11.275  < 2e-16 ***
#   testgroupnameshow SIT                    -0.16957    0.06246  -2.715 0.006632 ** 
#   VisitorType2                              0.20066    0.08617   2.329 0.019871 *  
#   VisitorType3                             -0.20169    0.08012  -2.517 0.011820 *  
#   VisitorType4                             -0.53504    0.07250  -7.379 1.59e-13 ***
#   Platform2                                 0.69723    0.04727  14.750  < 2e-16 ***
#   PriceBucket100 - 150                      0.37836    0.09158   4.132 3.60e-05 ***
#   PriceBucket150 - 200                      0.47697    0.09802   4.866 1.14e-06 ***
#   PriceBucket20 - 40                        0.20631    0.08340   2.474 0.013365 *  
#   PriceBucket200 - 250                      0.47943    0.11142   4.303 1.69e-05 ***
#   PriceBucket250 - 300                      0.70877    0.12245   5.788 7.11e-09 ***
#   PriceBucket300 - 400                      0.77642    0.11935   6.505 7.75e-11 ***
#   PriceBucket40 - 60                        0.42127    0.08837   4.767 1.87e-06 ***
#   PriceBucket400 - 500                      0.86726    0.14523   5.972 2.35e-09 ***
#   PriceBucket500 - 600                      0.79543    0.18400   4.323 1.54e-05 ***
#   PriceBucket60 - 80                        0.40719    0.09730   4.185 2.86e-05 ***
#   PriceBucket600 - 700                      0.91745    0.24058   3.813 0.000137 ***
#   PriceBucket700 +                          1.28994    0.17781   7.254 4.03e-13 ***
#   PriceBucket80 - 100                       0.46023    0.10489   4.388 1.14e-05 ***
#   PriceBucketunknown                       -2.36959    0.51751  -4.579 4.68e-06 ***
#   HadPDPTRUE                               -1.97055    0.05609 -35.129  < 2e-16 ***
#   isshipsintimeTRUE                        -0.22308    0.04269  -5.226 1.73e-07 ***
#   SessionCount                              0.75794    0.05186  14.616  < 2e-16 ***
#   `df$MkcNameAppliances`                   -0.54842    0.36026  -1.522 0.127940    
# `df$MkcNameDecorative Accents`            0.26896    0.06786   3.963 7.39e-05 ***
#   `df$MkcNameFlooring`                      1.13878    0.28810   3.953 7.73e-05 ***
#   `df$MkcNameFurniture - Bedroom`          -0.25994    0.10297  -2.524 0.011592 *  
#   `df$MkcNameHardware`                      0.93362    0.28521   3.273 0.001062 ** 
#   `df$MkcNameHeating & Grills`             -0.59513    0.17314  -3.437 0.000587 ***
#   `df$MkcNameKitchen`                      -0.41935    0.11664  -3.595 0.000324 ***
#   `df$MkcNameLighting`                      0.41227    0.09126   4.518 6.25e-06 ***
#   `df$MkcNameMattresses`                   -0.88395    0.14165  -6.240 4.36e-10 ***
#   `df$MkcNameOffice`                       -0.24582    0.11391  -2.158 0.030922 *  
#   `df$MkcNameOutdoor`                      -0.28366    0.18739  -1.514 0.130085    
# `df$MkcNameOutdoor Decor and Structures` -0.36342    0.15702  -2.314 0.020643 *  
#   `df$MkcNameRecreation`                   -0.31035    0.20583  -1.508 0.131616    
# `df$MkcNameRugs`                          0.38630    0.08555   4.516 6.32e-06 ***
#   `df$MkcNameSmall Electrics`              -0.62977    0.16755  -3.759 0.000171 ***
#   `df$MkcNameTabletop`                     -0.16066    0.09709  -1.655 0.097972 .  
# `df$MkcNameUpholstery`                   -0.26959    0.10417  -2.588 0.009653 ** 
# 
pred = predict(step_model, test, type="response")
hist(pred) # look at the distribution of predicted responses

predTF = pred >.5


errorRate = sum(predTF != test$drop_cart)/nrow(test)


errorBench = benchmarkErrorRate(train$drop_cart, test$drop_cart)


CrossTable(predTF, test$drop_cart, expected = F, prop.r = F, prop.c = F, prop.t = F, prop.chisq = F)
#              | test$drop_cart 
#       predTF |     FALSE |      TRUE | Row Total | 
# -------------|-----------|-----------|-----------|
#        FALSE |       718 |       552 |      1270 | 
# -------------|-----------|-----------|-----------|
#         TRUE |      1939 |      7006 |      8945 | 
# -------------|-----------|-----------|-----------|
# Column Total |      2657 |      7558 |     10215 | 
# -------------|-----------|-----------|-----------|
# 

# 
# False Negative Rate : FN/(TP+FN) : 1-Sensitivity 
fnr = 552/(7006+552) # 7%

# 
# False Postive Rate :  FP/(FP+TN) : 1-Specificity 
fpr = 1939/(1939+718) #  < 72%

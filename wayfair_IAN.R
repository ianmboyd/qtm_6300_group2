
#
# HEADER
#

# Force the RAND seed (for consistency)
set.seed(1234)

# Utilities
source("c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/BabsonAnalytics.R")

# Load Libraries
library(gmodels)
library(class)
library(readr)
library(ggplot2)

# /HEADER


#
# Load the data
# 
#C:\Users\iboyd\Documents\GRAD\S-2018\qtm6300\F2F_2_Wayfair
#

#Load the data from local storage
df = read_csv('c:/Users/iboyd/Documents/GRAD/S-2018/qtm6300/F2F_2_Wayfair/wayfair_click_stream_short.csv')



     
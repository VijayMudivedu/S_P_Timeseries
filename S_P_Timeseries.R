

#-----------------------------
# Business Understandading
#-----------------------------
# Business objective:
#
# Please apply any machine learning algorithm you are comfortable with for predicting this time series
#
# Please send your python jupyter books. We would review the results on Friday (June 8th)
#
# The results would be measured on:
#
# 1. Accuracy - How good are predictions
#
# 2. Visualization - How well are you able to convey your idea graphically
#
# 3. Code Cleanliness - How well have you documented your code in an easy language to understand. No need for excess code

library(tseries)
library(xts)
library(tidyverse)
library(reshape2)
library(forecast)
library(lubridate)

sp_historical_data <- read.csv(file = "historical_data.csv")
dim(sp_historical_data)
head(sp_historical_data)
str(sp_historical_data)

#-----------------------------
# Data Understanding
#-----------------------------

# DATA PREPARATION ----

# REMOVE UNWANTED COLUMNS ----

# remove the redundant columns symbol, name, class_type_of
# "SPY", "SPDR S&P500", "S_P_500"
sp_historical_data <-
  sp_historical_data[, -which(names(sp_historical_data) %in%
                                c("symbol", "name", "class_type_of"))]
head(sp_historical_data)


# Check for NAs       #-----------------------------
sapply(
  sp_historical_data,
  FUN = function(x)
    sum(is.na(x))
)
# Comments: NAs found in Lead_1, Lead_2 and Lead_10


# remove NAs -----------
#query to find  NAs
na_leads <-
  sapply(sp_historical_data[, which(names(sp_historical_data) %in%
                                      c("lead_1", "lead_5", "lead_10"))],
         function(x)
           which(is.na.data.frame(x)))
# Comments NAs were identified in lead_1, lead_5, lead_10
dim(sp_historical_data)

# NAS ARE
na_leads

# remove NAs from lead_10
sp_historical_cleaned <- sp_historical_data[-na_leads$lead_10, ]
dim(sp_historical_cleaned)


# find the NAs in lead_1 and lead_5
na_leads_iter1 <-
  sapply(sp_historical_cleaned[, which(names(sp_historical_cleaned) %in% c("lead_1", "lead_5","lead_10"))],
         function(x)
           which(is.na.data.frame(x)))

# NAs in lead iter1 are
na_leads_iter1

# remove the NA from lead_1
sp_historical_cleaned <-
  sp_historical_cleaned[-(na_leads_iter1$lead_1), ]

dim(sp_historical_cleaned)

# find the NAs in Lead 5
na_leads_iter2 <-
  sapply(sp_historical_cleaned[, which(names(sp_historical_data) %in% c("lead_1","lead_5","lead_10"))],
         function(x)
           which(is.na.data.frame(x)))

na_leads_iter2
# remove the NA from lead_5
sp_historical_cleaned <- sp_historical_cleaned[-na_leads_iter2$lead_5, ]
dim(x = sp_historical_cleaned)

# check for NAs
sapply(
  sp_historical_cleaned,
  FUN = function(x)
    sum(is.na(x))
)
# Comments: No NAs found


# recheck for NAs
sapply(
  sp_historical_cleaned,
  FUN = function(x)
    sum(is.na(x))
)

View(sp_historical_cleaned)

# Converting factors to Characters -----------------------------
str(sp_historical_cleaned)

# convert the date to R date format
sp_historical_cleaned$date_txn <- mdy(sp_historical_cleaned$date_txn) #%>%  head()

# check for uniuqes and duplicates ---------------------------------
sum(duplicated(x = sp_historical_data$date_txn))
unique(x = sp_historical_data$name)

# Check for invalid valid data    #-----------------------------
# minimum dips
sp_historical_cleaned[which.min(x = sp_historical_cleaned$open),]

# Check for outliers  #-----------------------------

#-----------------------------
# EXPLORATORY DATA ANALYSIS
#-----------------------------

plot(sp_historical_cleaned$date_txn,y = sp_historical_cleaned$open,type = "l")
lines(sp_historical_cleaned$date_txn,y = sp_historical_cleaned$low,type = "l",col = "red")
lines(sp_historical_cleaned$date_txn,y = sp_historical_cleaned$high,type = "l",col = "blue")
lines(sp_historical_cleaned$date_txn,y = sp_historical_cleaned$close_price,type = "l", col = "green")


ggplot(data = sp_historical_cleaned,aes(x = date_txn,y = close_price)) + 
  geom_line() +
  xlab(label = "transaction date") + ylab("closing price") + labs(title = "Closing Price to Transaction Date")

# Univariate analysis
ggplot(data = sp_historical_cleaned) + 
  geom_histogram(aes(x = open,fill = "red",alpha = 0.5)) + 
  geom_density(aes(open))# +  geom_histogram(aes(x = close_price,fill = "blue", alpha = 0.5))


# TIME-SERIES OBJECT 
# Creating a time series object of "sp_historical_cleaned"
sp_historical_ts <-   xts(x = sp_historical_cleaned[,-1],order.by = sp_historical_cleaned[,1]) #%>% head()

plot(sp_historical_ts$close_price,main = "Historical SPY Close")


apply.monthly(x = sp_historical_cleaned$date_txn,FUN = sum) %>% head()






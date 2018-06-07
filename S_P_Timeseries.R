
#-----------------------------
# Business Understandading
#-----------------------------
# Business objective:
#
# Please apply any machine learning algorithm you are comfortable with for predicting this time series
#

remove(list = ls())

# 
# install.packages("tseries")
# install.packages("xts")
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("forecast")
# install.packages("lubridate")
# install.packages("ggthemes")


library(tseries)
library(xts)
library(tidyverse)
library(reshape2)
library(forecast)
library(lubridate)
library(ggthemes)

# Read Data from the CSV

sp_historical_data <- read.csv(file = "Data/historical_data.csv")
dim(sp_historical_data)
head(sp_historical_data)
str(sp_historical_data)

#-----------------------------
# Data Understanding
#-----------------------------

# DATA PREPARATION ----


# Remove unwanted columns  ----

# remove the redundant columns symbol, name, class_type_of
# "SPY", "SPDR S&P500", "S_P_500"
sp_historical_cleaned <-
  sp_historical_data[,-which(names(sp_historical_data) %in% c("symbol", "name", "class_type_of"))]
head(sp_historical_cleaned)


# Check for NAs ----
sapply(
  sp_historical_cleaned,
  FUN = function(x)
    sum(is.na(x))
)
# Comments: NAs found in Lead_1, Lead_2 and Lead_10


# remove NAs ----
#query to find  NAs
na_leads <-
  sapply(sp_historical_cleaned[, which(names(sp_historical_cleaned) %in%
                                         c("lead_1", "lead_5", "lead_10"))],
         function(x)
           which(is.na.data.frame(x)))
# Comments NAs were identified in lead_1, lead_5, lead_10
dim(sp_historical_cleaned)

# NAs ARE ----
na_leads

# remove NAs from lead_10 ----
sp_historical_cleaned <- sp_historical_cleaned[-na_leads$lead_10, ]
dim(sp_historical_cleaned)

# find the NAs in lead_1 and lead_5 ----
na_leads_iter1 <-
  sapply(sp_historical_cleaned[, which(names(sp_historical_cleaned) %in% c("lead_1", "lead_5", "lead_10"))],
         function(x)
           which(is.na.data.frame(x)))

# NAs in lead iter1 are ----
na_leads_iter1

# remove the NA from lead_1 ----
sp_historical_cleaned <-
  sp_historical_cleaned[-(na_leads_iter1$lead_1),]

dim(sp_historical_cleaned)

# find the NAs in Lead 5 ----
na_leads_iter2 <-
  sapply(sp_historical_cleaned[, which(names(sp_historical_cleaned) %in% c("lead_1", "lead_5", "lead_10"))],
         function(x)
           which(is.na.data.frame(x)))

na_leads_iter2

# remove the NA from lead_5 ----
sp_historical_cleaned <-
  sp_historical_cleaned[-na_leads_iter2$lead_5,]
dim(x = sp_historical_cleaned)

# check for NAs ----
sapply(
  sp_historical_cleaned,
  FUN = function(x)
    sum(is.na(x))
)
# Comments: No NAs found

head(sp_historical_cleaned)

# check for outliers in close_price ----
ggplot(data = sp_historical_data,aes(x = symbol,y = close_price)) + geom_boxplot()
# Comments: No outliers found

# Converting factors to Characters ----
str(sp_historical_cleaned)

# convert the date to R date format ----
sp_historical_cleaned$date_txn <-
  mdy(sp_historical_cleaned$date_txn) #%>%  head()

# check for uniuqes and duplicates ----
sum(duplicated(x = sp_historical_data$date_txn))
# Comments: No duplicates found

# Check for invalid valid data ----
# checking for minimum dips 
sp_historical_cleaned[which.min(x = sp_historical_cleaned$open), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$low), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$high), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$close_price), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$lead_1), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$lead_5), ]
sp_historical_cleaned[which.min(x = sp_historical_cleaned$lead_10), ]

# There is bad data in open, low, high, lead_1, thus considering the "close_price"

#-----------------------------
# EXPLORATORY DATA ANALYSIS
#-----------------------------

# Plotting the datasets 
ggplot(data = sp_historical_cleaned) + geom_line(aes(x = date_txn,y = close_price)) +
  geom_line(aes(x = date_txn,y = lead_1, col = "chartresue1")) + #+ +
  geom_line(aes(date_txn,lead_5,col = "red")) + # 
  geom_line(aes(date_txn,lead_10, col = "blue")) +
  labs(title = "Lead Plots") +
  scale_color_manual(labels = c("Lead 1","Lead 5","Lead 10"), values = c("cornflowerblue","lightpink3", "darkgoldenrod2")) +
  theme_economist() +
  theme(legend.position = c(0.8, 0.3),
        legend.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size= 8),
        legend.title = element_blank())

# Comments: Thus considering the close_price as the lead_prices do not convey much information


plot(sp_historical_cleaned$date_txn,
     y = sp_historical_cleaned$open, ylab = "", xlab = "", main = "Open, low, high Price",
     type = "l")
lines(
  sp_historical_cleaned$date_txn,
  y = sp_historical_cleaned$low,
  type = "l",
  col = "red"
)
lines(
  sp_historical_cleaned$date_txn,
  y = sp_historical_cleaned$high,
  type = "l",
  col = "blue"
)
lines(
  sp_historical_cleaned$date_txn,
  y = sp_historical_cleaned$close_price,
  type = "l",
  col = "green"
)


# Univariate analysis
ggplot(data = sp_historical_cleaned, aes(close_price)) + 
  geom_histogram(aes(x = open, y = ..density.., fill = "red", alpha = 0.5),show.legend = FALSE) +
  geom_density(aes(y = ..density..,alpha = 0.1),show.legend = FALSE) +
  labs(title = "Distribution of Close_price Data")
  theme_economist() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none" ,
        axis.text.y = element_blank()
        )

# Comments: Most of the Pricing data is concentrated around 200 - 240

# TIME-SERIES OBJECT
# Creating a time series object of "sp_historical_cleaned"
sp_historical_ts <-  xts(x = sp_historical_cleaned[, -1], order.by = sp_historical_cleaned[, 1]) 

par(mfrow = c(2,2))

# Daily close price
plot(apply.daily(x = sp_historical_ts$close_price, FUN = mean), main = "Daily", cex = 0.4)

# Weekly close price
plot(
  apply.weekly(x = sp_historical_ts$close_price, FUN = mean),
  type = "l", cex = 0.4,
  col = "green",
  main = "Weekly"
)

# monthly close price
plot(
  apply.monthly(x = sp_historical_ts$close_price, FUN = mean),
  col = "red",cex = 0.4,
  main = "Monthly"
)

# quaterly close price
plot(
  apply.quarterly(x = sp_historical_ts$close_price, FUN = mean),
  col = "blue", cex = 0.4,
  main = "Quaterly"
)


#-----------------
# Breaking the dataset into Training and Validation datasets
#-----------------

test_rows <- 30  # one month data used for validation
training_rows <-
  nrow(sp_historical_cleaned) - test_rows  # training dataset has 600 rows

# training dataset ----
sp_train <- sp_historical_cleaned[1:training_rows, ] #

# test dataset ----
sp_test <-
  sp_historical_cleaned[(training_rows + 1):(training_rows + test_rows), ]
tail(sp_train)
head(sp_test)
dim(sp_train)
dim(sp_test)
#-----------------
# Smoothing
#----------------
# window width
w <- 9

par(mfrow = c(1,1))

plot(
  x = sp_train$close_price,
  type = "l",
  col = "black",
  lwd = 1, ylab =  "", xlab = ""
)

smoothed_moving_avg <-
  stats::filter(
    x = sp_train$close_price,
    filter = rep(x = (1 / (2 * w + 1)), (2 * w + 1)),
    sides = 2,
    method = "convolution"
  )

lines(smoothed_moving_avg, col = "darkgoldenrod3", lwd = 4)

# printing the smooted moving average ----
smoothed_moving_avg

# replacing the induced NAs as result of smoothing

#for (i in 1: )
lead_nas <- smoothed_moving_avg[w + 2] - smoothed_moving_avg[w + 1]

trail_nas <-
  smoothed_moving_avg[training_rows - w - 2] - smoothed_moving_avg[training_rows - w - 1]
lead_nas
trail_nas

# Replacing the lagging NAs are a result of windowing of moving average ----
for (i in seq(w, 1, -1)) {
  smoothed_moving_avg[i] <- smoothed_moving_avg[i + 1] - lead_nas #[i]
}

smoothed_moving_avg

# Replacing the smoothed leading NAs ----

for (i in (training_rows - w - 1):(training_rows)) {
  smoothed_moving_avg[i] <-
    smoothed_moving_avg[i - 1] + trail_nas #[(i - (training_rows - w - 1 ))]
}

smoothed_moving_avg
lines(smoothed_moving_avg, type = "l", col = "blueviolet")


# Smoothing using Holts method ----

alpha = seq(from = 0.09, to = 0.3, by = 0.1)
plot(
  x = sp_historical_cleaned$close_price,
  type = "l", ylab = "", xlab = "", main = "Verifying Smoothing - Holts Method",
  col = "black",
  lwd = 2
)
ln_colrs = c(
  "burlywood4",
  "coral",
  "cadetblue3",
  "chartreuse2",
  "chocolate4",
  "aquamarine",
  "black"
)


for (i in 1:length(alpha)) {
  smoothing_holts <-
    HoltWinters(
      x = sp_historical_cleaned$close_price,
      alpha = alpha[i],
      beta = FALSE,
      gamma = FALSE
    )
  lines(
    smoothing_holts$fitted[, 1],
    type = "l",
    col = ln_colrs[i],
    lwd = 2
  )
  
}
# Comments: Clearly, from Holts Method best smoothing happens when alpha is ~ 0.1

smoothing_holts <-
  HoltWinters(
    x = sp_historical_cleaned$close_price,
    alpha = 0.1,
    beta = FALSE,
    gamma = FALSE
  )

# Find smoothed series ----
plot(sp_historical_cleaned$close_price, type = "l")
lines(smoothing_holts$fitted[, 1], type = "l", col = "red")


# Creating a new dat frame for close_price and dates

smoothed_df <-
  cbind(index(sp_train), as.data.frame(smoothed_moving_avg))
names(smoothed_df) <- c("months_smoothed", "smoothed_close_price")

head(smoothed_df)

#--------------------
# Model Building
#--------------------
plot(sp_historical_cleaned$close_price,
     lwd = 1,
     type = "l",
     ylab = "",
     xlab = "",
     main = "SPY Close Price"
     )
lines(smoothed_df$smoothed_close_price,
      col = "coral3",
      lwd = 2)

# using the linear model fit
fit.close_price <-
  lm(
    formula = smoothed_close_price ~ cos(0.05 * months_smoothed) * poly(months_smoothed, 1)
    + sin(0.05 * months_smoothed) * poly(months_smoothed, 1),
    #+ months_smoothed,
    data = smoothed_df
  )

# summary of close
summary(fit.close_price)

lines(
  fit.close_price$fitted.values,
  type = "l",
  col = "darkorchid2",
  lwd = 3
)


# Stationarity tests on the residual time series -----
resi_close_price <-
  sp_train$close_price - fit.close_price$fitted.values

# adf test
adf.test(x = resi_close_price, alternative = "stationary")
kpss.test(x = resi_close_price)

# Comments: resi_close_price is Stationary

acf(x = resi_close_price) # AR =0  There is no auto regressive component
pacf(x = resi_close_price) # MA = 0 , there is no moving average noise component
# AR = 0 and MA = 0

Arima(y = resi_close_price)
# this has a very signma square, with a very high log likelihood. In addition to this this is AR(0) and MA(0) time series

# checking the noise and stationarity of the time series using the box-Ljung test
Box.test(x = resi_close_price, type = "Ljung-Box")
# Comments: thus the p-value of the time-series is very low making it a good fit to call it a Strictly Stationary time series.

# qqPlot of the residual timeseries
qqplot(x = smoothed_df$months_smoothed , y = resi_close_price)
# Comments:  QQ plot represent a time-series that is stationary

# Plotting the histogram of the residual time-series represents a Gaussian Curve

hist(resi_close_price, main = "Residual Series")


# Checking the properities of Residual time-series by inspection
plot(
  x = resi_close_price,
  type = "l",
  main = "Residual Time series",
  ylab = "",
  xlab = ""
)

# Comments: Thus, it is also clear from the plots ARIMA, ADF test, KPSS test the that the time series is stationary

#------------------------------------
# MODEL EVALUATION
#------------------------------------

# predicting the values fitted model from the vadlidation dataset ----

test_months <-
  as.data.frame((training_rows + 1):(training_rows + test_rows))
names(test_months) <- "months_smoothed"
head(test_months)

pred_close_price_train <-
  predict(object = fit.close_price, newdata = test_months)


# MAPE ERROR CALCULATION - ACCURACY PREDICTION

MAPE_close_price <-
  accuracy(pred_close_price_train, sp_test$close_price)[5]
MAPE_close_price

#COMMENTS: Thus with a MAPE error of just 1.0268, model is apparently a very good fit.

# combining the predicted values train and test data
sp_historical_cleaned$predicted_close_price <-
  c(fit.close_price$fitted.values, pred_close_price_train)

library(ggthemes)

# Plottig the actual vs the predicted data, using classical decomposition
ggplot(data = sp_historical_cleaned, aes(x = date_txn, y = close_price)) + geom_line() +
  geom_line(aes(x = date_txn, y = predicted_close_price, col = "green")) +
  geom_vline(
    xintercept = sp_historical_cleaned$date_txn[training_rows],
    show.legend = FALSE,
    linetype = "dashed"
  )  +
  labs(title = "Actual and Predicted Close Price - Decompostion") +
  scale_color_manual(labels = c("predicted"), values = c("blue")) +
  theme_economist() +
  theme(
    legend.position = c(0.5, 0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11)
  )


#------------------------------------------
# PREDICTION ANALYSIS USING AUTO.ARIMA
#------------------------------------------

plot(sp_historical_cleaned$close_price,
     type = "l",
     lwd = 2)

# Modelling auto arima
fit.autoarima.close_price <-
  auto.arima(sp_train$close_price,
             allowdrift = F,
             approximation = T)


summary(fit.autoarima.close_price)

close_price_autoarima_fitted <-  fit.autoarima.close_price$fitted

close_price_autoarima_fitted_test <-
  predict(fit.autoarima.close_price , n.ahead = 30)

lines(close_price_autoarima_fitted,
      type = "l",
      col = "red")

MAPE_close_price_arima <-
  accuracy(
    close_price_autoarima_fitted_test$pred,
    sp_test$close_price
  ) [5]
MAPE_close_price_arima

sp_historical_cleaned$arima_close_price <-
  c(close_price_autoarima_fitted,
    close_price_autoarima_fitted_test$pred)


#
ggplot(data = sp_historical_cleaned, aes(x = date_txn, y = close_price)) + geom_line() +
  geom_line(aes(x = date_txn, y = arima_close_price, col = "green")) +
  geom_vline(
    xintercept = sp_historical_cleaned$date_txn[training_rows],
    show.legend = FALSE,
    linetype = "dashed"
  )  +
  labs(title = "Actual Close Price and Predicted Close Price - ARIMA") +
  scale_color_manual(labels = c("predicted"), values = c("blue")) +
  theme_economist() +
  theme(
    legend.position = c(0.5, 0.9),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 10)
  )


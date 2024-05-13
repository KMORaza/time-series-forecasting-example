library(ISLR)     # used for dataset
library(tseries)  # used for time series analysis
library(forecast) # used for forecasting
library(ggplot2)  # used for visualization
data("Smarket") # Smarket dataset
smarket_ts <- ts(Smarket$Volume, start = c(2001, 1), frequency = 5)
autoplot(smarket_ts) + labs(title = "Smarket Volume Time Series")
decomp <- decompose(smarket_ts)
autoplot(decomp) + labs(title = "Decomposition of Smarket Volume Time Series")
adf.test(smarket_ts)
diff_smarket_ts <- diff(smarket_ts)
adf.test(diff_smarket_ts)
train <- window(diff_smarket_ts, end=c(2004, 1))
test <- window(diff_smarket_ts, start=c(2004, 2))
arima_model <- auto.arima(train)
forecast <- forecast(arima_model, h = length(test))
autoplot(forecast) + labs(title = "ARIMA Forecast for Smarket Volume")
accuracy(forecast, test)

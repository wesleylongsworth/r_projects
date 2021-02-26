library(forecast)
library(dplyr)
library(lubridate)

# =====================
#  Import raw data
# =====================

raw <- read.csv('data.csv', check.names = FALSE)
raw

#raw <- raw[c('Month','APJ-KOR')]
#colnames(raw)[colnames(raw)=="APJ-KOR"] <- "Count"
# =====================
#  Set input parameters
# =====================

# frequency of ts
freq = 12

# forecast length
forecast_length = 16

# manual HW parameters for override, provided by user
hw_alpha <- NULL
hw_beta <- NULL
hw_gamma <- NULL

# raw ts
raw_start = c(
              year(raw[which.min(as.POSIXct(raw$month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$month)), 1]))
raw_end = c(
            year(raw[which.max(as.POSIXct(raw$month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$month)), 1]))

# training set
in_start = c(
              year(raw[which.min(as.POSIXct(raw$month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$month)), 1]))
in_end = c(
            if_else(
              month(raw[which.max(as.POSIXct(raw$month)), 1]) <= 6, 
              year(raw[which.max(as.POSIXct(raw$month)), 1]) - 1, 
              year(raw[which.max(as.POSIXct(raw$month)), 1])
            ),
            if_else(
              month(raw[which.max(as.POSIXct(raw$month)), 1]) - 6 <= 0, 
              month(raw[which.max(as.POSIXct(raw$month)), 1]) - 6 + 12, 
              month(raw[which.max(as.POSIXct(raw$month)), 1]) - 6
            )
          )

# test set
out_start = c(
              if_else(
                month(raw[which.max(as.POSIXct(raw$month)), 1]) <= 5, 
                year(raw[which.max(as.POSIXct(raw$month)), 1]) - 1, 
                year(raw[which.max(as.POSIXct(raw$month)), 1])
              ),
              if_else(
                month(raw[which.max(as.POSIXct(raw$month)), 1]) - 5 <= 0, 
                month(raw[which.max(as.POSIXct(raw$month)), 1]) - 5 + 12, 
                month(raw[which.max(as.POSIXct(raw$month)), 1]) - 5
              )
             )
out_end = c(
            year(raw[which.max(as.POSIXct(raw$month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$month)), 1]))

# =====================
#  Create master time
#  series object
# =====================

ts <- ts(raw$respondents, start = raw_start, end = raw_end, frequency = freq)

ts

#png("Plot3.png", width = 7, height = 4, units = 'in', res = 1000)
plot(ts, ylab = "Value", main = "Overall Trend")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(ts, pch = 1)
abline(reg = lm(ts ~ time(ts)), col = 'red')
#dev.off()

# =======================
# Seasonality
# =======================

boxplot(ts ~ cycle(ts), col = "#0cd5c2", 
        main = "Seasonality\n",
        boxwex = 0.7,
        staplelty = 0,
        axes = FALSE)
axis(1, at=1:12, labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))

# =======================
# Create in/out samples
# =======================

# MR data for test
train <- ts(raw$respondents, start = in_start, end = in_end, frequency = freq)
test <- window(ts, start = out_start, end = out_end, frequency = freq)

fit <- HoltWinters(train, seasonal = 'multiplicative')

forecast <- forecast(fit, h = 6, level = c(80,95))
#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Test Predictions\nHolt-Winters")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(train, pch = 1)
points(forecast$mean, pch = 1)
points(test, pch = 3)
abline(reg = lm(ts ~ time(ts)), col = 'red')

out <- as.vector(test)
pred <- as.vector(forecast$mean)
diff <- pred - out
percent.diff <- round((abs(diff)/out)*100,digits=1)

t1 <- data.frame(out,pred,diff,percent.diff)
t1


# ==============================
# Forecasting with trained model
# ==============================

fit <- HoltWinters(ts, 
                   alpha = if_else(is.null(hw_alpha), fit$alpha, hw_alpha), 
                   beta = if_else(is.null(hw_beta), fit$beta, hw_beta), 
                   gamma = if_else(is.null(hw_gamma), fit$gamma, hw_gamma),
                   seasonal = 'multiplicative')

forecast <- forecast(fit, h = forecast_length, level = c(80,95))

#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Trend Prediction\n")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(ts, pch = 1)
points(forecast$mean, pch = 1)
abline(reg = lm(ts ~ time(ts)), col = 'red')


output <- data.frame(forecast$lower, forecast$mean, forecast$upper)
clean_output <- data.frame(Month = seq(as.Date(raw[which.max(as.POSIXct(raw$month)), 1]) %m+% months(1), by = "month", length.out = forecast_length),
                           Count = round(forecast$mean),
                           Upper = round(forecast$upper),
                           Lower = round(forecast$lower))
clean_output

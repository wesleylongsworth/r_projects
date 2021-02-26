library(forecast)
library(lubridate)
library(dplyr)

# ==========================================================================
#  Model requires two-column csv with Month (ISO format) and Count (integer)
# ==========================================================================

raw <- read.csv('your_file.csv')

# =====================
#  Set input parameters
# =====================

# frequency of ts, provided by user
freq = 12

# forecast length, provided by user
forecast_length = 6

# manual HW parameters for override, provided by user
hw_alpha <- NULL
hw_beta <- NULL
hw_gamma <- NULL

# raw ts
raw_start = c(
              year(raw[which.min(as.POSIXct(raw$Month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$Month)), 1]))
raw_end = c(
            year(raw[which.max(as.POSIXct(raw$Month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$Month)), 1]))

# training set
in_start = c(
              year(raw[which.min(as.POSIXct(raw$Month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$Month)), 1]))
in_end = c(
            if_else(
                    month(raw[which.max(as.POSIXct(raw$Month)), 1]) <= 6, 
                    year(raw[which.max(as.POSIXct(raw$Month)), 1]) - 1, 
                    year(raw[which.max(as.POSIXct(raw$Month)), 1])
            ),
            if_else(
              month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 6 <= 0, 
              month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 6 + 12, 
              month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 6
            )
          )
  
# test set
out_start = c(
              if_else(
                month(raw[which.max(as.POSIXct(raw$Month)), 1]) <= 5, 
                year(raw[which.max(as.POSIXct(raw$Month)), 1]) - 1, 
                year(raw[which.max(as.POSIXct(raw$Month)), 1])
              ),
              if_else(
                month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 5 <= 0, 
                month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 5 + 12, 
                month(raw[which.max(as.POSIXct(raw$Month)), 1]) - 5
              )
            )
out_end = c(
            year(raw[which.max(as.POSIXct(raw$Month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$Month)), 1]))


# =====================
#  Create master time
#  series object
# =====================

ts <- ts(raw$Count, 
         start = raw_start, 
         end = raw_end, 
         frequency = freq)

# =======================
# Create in/out samples
# =======================

# MR data for training
train <- ts(raw$Count, start = in_start, end = in_end, frequency = freq)
test <- window(ts, start = out_start, end = out_end, frequency = freq)

# =======================
# Train model and add 
# relevant parameters to 
# master forecast
# =======================
fit <- HoltWinters(train)

fit2 <- HoltWinters(ts, 
                    alpha = if_else(is.null(hw_alpha), fit$alpha, hw_alpha), 
                    beta = if_else(is.null(hw_beta), fit$beta, hw_beta), 
                    gamma = if_else(is.null(hw_gamma), fit$gamma, hw_gamma))

forecast <- forecast(fit2, h = forecast_length, level = c(80,95))

# =======================
# Print forecast results 
# =======================
output <- data.frame(Month = seq(as.Date(raw[which.max(as.POSIXct(raw$Month)), 1]) %m+% months(1), by = "month", length.out = forecast_length),
                          Count = round(forecast$mean),
                          Upper = round(forecast$upper),
                          Lower = round(forecast$lower), row.names = NULL)

names(output) <- c("month","count","upper_80","upper_95","lower_80","lower_95")
file <- write.csv(output, file = "hw_forecast_outputs.csv", row.names = FALSE)

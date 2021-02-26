library(forecast)
library(dplyr)
library(lubridate)

# sample data
raw <- read.csv('hw_input.csv', check.names = FALSE)

# ===============================================
# inputs
# ===============================================

# frequency of ts
freq = 12

# forecast length
forecast_length = 6

# manual HW parameters for override, provided by user
hw_alpha <- NULL
hw_beta <- NULL
hw_gamma <- NULL

# raw ts
raw_start = c(year(raw[which.min(as.POSIXct(raw$Month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$Month)), 1]))

raw_end = c(year(raw[which.max(as.POSIXct(raw$Month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$Month)), 1]))

# training set
in_start = c(year(raw[which.min(as.POSIXct(raw$Month)), 1]), 
              month(raw[which.min(as.POSIXct(raw$Month)), 1]))

in_end = c(if_else(
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
out_start = c(if_else(
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
out_end = c(year(raw[which.max(as.POSIXct(raw$Month)), 1]), 
            month(raw[which.max(as.POSIXct(raw$Month)), 1]))

# ===============================
# loop column splice and forecast
# ===============================
trim <- subset(raw, select = -c(Month))

elements <- list()
for(i in colnames(trim))
{ #create individual raw
  Month <- raw$Month
  Count <- trim[[i]]
  elements[[i]] <- data.frame(Month,Count)
}

forecasts <- list()
for(i in names(elements))
{
  # ===============================
  # create master/train ts datasets
  # ===============================
  ts <- ts(elements[[i]]$Count, start = raw_start, end = raw_end, frequency = freq) 
  train <- ts(elements[[i]]$Count, start = in_start, end = in_end, frequency = freq)

  # ===============================================
  # derive model structure and pass to master model
  # ===============================================    
  fit <- withCallingHandlers(fit <- HoltWinters(train, seasonal = 'multiplicative'), #handle optimization errors
                      error = function(condition){
                        HoltWinters(train, seasonal = 'additive')
                      },
                      warning = function(condition){
                        HoltWinters(train, seasonal = 'additive')
                      },
                      message = function(condition){
                        HoltWinters(train, seasonal = 'additive')
                      },
                      finally = {
                        message("model complete")
                      }
                  )
  
  if(fit$seasonal == 'multiplicative' & fit$alpha == 0){ #run when multiplicative alpha = 0
    fit_add <- HoltWinters(train) #what to do when alpha is 0 for both additive and multiplicative
    
    } else {
      fit_add <- NULL
    } 
  
  alpha_for <- ifelse(is.null(hw_alpha) & 
                   fit$alpha != 0, fit$alpha,
                 ifelse(is.null(hw_alpha) & 
                          fit$alpha == 0 &
                          fit_add$alpha == 0, fit$alpha,
                        ifelse(is.null(hw_alpha) &
                                 fit$alpha == 0 &
                                 fit$seasonal == 'multiplicative' &
                                 fit_add$alpha != 0, fit_add$alpha,
                               hw_alpha)))
  
  beta_for <-  ifelse(is.null(hw_beta) & 
                   fit$alpha != 0, fit$beta,
                 ifelse(is.null(hw_beta) & 
                          fit$alpha == 0 &
                          fit_add$alpha == 0, fit$beta,
                        ifelse(is.null(hw_beta) &
                                 fit$alpha == 0 &
                                 fit$seasonal == 'multiplicative' &
                                 fit_add$alpha != 0, fit_add$beta,
                               hw_beta)))
  
  gamma_for <- ifelse(is.null(hw_gamma) & 
                   fit$alpha != 0, fit$gamma,
                 ifelse(is.null(hw_gamma) & 
                          fit$alpha == 0 &
                          fit_add$alpha == 0, fit$gamma,
                        ifelse(is.null(hw_gamma) &
                                 fit$alpha == 0 &
                                 fit$seasonal == 'multiplicative' &
                                 fit_add$alpha != 0, fit_add$gamma,
                               hw_gamma)))
  
  seasonal_for <- ifelse(is.null(hw_alpha) & 
                      fit$alpha != 0,fit$seasonal,
                    ifelse(is.null(hw_alpha) & 
                             fit$alpha == 0 &
                             fit_add$alpha == 0, 'multiplicative',
                           ifelse(is.null(hw_alpha) & 
                                    fit$alpha == 0 & 
                                    fit$seasonal == 'multiplicative' &
                                    fit_add$alpha != 0 ,'additive', 
                                  'multiplicative')))
  
  if(alpha_for == 0 & seasonal_for == 'multiplicative'){
    fit_for <- HoltWinters(train, seasonal = 'multiplicative')
  } else {
    fit_for <- HoltWinters(train, 
                           alpha = as.numeric(alpha_for), 
                           beta = as.numeric(beta_for), 
                           gamma = as.numeric(gamma_for), 
                           seasonal = as.character(seasonal_for))
  }

  # ===============
  # create forecast
  # ===============
  forecast <- forecast(fit_for, h = forecast_length, level = c(80,95), seasonal = fit_for$seasonal) #forecast
  
  # ====================
  # print outputs to df
  # ====================
  Month <- seq(as.Date(elements[[i]][which.max(as.POSIXct(elements[[i]]$Month)), 1]) %m+% months(1), by = "month", length.out = forecast_length)
  Count <- as.vector(round(forecast$mean))
  Upper <- data.frame(unlist(round(forecast$upper)))
    colnames(Upper)[colnames(Upper)=="X80."] <- "Upper_80"
    colnames(Upper)[colnames(Upper)=="X95."] <- "Upper_95"
  Lower <- data.frame(unlist(round(forecast$lower)))
    colnames(Lower)[colnames(Lower)=="X80."] <- "Lower_80"
    colnames(Lower)[colnames(Lower)=="X95."] <- "Lower_95"
  
  results <- data.frame(Month,Lower$Lower_95,Lower$Lower_80,Count,Upper$Upper_80,Upper$Upper_95)
  names(results) <- c("month","lower_95","lower_80","forecast","upper_80","upper_95")
  forecasts[[i]] <- results

}

# ================================================
# export forecast results to csv for each element
# ================================================
for(i in names(forecasts))
{
  file <- write.csv(forecasts[[i]], 
                    file = paste(i, "_hw_forecast_outputs.csv",sep = ""), 
                    row.names = FALSE)
}

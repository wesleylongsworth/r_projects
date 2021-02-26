library(lubridate)
library(dplyr)
library(forecast)
library(ggplot2)


data <- read.csv('data123.csv')
View(data)
str(data)

### feature tuning
data$Created.Date <- as.Date(data$Start.Date, format = '%m/%d/%Y')
data$Start.Date <- as.Date(data$Start.Date, format = '%m/%d/%Y')
data$Response.Day <- as.Date(data$Response.Day,format = '%m/%d/%Y')
data$Response.Month <- data$Response.Day
day(data$Response.Month) <- 1
str(data)

### imputation

data$bu_flag <- factor(if_else(
                          year(data$Start.Date) < year(data$Response.Day) & 
                            month(data$Start.Date) == 12 & 
                            month(data$Response.Day) == 1, 
                          'Uplift',
                            ifelse(
                              year(data$Created.Date) < year(data$Start.Date) &
                                year(data$Response.Day) < year(data$Start.Date) |
                              year(data$Created.Date) == year(data$Start.Date) &
                                year(data$Response.Day) == year(data$Start.Date) &
                                month(data$Created.Date) <= month(data$Start.Date) & 
                                month(data$Response.Day) <= month(data$Start.Date) + 1,
                              'Uplift','Baseline'
                                )
                            
                            )
)
                       

### subsetting

# isolate trials
responses_notrials <- subset(data, Language != '', select = Campaign.ID:bu_flag)
responses_trials <- subset(data, Language == '', select = Campaign.ID:bu_flag)

# isolate local vs non-local responses            
local_base <- subset(responses_notrials, Language != 'English' & bu_flag == 'Baseline', select = Campaign.ID:bu_flag)
local_up <- subset(responses_notrials, Language != 'English' & bu_flag == 'Uplift', select = Campaign.ID:bu_flag)
  
nonlocal_base <- subset(responses_notrials, Language == 'English' & bu_flag == 'Baseline', select = Campaign.ID:bu_flag)
nonlocal_up <- subset(responses_notrials, Language == 'English' & bu_flag == 'Uplift', select = Campaign.ID:bu_flag)


### rollup and convert to time series


lb_df <- as.data.frame(table(local_base$Response.Month, local_base$bu_flag))
lb_df <- subset(lb_df, Var2 =='Baseline', select = c('Var1','Freq'))
colnames(lb_df)[colnames(lb_df)=='Var1'] <- 'Month'
colnames(lb_df)[colnames(lb_df)=='Freq'] <- 'Baseline_Local'
lb_df$Month <- as.POSIXct(lb_df$Month, format = '%Y-%m-%d')

lu_df <- as.data.frame(table(local_up$Response.Month, local_up$bu_flag))
lu_df <- subset(lu_df, Var2 =='Uplift', select = c('Var1','Freq'))
colnames(lu_df)[colnames(lu_df)=='Var1'] <- 'Month'
colnames(lu_df)[colnames(lu_df)=='Freq'] <- 'Uplift_Local'
lu_df$Month <- as.POSIXct(lu_df$Month, format = '%Y-%m-%d')

nb_df <- as.data.frame(table(nonlocal_base$Response.Month, nonlocal_base$bu_flag))
nb_df <- subset(nb_df, Var2 =='Baseline', select = c('Var1','Freq'))
colnames(nb_df)[colnames(nb_df)=='Var1'] <- 'Month'
colnames(nb_df)[colnames(nb_df)=='Freq'] <- 'Baseline_English'
nb_df$Month <- as.POSIXct(nb_df$Month, format = '%Y-%m-%d')


nu_df <- as.data.frame(table(nonlocal_up$Response.Month, nonlocal_up$bu_flag))
nu_df <- subset(nu_df, Var2 =='Uplift', select = c('Var1','Freq'))
colnames(nu_df)[colnames(nu_df)=='Var1'] <- 'Month'
colnames(nu_df)[colnames(nu_df)=='Freq'] <- 'Uplift_English'
nu_df$Month <- as.POSIXct(nu_df$Month, format = '%Y-%m-%d')


### impute uplift with months where MR = 0

ts <- seq.POSIXt(as.POSIXct("2015-6-01 0:00",'%Y-%m-%d %H:%M'), as.POSIXct("2019-05-01 0:00",'%Y-%m-%d %H:%M'), by="month")

ts <- seq.POSIXt(as.POSIXlt("2015-6-01"), as.POSIXlt("2019-05-01"), by="month")
ts <- format.POSIXct(ts,'%Y-%m-%d')

df <- data.frame(timestamp=ts)

df$Month<-as.POSIXct(df$timestamp,format="%Y-%m-%d")

# impute local
lu_df <- full_join(df,lu_df)
lu_df$timestamp <- NULL

lu_df <- lu_df %>% group_by(Month) %>% mutate_each(funs(ifelse(is.na(.),0,.)))
lu_df <- as.data.frame(lu_df)

lb_df <- full_join(df,lb_df)
lb_df$timestamp <- NULL

lb_df <- lb_df %>% group_by(Month) %>% mutate_each(funs(ifelse(is.na(.),0,.)))
lb_df <- as.data.frame(lb_df)

#impute non-local

nu_df <- full_join(df, nu_df)
nu_df$timestamp <- NULL

nu_df <- nu_df %>% group_by(Month) %>% mutate_each(funs(ifelse(is.na(.),0,.)))
nu_df <- as.data.frame(nu_df)




# =================================
# Exploratory Time Series Analysis
# =================================

# overlap individual time series

lb_ts <- ts(lb_df$Baseline_Local, start = c(2015, 6), end = c(2019, 5), frequency = 12)
lu_ts <- ts(lu_df$Uplift_Local, start = c(2015, 6), end = c(2019, 5), frequency = 12)
nb_ts <- ts(nb_df$Baseline_English, start = c(2015, 6), end = c(2019, 5), frequency = 12)
nu_ts <- ts(nu_df$Uplift_English, start = c(2015, 6), end = c(2019, 5), frequency = 12)

# local <- full_join(lb_df,lu_df)
# english <- full_join(nb_df, nu_df)

ts.plot(lb_ts, lu_ts, nb_ts, nu_ts, 
        col = c('red', 'pink', 'black', 'gray'), 
        main = 'Local vs Non-local Language Response Trends',
        ylab = 'Responses',
        lwd=1,
        gpars=list(xaxt="n"))
axis(1, at=seq(2015-.25,2019,.25),labels=NA) 
axis(1, at=seq(2015-.25,2019,.25),labels = seq(2015-.25,2019,.25),lwd.ticks=1.2) 
points(lb_ts, pch = 1, col = 'red')
points(lu_ts, pch = 1, col = 'pink')
points(nb_ts, pch = 1, col = 'black')
points(nu_ts, pch = 1, col = 'gray')
legend("topleft", 
       legend=c("English Baseline", "English Uplift", "Local Baseline","Local Uplift"), 
       text.col=c('black','gray','red','pink'), 
       ncol = 2,
       cex = 0.75)

# =================================
#  Time Series Forecasting
# =================================

boxplot(lb_ts ~ cycle(lb_ts), col = 'mistyrose', 
        main = "Local Language Baseline Seasonality",
        boxwex = 0.7,
        staplelty = 0)

boxplot(lu_ts ~ cycle(lu_ts), col = 'mistyrose', 
        main = "Local Language Uplift Seasonality",
        boxwex = 0.7,
        staplelty = 0)

boxplot(nb_ts ~ cycle(nb_ts), col = 'mistyrose', 
        main = "Non-local Language Baseline Seasonality",
        boxwex = 0.7,
        staplelty = 0)

boxplot(nu_ts ~ cycle(nu_ts), col = 'mistyrose', 
        main = "Non-local Language Uplift Seasonality",
        boxwex = 0.7,
        staplelty = 0)

# ============================================================
# Create in/out sample & train model for non-local baseline
# ============================================================

# MR data for test
sample_in_nonlocal <- ts(nb_df$Baseline_English, start = c(2015, 6), end = c(2018, 11), frequency = 12)
sample_out_nonlocal <- window(nb_ts, start = c(2018, 12), end = c(2019, 5), frequency = 12)

fit <- HoltWinters(sample_in_nonlocal, alpha = 0.2, beta = 0.2, gamma = 0.7, seasonal = "multiplicative")
fit


forecast <- forecast(fit, h = 6, level = c(80,95))
#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Test of Non-local Baseline Forecast Model\nHolt-Winters")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(sample_in_nonlocal, pch = 1)
points(forecast$mean, pch = 1)
points(sample_out_nonlocal, pch = 3)
abline(reg = lm(nb_ts ~ time(nb_ts)), col = 'red')

out <- as.vector(sample_out_nonlocal)
pred <- as.vector(forecast$mean)
diff <- pred - out
percent.diff <- round((abs(diff)/out)*100,digits=1)

t1 <- data.frame(out,pred,diff,percent.diff)
t1


# =======================
# Forecast using Model
# =======================

fit <- HoltWinters(nb_ts, alpha = 0.4709647, beta = 0, gamma = 0.1511993)

summary(fit)

forecast <- forecast(fit, h = 8, level = c(80,95))

#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Non-local Baseline Forecast (c. 80/95)")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(nb_ts, pch = 1)
points(forecast$mean, pch = 1)
abline(reg = lm(nb_ts ~ time(nb_ts)), col = 'red')


t2 <- data.frame(forecast$lower, forecast$mean, forecast$upper)
t2



# =====================================================
# Derive uplift levels - avg uplift % by campaign type
# =====================================================

# group by campaign

View(nonlocal_up)
a <- nonlocal_up %>% group_by(Campaign.ID, Campaign.Type, Response.Month) %>% summarize(count = n()) %>% arrange(desc(count)) 
a_df <- as.data.frame(a)
colnames(a_df)[colnames(a_df)=='count'] <- 'Uplift'

# join with baseline to pull in monthly baseline
b <- nonlocal_base %>% group_by(Response.Month) %>% summarize(count = n()) %>% arrange(Response.Month)
b_df <- as.data.frame(b)
colnames(b_df)[colnames(b_df)=='count'] <- 'Baseline'

c <- full_join(a_df,b_df)

# calculate baseline contribution at the campaign level

c$Uplift.Ratio <- (c$Uplift / c$Baseline)*100

# visualize uplift ratio distribution

boxplot(Uplift.Ratio ~ Campaign.Type, data = c,
        col = 'mistyrose', 
        main = "Non-local Uplift Contribution by Campaign Type",
        xlab = "Uplift Ration: % of Baseline",
        boxwex = 0.7,
        staplelty = 0,
        las = 2,
        horizontal = TRUE)

# calc and visualize average % uplift for each campaign type

mean_Uplift.Ratio <- c %>% group_by(Campaign.Type) %>% summarize(mean = mean(Uplift.Ratio)) %>% arrange(desc(mean))
View(mean_Uplift.Ratio)

d <- full_join(mean_Uplift.Ratio,c)
d <- as.data.frame(d)

ggplot(d, aes(x = Campaign.Type, y = Uplift.Ratio)) + 
  geom_point(alpha = .2) +
  geom_point(aes(x = Campaign.Type, y = mean), color = 'red', shape = 4, stroke = 1.5) +
  coord_flip() +
  geom_jitter(alpha = .2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Non-local Uplift Contribution by Campaign Type') +
  ylab('Uplift Ratio: % of Baseline')

View(mean_Uplift.Ratio)



# ============================================================
# Create in/out sample & train model for local baseline
# ============================================================

# MR data for test
sample_in_local <- ts(lb_df$Baseline_Local, start = c(2015, 6), end = c(2018, 11), frequency = 12)
sample_out_local <- window(lb_ts, start = c(2018, 12), end = c(2019, 5), frequency = 12)

fit <- HoltWinters(sample_in_local, alpha = 0.7, beta = 0.003861955, gamma = 0.8)
fit

forecast <- forecast(fit, h = 6, level = c(80,95))
#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Test of Local Baseline Forecast Model\nHolt-Winters")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(sample_in_local, pch = 1)
points(forecast$mean, pch = 1)
points(sample_out_local, pch = 3)
abline(reg = lm(lb_ts ~ time(lb_ts)), col = 'red')

out <- as.vector(sample_out_local)
pred <- as.vector(forecast$mean)
diff <- pred - out
percent.diff <- round((abs(diff)/out)*100,digits=1)

t1 <- data.frame(out,pred,diff,percent.diff)
t1


# =======================
# Forecast using Model
# =======================

fit <- HoltWinters(lb_ts, alpha = 0.7, beta = 0.003861955, gamma = 0.8)
fit

forecast <- forecast(fit, h = 8, level = c(80,95))

#forecast$mean<-exp(forecast$mean)
#forecast$upper<-exp(forecast$upper)
#forecast$lower<-exp(forecast$lower)
#forecast$x<-exp(forecast$x)
plot(forecast, main = "Local Baseline Forecast (c. 80/95)")
grid (NULL,NULL, lty = 1, col = "light gray") 
points(lb_ts, pch = 1)
points(forecast$mean, pch = 1)
abline(reg = lm(lb_ts ~ time(lb_ts)), col = 'red')


t2 <- data.frame(forecast$lower, forecast$mean, forecast$upper)
t2



# =====================================================
# Derive uplift levels - avg uplift % by campaign type
# =====================================================


View(local_up)
a <- local_up %>% group_by(Campaign.ID, Campaign.Type, Response.Month) %>% summarize(count = n()) %>% arrange(desc(count)) 
a_df <- as.data.frame(a)
colnames(a_df)[colnames(a_df)=='count'] <- 'Uplift'

# join with baseline table to bring in Baseline for time period under campaign uplift
b <- local_base %>% group_by(Response.Month) %>% summarize(count = n()) %>% arrange(Response.Month)
b_df <- as.data.frame(b)
colnames(b_df)[colnames(b_df)=='count'] <- 'Baseline'

c <- full_join(a_df,b_df)
c_noNA <- subset(c, !is.na(Baseline), select = Campaign.ID:Baseline)

# divide uplift in month by baseline in-month to derive % of baseline boosted by each campaign

c_noNA$Uplift.Ratio <- (c_noNA$Uplift / c_noNA$Baseline)*100

# visualize uplift ratio distribution

boxplot(Uplift.Ratio ~ Campaign.Type, data = c_noNA,
        col = 'mistyrose', 
        main = "Local Uplift Contribution by Campaign Type",
        xlab = "Uplift Ration: % of Baseline",
        boxwex = 0.7,
        staplelty = 0,
        las = 2,
        horizontal = TRUE)

# calc and visualize average % uplift for each campaign type

mean_Uplift.Ratio <- c_noNA %>% group_by(Campaign.Type) %>% summarize(mean = mean(Uplift.Ratio)) %>% arrange(desc(mean))
mean_Uplift.Ratio


d <- full_join(mean_Uplift.Ratio,c_noNA)
d <- as.data.frame(d)



ggplot(d, aes(x = Campaign.Type, y = Uplift.Ratio)) + 
  geom_point(alpha = .2) +
  geom_point(aes(x = Campaign.Type, y = mean), color = 'red', shape = 4, stroke = 1.5) +
  coord_flip() +
  geom_jitter(alpha = .2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Local Uplift Contribution by Campaign Type') +
  ylab('Uplift Ratio: % of Baseline')

View(mean_Uplift.Ratio)

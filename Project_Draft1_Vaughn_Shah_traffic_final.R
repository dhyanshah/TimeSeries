rm(list = ls(all.names = TRUE))

sessionInfo() # Dhyan 
#R version 3.5.2 (2018-12-20)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.15.3


library(tswge)
library(ggplot2)
library(ggthemes)
library(forecast)
library(tseries)
library(lubridate)
library(datetime)

library(caret)
library(corrplot)
library(DMwR)
library(Hmisc)
library(ROCR)
library(stringr)
library(RVAideMemoire)

install.packages("greybox")
install.packages("nnfor")
library(nnfor)

traffic = read.csv(file.choose(),header = TRUE)

################################
### EDA - DATA PREPARATION
################################


# Take a peek at the dataframe after the import
head(traffic)
str(traffic)

# Check for missing values
sum(is.na(traffic))

#summary statistics
summary(traffic)

# Create new columns from the date_time attribute
traffic$date_time <- strptime(traffic$date_time, "%m/%d/%y %H:%M")
traffic$date_month <- months(traffic$date_time)
traffic$date_month <- as.factor(traffic$date_month)
traffic$date_weekday <- weekdays(traffic$date_time)
traffic$date_weekday <- as.factor(traffic$date_weekday)
traffic$date_hour <- strftime(traffic$date_time, "%H")
traffic$date_hour <- as.factor(traffic$date_hour)
traffic$traffic_volume <- traffic$traffic_volume

# Re-engineer the weather related attributes
# Rescale the temperature in Kelvin to Celcius & correct some zero degree Kelvin entries 
mean_temp = mean(traffic$temp)
traffic$temp[traffic$temp == 0] <- mean_temp
traffic$temp = traffic$temp - 272.15

# Reset one row with rainfall of 9831.3 mm to the max of 55.63
max_rain = 55.63
traffic$rain_1h[traffic$rain_1h > 100] <- max_rain

# Drop un-needed features
traffic$date_time <- NULL
traffic$weather_description <- NULL


#view classes and missing values
sapply(traffic, class)
sapply(traffic, function(x) sum(is.na(x)))


# Use variable totCol to hold the number of columns in the dataframe
totCol <- ncol(traffic)
targetCol <- totCol
# Set up variable totAttr for the total number of attribute columns
totAttr <- totCol-1


################################
### EDA - GRAPHICS
################################

# Boxplots for each attribute
par(mfrow=c(2,2))
boxplot(traffic[,3], main=names(traffic)[3])
boxplot(traffic[,4], main=names(traffic)[4])
boxplot(traffic[,5], main=names(traffic)[5])
boxplot(traffic[,6], main=names(traffic)[6])
 
# Histograms each numeric attribute
par(mfrow=c(2,2))
hist(traffic[,3], main=names(traffic)[3], xlab = names(traffic)[3])
hist(traffic[,4], main=names(traffic)[4], xlab = names(traffic)[4])
hist(traffic[,5], main=names(traffic)[5], xlab = names(traffic)[5])
hist(traffic[,6], main=names(traffic)[6], xlab = names(traffic)[6])

# Histograms each factor attributes by Target (traffic volume)
par(mfrow=c(3,2))
byf.hist(traffic_volume~holiday, data=traffic)
byf.hist(traffic_volume~weather_main, data=traffic)
byf.hist(traffic_volume~date_month, data=traffic)
byf.hist(traffic_volume~date_weekday, data=traffic)
byf.hist(traffic_volume~date_hour, data=traffic)

# Density plot for each attribute
par(mfrow=c(2,2))
plot(density(traffic[,3]), main=names(traffic)[3])
plot(density(traffic[,4]), main=names(traffic)[4])
plot(density(traffic[,5]), main=names(traffic)[5])
plot(density(traffic[,6]), main=names(traffic)[6])

# Correlation matrix
par(mfrow=c(1,1))
correlations <- cor(traffic[,c(1, 3:6)])
corrplot(correlations, type="upper", order="hclust", tl.col="black", tl.srt=45)



################################
### EDA - TIME SERIES
################################

#all data
plotts.sample.wge(traffic$traffic_volume)
#truncated to last year
plotts.sample.wge(traffic$traffic_volume[39444:48204])
#truncated to 6 months 
plotts.sample.wge(traffic$traffic_volume[43824:48204])
#truncated to 3 months 
plotts.sample.wge(traffic$traffic_volume[46014:48204])
#truncated to 2 months 
plotts.sample.wge(traffic$traffic_volume[46744:48204])
#truncated to 1 month
plotts.sample.wge(traffic$traffic_volume[47474:48204])

length(traffic$traffic_volume[39444:48204]) 

acf(traffic$traffic_volume[1:24102])
acf(traffic$traffic_volume[24102:48204])

################################
### MODELING - TIME SERIES
################################

#1 year = 8760 if we want to look back that far
#looking at 2 months BIC
aic5.wge(traffic$traffic_volume[46744:48204],p=0:15,q=0:3,type = 'bic')
  #Five Smallest Values of  bic 
  #      p    q        bic
  #14    3    1   12.76788
  #43   10    2   12.77147
  #41   10    0   12.77183
  #45   11    0   12.77235
  #18    4    1   12.77284

#1 year = 8760 BIC
aic5.wge(traffic$traffic_volume[39444:48204],p=0:15,q=0:5,type = 'bic')
  #Five Smallest Values of  bic 
  #     p    q        bic
  #47   11    2   12.69571
  #43   10    2   12.69651
  #51   12    2   12.69674
  #48   11    3   12.69677
  #59   14    2   12.69695

#2months AIC
aic5.wge(traffic$traffic_volume[46744:48204],p=0:20,q=0:3,type = 'aic')
  #  Five Smallest Values of  aic 
  #       p    q        aic
  #  72   17    3   12.74404
  #  84   20    3   12.74782
  #  76   18    3   12.74886
  #  70   17    1   12.75327
  #  78   19    1   12.75377

#1 year = 8760 AIC
aic5.wge(traffic$traffic_volume[39444:48204],p=0:20,q=0:3,type = 'aic')
  #Five Smallest Values of  aic 
  #     p    q        aic
  #84   20    3   12.64233
  #76   18    3   12.64665
  #79   19    2   12.65141
  #82   20    1   12.65251
  #78   19    1   12.65544

#Fit the model using your model identification (p and q). For
#For the last 2 month of data
x=traffic$traffic_volume[46744:48204]
end(x)

s31=est.arma.wge(x,p=3, q=1)
  #Coefficients of Original polynomial:  
  #  2.2137 -1.5760 0.3391 
  #
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.8175B+0.8558B^2    1.0619+-0.2024i      0.9251       0.0300
  #1-0.3963B              2.5236               0.3963       0.0000

s31$phi
  #[1]  2.2137351 -1.5759921  0.3391203

s31$theta
  #[1] 0.874308

s31$avar
  #[1] 342129.2

mean(x)
  #[1] 3364.218

# Next We Examine the Residuals
# s23$res: COntains residuals from the ARMA(2,3) fit

plotts.sample.wge(s31$res)
acf(s31$res)


# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting Last 24 observations
s31.for = fore.arma.wge(x[461:1461],phi=s31$phi,n.ahead=24,
                          lastn=FALSE,limits=FALSE) 


# Calculating ASE for Last 24 observations
n = 24
x.pred = s31.for$f
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))

# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting 24 observations forward
s31.for = fore.arma.wge(x,phi=s31$phi,n.ahead=24,
                        lastn=FALSE,limits=FALSE)


######################################################
# Fittiing a seasonal model to the data
# Find the ASE for this model using the data
# Looking at Realization, it appears that the data has
# a seasonal component.

# overfit to identify seasonality  
est.ar.wge(x,p=18,type = 'burg')

  #Coefficients of Original polynomial:  
  #  1.2939 -0.2981 -0.1099 0.0067 -0.0406 0.0386 -0.0215 -0.0377 0.1446 -0.0738 -0.1334 0.0072 0.0569 0.0601 0.0113 -0.1665 0.0344 0.0505 
  
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.8737B+0.9187B^2    1.0197+-0.2206i      0.9585       0.0339
  #1-0.4996B+0.8780B^2    0.2845+-1.0286i      0.9370       0.2071
  #1-1.2078B+0.8159B^2    0.7402+-0.8233i      0.9033       0.1334
  #1-1.5281B+0.7916B^2    0.9652+-0.5759i      0.8897       0.0856
  #1+0.8482B+0.7595B^2   -0.5584+-1.0024i      0.8715       0.3309
  #1+0.2886B+0.7442B^2   -0.1939+-1.1429i      0.8627       0.2767
  #1+1.3796B+0.7044B^2   -0.9793+-0.6787i      0.8393       0.4035
  #1+1.6111B+0.6822B^2   -1.1808+-0.2674i      0.8260       0.4646
  #1-0.7734B              1.2930               0.7734       0.0000
  #1+0.4612B             -2.1684               0.4612       0.5000

factor.wge(phi = c(rep(0,11),1))

  #Coefficients of Original polynomial:  
  #  0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
  #
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
  #1-1.0000B              1.0000               1.0000       0.0000
  #1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
  #1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
  #1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
  #1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
  #1+1.0000B             -1.0000               1.0000       0.5000

#This looks like a s=24 cycle. The factor table supports this. 
factor.wge(phi = c(rep(0,23),1))

  #Coefficients of Original polynomial:  
  #  0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
  
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.4142B+1.0000B^2    0.7071+-0.7071i      1.0000       0.1250
  #1-0.5176B+1.0000B^2    0.2588+-0.9659i      1.0000       0.2083
  #1-1.9319B+1.0000B^2    0.9659+-0.2588i      1.0000       0.0417
  #1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
  #1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
  #1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
  #1-1.0000B              1.0000               1.0000       0.0000
  #1+0.5176B+1.0000B^2   -0.2588+-0.9659i      1.0000       0.2917
  #1+1.4142B+1.0000B^2   -0.7071+-0.7071i      1.0000       0.3750
  #1+0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
  #1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
  #1+1.9319B+1.0000B^2   -0.9659+-0.2588i      1.0000       0.4583
  #1+1.0000B             -1.0000               1.0000       0.5000


#difference the data s=24, s=168, s=730 (hourly, weekly, monthly)
#For the last 2 month of data
#truncated to 2 months 

#7 day
plotts.sample.wge(traffic$traffic_volume[48036:48204])
#2months
plotts.sample.wge(traffic$traffic_volume[46744:48204])

#Monthly
x=artrans.wge(traffic$traffic_volume,phi.tr = c(rep(0,729),1))
plotts.sample.wge(tail(x,n=168))
plotts.sample.wge(tail(x,n=1460))

#7 days
x=artrans.wge(x,phi.tr = c(rep(0,167),1))
plotts.sample.wge(tail(x,n=168))
plotts.sample.wge(tail(x,n=1460))

#48 hours
x=artrans.wge(x,phi.tr = c(rep(0,47),1))
plotts.sample.wge(tail(x,n=168))
plotts.sample.wge(tail(x,n=1460))

end(x)



#Fit the model using your model identification (p and q). For
aic5.wge(tail(x,n=2190),p=0:15,q=0:3,type = 'bic')
  #Five Smallest Values of  bic 
  #      p    q        bic
  #43   10    2   14.71023
  #48   11    3   14.71598
  #39    9    2   14.71608
  #49   12    0   14.71655
  #64   15    3   14.71677

aic5.wge(tail(x,n=2190),p=0:15,q=0:3,type = 'aic')
  #Five Smallest Values of  aic 
  #      p    q        aic
  #64   15    3   14.66739
  #56   13    3   14.67315
  #59   14    2   14.67463
  #61   15    0   14.67558
  #62   15    1   14.67639

model1=est.arma.wge(tail(x,n=2190),p=10, q=2)
  #Coefficients of Original polynomial:  
  #  1.8086 -1.5157 0.6788 0.0095 -0.1064 0.0156 -0.0997 0.1239 0.0448 -0.1361 
  
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.8253B+0.8743B^2    1.0439+-0.2327i      0.9350       0.0349
  #1-0.5379B+0.8725B^2    0.3082+-1.0252i      0.9341       0.2035
  #1-1.3014B+0.7691B^2    0.8460+-0.7645i      0.8770       0.1169
  #1+0.6278B+0.5542B^2   -0.5664+-1.2180i      0.7445       0.3193
  #1+1.2281B+0.4185B^2   -1.4673+-0.4863i      0.6469       0.4491

model1$phi
#[1]  1.808597136 -1.515684556  0.678793116  0.009457749 -0.106394586  0.015645465 -0.099660362  0.123891104  0.044763221 -0.136078746

model1$theta
#[1]  0.5005540 -0.5357698

model1$avar
#[1] 2337459

mean(tail(x,n=2190))
#[1] -9.33516

# Next We Examine the Residuals

plotts.sample.wge(model1$res)
acf(model1$res)

ljung.wge(model1$res, p=10, q=2, K=24)
ljung.wge(model1$res, p=10, q=2, K=48)

# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting Last 24 observations
model1.for = fore.arma.wge(tail(traffic$traffic_volume,n=500),phi=model1$phi,n.ahead=24,
                        lastn=FALSE,limits=FALSE) 

x=tail(traffic$traffic_volume,n=2190)
# Calculating ASE for Last 24 observations
n = 24
x.pred = model1.for$f
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,0)))
#[1] "ASE: 1100243"



######################################################
## VAR 

VAR_1 = VAR(cbind(traffic$traffic_volume, traffic$temp, lag.max = 5, type = "both"))


#7 day
traffic_7 = traffic$traffic_volume[48036:48204]
plotts.sample.wge(traffic_7)

traffic_t7 = traffic$temp[48036:48204]
plotts.sample.wge(traffic_t7)


VAR_7 = VAR(cbind(traffic_7, traffic_t7, lag.max = 5, type = "both"))
pred = predict(VAR_7, n.ahead = 8)
plot(traffic_7, type = "l")
lines(seq(48204,48211,1), pred$fcst$y1[,1], col = "red")
ASE = mean((traffic_7[48204:48211] - pred$fcst$y1[1:8])^2)

ASE


#2months
#plotts.sample.wge(traffic$traffic_volume[46744:48204])

#Monthly
#x=artrans.wge(traffic$traffic_volume,phi.tr = c(rep(0,729),1))
#plotts.sample.wge(tail(x,n=168))
#plotts.sample.wge(tail(x,n=1460))

#7 days
#x=artrans.wge(x,phi.tr = c(rep(0,167),1))
#plotts.sample.wge(tail(x,n=168))
#plotts.sample.wge(tail(x,n=1460))

#48 hours
#x=artrans.wge(x,phi.tr = c(rep(0,47),1))
#plotts.sample.wge(tail(x,n=168))
#plotts.sample.wge(tail(x,n=1460))


##########################
### MLP






##########################
### Neural Network

TrafficTrain = ts(traffic$traffic_volume[1:33742], start = c(2012,10), frequency = 12)
#TrafficTrain
TrafficTest = ts(traffic$traffic_volume[33743:48204], start = c(2018,10), frequency = 12)
#TrafficTest
set.seed(2)
fit.mlp = mlp(TrafficTrain, reps = 50, comb = "mean")
fit.mlp
plot(fit.mlp)

fore.mlp = forecast(fit.mlp, h= 36)
plot(fore.mlp)

ASE = mean((TrafficTest - fore.mlp$mean)^2)
ASE

########## Adding lag and taking seasonal variable out

fit.mlp = mlp(TrafficTrain, lags = c(1,2,3,4,5,6,7,8,9,10,11,12),
              allow.det.season = FALSE)
set.seed(2)

fit.mlp
plot(fit.mlp)

fore.mlp = forecast(fit.mlp, h = 36)
plot(fore.mlp)

ASE = mean((TrafficTest - fore.mlp$ean)^2)
ASE

### difference ofder 
fit.mlp = mlp(TrafficTest, difforder = c(12), allow.det.season = FALSE)
set.seed(2)

fit.mlp
plot(fit.mlp)

fore.mlp = forecast(fit.mlp, h=36)
plot(fore.mlp)

ASE = mean((TrafficTest-fore.mlp$mean)^2)
ASE

trafficDF = data.frame(TF = ts)

#############################################
# Ensemble Model 

ensemble = (fore.mlp$mean + pred$fcst$y1[,1])/2

plot(traffic$traffic_volume, type = "l")
lines (seq(48204,48211,1), ensemble, col = "green")

ASE = mean ((traffic$traffic_volume[48204:48211] - ensemble)^2)
ASE
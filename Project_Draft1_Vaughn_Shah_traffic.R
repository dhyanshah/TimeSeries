rm(list = ls(all.names = TRUE))

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



plotts.sample.wge(traffic$traffic_volume[47205:48204])
length(traffic$traffic_volume[47205:48204]) 


################################
### MODELING - TIME SERIES
################################

#1 year = 8760 if we want to look back that far
aic5.wge(traffic$traffic_volume[47205:48204],p=0:15,q=0:3,type = 'bic')
#  Five Smallest Values of  bic 
#        p    q        bic
#  14    3    1   12.81832
#  12    2    3   12.82339
#  18    4    1   12.82486
#  43   10    2   12.82678
#  11    2    2   12.82858


aic5.wge(traffic$traffic_volume[47205:48204],p=0:20,q=0:3,type = 'aic')
#  Five Smallest Values of  aic 
#       p    q        aic
#  72   17    3   12.74404
#  84   20    3   12.74782
#  76   18    3   12.74886
#  70   17    1   12.75327
#  78   19    1   12.75377


#Fit the model using your model identification (p and q). You may use 
#    any of the estimates you like (maximum likelihood, Yule–Walker, Burg).

x=traffic$traffic_volume[47205:48204]

s31=est.arma.wge(x,p=3, q=1)
  #Coefficients of Original polynomial:  
  #2.1935 -1.5381 0.3201 
  #
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.8216B+0.8605B^2    1.0584+-0.2045i      0.9276       0.0304
  #1-0.3720B              2.6885               0.3720       0.0000

s31$phi
  #[1]  2.1935313 -1.5380646  0.3200765

s31$theta
  #[1] 0.8772135

s31$avar
  #[1] 356389

mean(x)
  #[1] 3314.262

# Next We Examine the Residuals
# X21$res: COntains residuals from the ARMA(3,1) fit

plotts.sample.wge(s31$res)
acf(s31$res)


# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting Last 15 observations
s31.for = fore.arma.wge(x,phi=s31$phi,n.ahead=24,
                          lastn=TRUE,limits=FALSE)


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
# Fittiing a seasonal model to the Sunspot data
# Find the ASE for this model using the last 15 years of sunspot data.
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
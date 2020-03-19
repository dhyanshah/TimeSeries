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


#difference the data s=24
#For the last 2 month of data
x=artrans.wge(traffic$traffic_volume[46744:48204],phi.tr = c(rep(0,23),1))


#Fit the model using your model identification (p and q). For
aic5.wge(x,p=0:20,q=0:3,type = 'bic')
  #Five Smallest Values of  bic 
  #      p    q        bic
  #38    9    1   13.31252
  #66   16    1   13.31496
  #74   18    1   13.31514
  #42   10    1   13.31745
  #58   14    1   13.31861

aic5.wge(x,p=0:20,q=0:3,type = 'aic')
  #Five Smallest Values of  aic 
  #      p    q        aic
  #74   18    1   13.24179
  #82   20    1   13.24204
  #78   19    1   13.24486
  #66   16    1   13.24894
  #58   14    1   13.25993

ns91=est.arma.wge(x,p=9, q=1)
  #Coefficients of Original polynomial:  
  #  2.1868 -1.4934 0.2989 -0.0211 0.0039 0.0758 -0.1854 0.2147 -0.0981 
  #
  #Factor                 Roots                Abs Recip    System Freq 
  #1-1.9229B+0.9405B^2    1.0223+-0.1348i      0.9698       0.0209
  #1-1.3563B+0.5961B^2    1.1375+-0.6192i      0.7721       0.0793
  #1+0.7116B             -1.4054               0.7116       0.5000
  #1+0.7972B+0.5045B^2   -0.7901+-1.1653i      0.7103       0.3448
  #1-0.4164B+0.4874B^2    0.4272+-1.3672i      0.6981       0.2018

ns91$phi
#[1]  2.186795606 -1.493380230  0.298903594 -0.021051089  0.003941529  0.075834713 -0.185412702
#[8]  0.214693277 -0.098100068

ns91$theta
#[1] 0.9561202

ns91$avar
#[1] 571983.1

mean(x)
#[1] -19.99443

# Next We Examine the Residuals

plotts.sample.wge(ns91$res)
acf(ns91$res)


# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting Last 24 observations
ns91.for = fore.arma.wge(traffic$traffic_volume[47204:48204],phi=ns91$phi,n.ahead=24,
                        lastn=FALSE,limits=FALSE) 

x=traffic$traffic_volume[47204:48204]
# Calculating ASE for Last 24 observations
n = 24
x.pred = ns91.for$f
ASE = mean((x[(length(x)-n+1):(length(x))]-x.pred)^2)
print(paste0('ASE: ',round(ASE,3)))

# Use this model to generate an ASE from forecasting the last 48 datapoints 
# Forecasting 24 observations forward
s31.for = fore.arma.wge(x,phi=s31$phi,n.ahead=24,
                        lastn=FALSE,limits=FALSE)

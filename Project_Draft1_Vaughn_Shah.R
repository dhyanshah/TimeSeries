#######################################
######### MSDS DS 6373 - 403  #########
########### Spring 2020 ###############
####### Project - Time Series  ########
#### Dhyan Shah & Chandler Vaughn  ####
#######################################

### Objective ###
# Evaluating S&P 500 index trends with respect to larger market volatality sentiments. 


### Data ###
# S&P 500 Index Data
# Vix - Volatality Index Data


#set your working directory for this code to work properly

setwd("/Volumes/Dhyan-MacPC/Education/SMU /MSDS/DS 6373 Time Series/Project")
#setwd("chandler")

### Packages ####
# List of packages loaded for the subject analysis. 

install.packages('tidyverse')
install.packages('dataMaid')
install.packages('ggfortify')
install.packages('GGally')
install.packages('knitr')
install.packages('h2o')
install.packages('jtools')
install.packages('ggstance')
install.packages('ggplaot2')
install.packages("readxl")
install.packages("caret")
install.packages("lattice")
install.packages("xts", repos="http://cloud.r-project.org")
install.packages("zoo")
install.packages("quantmod")
install.packages("TTR")
install.packages("tswge")
install.packages("tidyr")

#load my standard toolbox
suppressWarnings(suppressMessages(library(tidyverse))) #required
suppressWarnings(suppressMessages(library(dataMaid)))
suppressWarnings(suppressMessages(library(ggfortify)))
suppressWarnings(suppressMessages(library(GGally)))
suppressWarnings(suppressMessages(library(knitr)))
suppressWarnings(suppressMessages(library(h2o)))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(readxl))
suppressWarnings(suppressMessages(library(jtools))) 
suppressWarnings(suppressMessages(library(ggstance))) 
suppressWarnings(suppressMessages(library(caret))) 
suppressWarnings(suppressMessages(library(lattice))) 
install.packages("rvest")
install.packages("pbapply")
install.packages("dygraphs")
install.packages("lubridate")
install.packages("tidyquant")
install.packages("timetk")
library(xts)
library(zoo)
library(quantmod)
library(TTR)
library(tswge)
library(tidyr)
library(rvest)
library(pbapply)
library(dygraphs)
library(lubridate)
library(tidyquant)
library(timetk)
require(gplots)

#set print options
options(max.print=1000)
options(tibble.print_max = 1000, tibble.print_min = 1000)

#Session Info
sessionInfo()



# getSymbols(c("^GSPC", "^VIX"), from = as.Date(("1881-01-01"), to = as.Date("2020-10-03")))
# df = as.data.frame(merge(GSPC, VIX))
# df = data.frame(date = row.names(df), df, row.names = NULL)
# head(df)
# tail(df)
# GSPC_VIX = na.omit(df)
# head(df)
# tail(df)

write.csv(GSPC_VIX, "/Volumes/Dhyan-MacPC/Education/SMU /MSDS/DS 6373 Time Series/Project/GSPC_VIX.csv", row.names = FALSE )

SNP_Combined = read.csv(file.choose(),header = TRUE)
head(SNP_Combined)
colnames(SNP_Combined)

############################################################
############ EDA Analysis ##################################
## S&P Index
getSymbols("^GSPC",src="yahoo", from="1990-01-01")
barChart(to.monthly(GSPC), up.col = 'green', dn.col = 'red')
addMACD()
addBBands()


## VIX 
getSymbols("VIX",src="yahoo", from="1990-01-01")
barChart(to.monthly(VIX), up.col = 'green', dn.col = 'red')
addMACD()

#############################################################

## S&P Vs. VIX daily adjusted


plot (SNP_Combined$Date,SNP_Combined$S.P500)
lines(SNP_Combined$Date,SNP_Combined$S.P500,col="blue")
par(new = TRUE)
plot (SNP_Combined$Date,SNP_Combined$VIX.Adjusted,type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(SNP_Combined$Date,SNP_Combined$VIX.Adjusted,col="red")
axis(side=4, at = pretty(range(SNP_Combined$VIX.Adjusted)))
abline(h=20,col = "gray60")
abline(h=30,col = "green")

## Vix Daily Changes

plot(SNP_Combined$Date, Delt(SNP_Combined$VIX.Adjusted))
lines(SNP_Combined$Date, Delt(SNP_Combined$VIX.Adjusted))
abline(h=0)
abline(h=.2, col="red")
abline(h=-.2, col="green")

## Vix Daily Changes Vs. SP500 Adjusted

plot(SNP_Combined$Date, SNP_Combined$GSPC.Adjusted)
lines(SNP_Combined$Date, SNP_Combined$GSPC.Adjusted, col = "blue")
par(new=TRUE)
plot(SNP_Combined$Date,Delt(SNP_Combined$VIX.Adjusted), type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(SNP_Combined$Date, Delt(SNP_Combined$VIX.Adjusted), col= "purple")
abline(h=0)
abline(h=.2, col="red")
abline(h=-.2, col="green")

##  SP500 Adjusted Vs. Interest Rate
plot(SNP_Combined$Date, SNP_Combined$GSPC.Adjusted)
lines(SNP_Combined$Date, SNP_Combined$GSPC.Adjusted, col = "blue")
par(new=TRUE)
plot(SNP_Combined$Date,SNP_Combined$Long.Interest.Rate, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
lines(SNP_Combined$Date, SNP_Combined$Long.Interest.Rate, col= "red")


### Correlation




## S&P Vs CPI
plot(SNP_Combined$CPI, SNP_Combined$S.P500)
lines(SNP_Combined$CPI, SNP_Combined$S.P500)

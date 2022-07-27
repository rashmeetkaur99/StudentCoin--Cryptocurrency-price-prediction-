install.packages("quantmod")       #The quantmod package for R is designed to assist the quantitative trader, it is Quantitative Financial Modelling and Trading Framework for R"
install.packages("broom")          #The broom package takes the messy output of built-in functions in R, such as lm, nls, or t.test, and turns them into tidy tibbles.
install.packages("ggplot2")        #The package is required for data visualization using the Grammar of Graphics.                     
install.packages("fitdistrplus")   #This package is necessary to fit the data in order to understand the distribution
install.packages("tidyquant")
library(ggplot2)                   # After installing the packages, loading the package using library command.
library(quantmod)
library(magrittr)
library(broom)
library(dplyr)
library(ggplot2)
library(fitdistrplus)
# Libraries for GBM)
library(lubridate)
library(TTR)
library(dplyr)


STC = data.frame(Book1)       #Import the data from excel and assigning it as a data-frame under the name STC.
head(STC)                     #Top 6 rows of the data are shown.
str(STC)                      #Structure of the data, i.e number of observations, variables and their data-type.
summary(STC)                  #Summary of the data, it computes range, mean, median, max, min, and quartiles.
STC$Date=as.Date(STC$Date)   #Since, the date in the data-frame is in POSIXct format, i.e date, time and zone, it is better to convert it into a date format only for easy application of certain functions.

                                      
ggplot(STC, aes(x = Date, y = Close..)) + #Draw ggplot2 plot
  geom_line() + 
  scale_x_date(date_labels = "%Y-%m")+
  labs(title="Plot of closing price Vs date")

#Identifying the distribution of the data using Cullen and frey graph
descdist(STC$Close.., discrete = FALSE)


#A Normal Distribution represents a distribution that most natural events follow.
normal_dist <- fitdist(STC$Close.., "norm")
plot(normal_dist)

#Since the data doesn't seem to fit the normal distribution, log closing price has been calculated to see if the data fits the normal distribution
#Computing the log-returns
log_prices <- log(STC$Close..)
log_returns <- diff(log_prices, lag=1)
plot((log_prices), type = 'l')
plot(diff(log_prices), type = 'l')

log_prices_dist <- fitdist(log_prices, "norm")
plot(log_prices_dist)

log_returns_dist <- fitdist(log_returns, "norm")
plot(log_returns_dist)

#Estimating parameters of the GBM
mu <- mean(log_returns)#expected rate of return
sigma <- sd(log_returns)#volatility

#creating GBM model using SDE
set.seed(12345)
S0 <-STC$Close..[667] #last
T <-0.083 #forecasting for 1 month, 1/12 =0.083
t <- seq(1/365,T,by=1/365) #time moments in which we simulate the process (daily)
n <-length(t) #number of days in each trial
par(mfrow = c(3,3))
S<-numeric(n)
S<-S0*exp(cumsum((mu-sigma*sigma/2)*T/n + sigma*sqrt(T/n)*rnorm(n)))
plot(t,S,type="l")

#Definition of a function to calculate the expected stock price after n days
ExpectedPrice <- function(drift, n, S0){
  x <- S0* exp(drift*n)
  abline(h = x, col='blue') #adding a horizontal line on the previous plot
  return(print(paste("The expected price after", n, "days is", x)))
}

#Expected price after one month
ExpectedPrice(drift = mu, n = n, S0 = S0)

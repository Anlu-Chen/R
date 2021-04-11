https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

kings<-scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip = 3)
kings
kingstimeseries<-ts(kings)
kingstimeseries

births<-scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries<-ts(births, frequency=12, start=c(1946,1))
birthstimeseries

souvenir<-scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries<-ts(souvenir, frequency=12, start=c(1987,1))
souvenirtimeseries

plot.ts(kingstimeseries)
plot.ts(birthstimeseries)
plot.ts(souvenirtimeseries)
logsouvenirtimeseries<-log(souvenirtimeseries)
plot.ts(logsouvenirtimeseries)

library(TTR)
kingstimeseriesSMA3<-SMA(kingstimeseries, n=3)
plot.ts(kingstimeseriesSMA3)

kingstimeseriesSMA8<-SMA(kingstimeseries, n=8)
plot.ts(kingstimeseriesSMA8)
birthstimeseriescomponents<-decompose(birthstimeseries)
birthstimeseriescomponents$seasonal
plot(birthstimeseriescomponents) #aqui he la clave

birthstimeseriesseasonallyadjusted<-birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

###
rain<-scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat", skip=1)
rainseries<-ts(rain, start=(1814), end=(1912),frequency = 1)
plot.ts(rainseries)
rainseriesforecasts<-HoltWinters(rainseries, beta=FALSE, gamma = FALSE)
rainseriesforecasts
rainseriesforecasts$fitted
plot(rainseriesforecasts)
rainseriesforecasts$SSE # error cuadratico estimado
predictHoltWinters<-HoltWinters(rainseries,beta=FALSE,gamma=FALSE,l.start=23.56)
library(forecast)
rainseriesforecasts2<-forecast(predictHoltWinters, h=8)
rainseriesforecasts2
plot(rainseriesforecasts2)
rainseriesforecasts2<-forecast(predictHoltWinters, h=8, level=c(80,99), fan=TRUE)
plot(rainseriesforecasts2)
acf(rainseriesforecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(rainseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#X-squared(chi^2) = 17.401, df (grados de libertad) = 20, p-value (demasiado alto)= 0.6268
plot.ts(rainseriesforecasts2$residuals)


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
rainseriesforecasts2$residuals[is.na(rainseriesforecasts2$residuals)]<-0 #para sustituir esos NAs
plotForecastErrors(rainseriesforecasts2$residuals)

skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))

plot.ts(skirtsseries)
skirtsseriesforecasts <- HoltWinters(skirtsseries, gamma=FALSE)
skirtsseriesforecasts
skirtsseriesforecasts$SSE
sqrt(skirtsseriesforecasts$SSE)

plot(skirtsseriesforecasts)

HoltWinters(skirtsseries, gamma=FALSE, l.start=608, b.start=9)

skirtsseriesforecasts2 <- forecast(skirtsseriesforecasts, h=19)
plot(skirtsseriesforecasts2)

acf(skirtsseriesforecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(skirtsseriesforecasts2$residuals, lag=20, type="Ljung-Box")
#X-squared = 19.731, df = 20, p-value = 0.4749, no nos vale por la p,

plot.ts(skirtsseriesforecasts2$residuals)    # make a time plot 

skirtsseriesforecasts2$residuals[is.na(skirtsseriesforecasts2$residuals)]<-0
skirtsseriesforecasts2$residuals

plotForecastErrors(skirtsseriesforecasts2$residuals) # make a histogram
souvenirtimeseriesforecasts <- HoltWinters(logsouvenirtimeseries)
souvenirtimeseriesforecasts

souvenirtimeseriesforecasts$SSE
plot(souvenirtimeseriesforecasts)

predictHoltWinters<-HoltWinters(souvenirtimeseriesforecasts)
souvenirtimeseriesforecasts2 <- forecast(souvenirtimeseriesforecasts, h=48)
plot(souvenirtimeseriesforecasts2)

####
acf(souvenirtimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirtimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(souvenirtimeseriesforecasts2$residuals)            # make a time plot
souvenirtimeseriesforecasts2$residuals[is.na(souvenirtimeseriesforecasts2$residuals)]<-0 #para sustituir esos NAs
plotForecastErrors(souvenirtimeseriesforecasts2$residuals) # make a histogram

####

skirtsseriesdiff1 <- diff(skirtsseries, differences=1)
plot.ts(skirtsseriesdiff1)

skirtsseriesdiff2 <- diff(skirtsseries, differences=2)
plot.ts(skirtsseriesdiff2)

######

kingtimeseriesdiff1 <- diff(kingstimeseries, differences=1)
plot.ts(kingtimeseriesdiff1)
kingtimeseriesdiff2 <- diff(kingstimeseries, differences=2)
plot.ts(kingtimeseriesdiff2)

acf(kingtimeseriesdiff1, lag.max=20)             # plot a correlogram
acf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values
pacf(kingtimeseriesdiff1, lag.max=20, plot=FALSE) # get the autocorrelation values
pacf(kingtimeseriesdiff1,lag.max = 20,plot = TRUE)

######

volcanodust <- scan("http://robjhyndman.com/tsdldata/annual/dvi.dat", skip=1)
volcanodustseries <- ts(volcanodust,start=c(1500))
plot.ts(volcanodustseries)

acf(volcanodustseries, lag.max=20)             # plot a correlogram
acf(volcanodustseries, lag.max=20, plot=FALSE) # get the values of the autocorrelations

pacf(volcanodustseries, lag.max=20)
pacf(volcanodustseries, lag.max=20, plot=FALSE)

.
..
...
...

volcanodustseriesarima <- arima(volcanodustseries, order=c(2,0,0))
volcanodustseriesarima

volcanodustseriesforecasts <- forecast(volcanodustseriesarima, h=31)
volcanodustseriesforecasts                                             
plot(volcanodustseriesforecasts)

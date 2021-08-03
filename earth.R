data(quakes)


head(quakes)

summary(quakes)


attach(quakes)



##abline
plot(mag, stations, 
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting")


plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting",
     col = rgb(0.1, 0.2, 0.8, 0.3))



Quake.mod <- lm(stations ~ mag)
Quake.mod

##SLR Model Assumptions


plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting",
     col = rgb(0.1, 0.2, 0.8, 0.3))

abline(-180.42, 46.28, col="red", lwd = 2)


##Residual Plot
    
QuakeResiduals <- Quake.mod$residuals
QuakeFittedValues <- Quake.mod$fitted.values
plot(QuakeFittedValues, QuakeResiduals, 
     pch = 20,
     xlab= "Magnitude",
     ylab= "Residual",
     main= "Residual Plot",
     col = rgb(0.1, 0.2, 0.8, 0.3))
abline(0,0,col="brown", lwd = 2.5)


##Residual Histogram

hist(QuakeResiduals, breaks=25,
     xlab="Residual Value",
     ylab="Frequency",
     main="Histogram of Residuals",
     col="moccasin")


##Q-Q Plot

qqnorm(QuakeResiduals, col="royalblue")
qqline(QuakeResiduals, col="orange")



##Model Accuracy and Precision
##Confidence Intervals for Regression Coefficients



##confint

confint(Quake.mod, level=.95)

plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting",
     col = rgb(0.1, 0.2, 0.8, 0.3))
abline(-188.6463, 46.28, col = 'black', lwd = 2)
abline(-180.42, 46.28, col="red", lwd = 2)
abline(-172.2024, 46.28, col = 'black', lwd = 2)




plot(jitter(mag, amount = 0.05), stations, 
     pch = 20,
     ylab = "# of Stations Reporting",
     xlab = "Magnitude",
     main = "Fiji Earthquakes Magnitude and Reporting",
     col = rgb(0.1, 0.2, 0.8, 0.3))
abline(-180.42, 44.5094, col = "black", lwd = 2)
abline(-180.42, 46.28, col="red", lwd = 2)
abline(-180.42, 48.0550, col = "black", lwd = 2)

##Confidence and Prediction Intervals for Response Variable

Quake4.5 <- data.frame(mag=4.5)

##Response Confidence Interval

Stations.confidenceint <- predict(Quake.mod, Quake4.5, interval="confidence", level = .95)
Stations.confidenceint


##Response Prediction Interval

Stations.predictionint <- predict(Quake.mod, Quake4.5, interval="prediction", level = .95)
Stations.predictionint


##Correlation


summary(Quake.mod)


##cor
cor(mag, stations)


##Testing Significance
##Hypothesis Testing for Regression Coefficients

summary(Quake.mod)



##F Distribution & F-Test

curve(df(x, df1=100, df2=10), from=0, to=5,
      xlab="F-Statistic",
      ylab="Probability",
      main="F-Distribution",
      col = "navy")



summary(Quake.mod)

### R code from vignette source 'Stat579-HW1-Xie.Rnw'


###################################################
### code chunk number 3: read-apt
###################################################

x <- "http://www.public.iastate.edu/~maitra/stat579/datasets/student-apt.dat"
apt <- read.table(x, col.names = c("group", "aptitude", 
    "mathematics", "language", "general.knowledge"))
## or you can use names(apt) <- c(...)



###################################################
### code chunk number 4: apt-pairs
###################################################

pairs(apt[, -1], pch = 20, col = apt[, 1])



###################################################
### code chunk number 5: attach-cars
###################################################

attach(cars)



###################################################
### code chunk number 6: convert-speed
###################################################

speed.fps <- speed * 5280/3600  # feet per second



###################################################
### code chunk number 7: speed-vs-distance
###################################################

plot(dist, speed.fps, xlab = "distance (feet)", ylab = "speed (feet/second)", 
    pch = 20)



###################################################
### code chunk number 8: metric-system
###################################################

speed.mps <- speed * 1609.3/3600
dist.m <- dist/5280 * 1609.3



###################################################
### code chunk number 9: detach-cars
###################################################

detach(cars)



###################################################
### code chunk number 10: speed-vs-distance-m
###################################################

plot(dist.m, speed.mps, xlab = "distance (metres)", 
    ylab = "speed (metres/second)", pch = 20)



###################################################
### code chunk number 11: save-plot (eval = FALSE)
###################################################

##
## png('my-cars-plot1.png') # open a device; other
#   possibilities: pdf(), jpeg(), ...
##
## plot(cars)
##
## dev.off()  # close the device, and a PNG file is created
##
## getwd() # go find the image file under the current
#   working directory
##



###################################################
### code chunk number 12: cel-to-fah
###################################################

temperature.f <- pressure$temperature * 9/5 + 32  # happy Americans
## now create a new data frame
pressure2 <- data.frame(temperature = temperature.f, 
    pressure = pressure$pressure)



###################################################
### code chunk number 13: pressure-vs-temp
###################################################

plot(temperature ~ pressure, data = pressure2, xlab = "temperature (Fahrenheit scale)", 
    pch = 20)



###################################################
### code chunk number 14: regression-wo-inter
###################################################

# + 0 means no intercept, or you can use: pressure - 1
fit <- lm(temperature ~ pressure + 0, data = pressure2)
summary(fit)
## plot with a fitted line
plot(temperature ~ pressure, data = pressure2, ylab = "temperature (Fahrenheit scale)", 
    pch = 20)
abline(fit, col = "red")



###################################################
### code chunk number 15: res-vs-fitted
###################################################

plot(fitted(fit), resid(fit), xlab = "fitted values", 
    ylab = "residuals", pch = 20)



###################################################
### code chunk number 16: more-pressure
###################################################

pressure3 <- pressure2  # base on pressure2
pressure3$pressure.2 <- pressure3$pressure^2  # square
pressure3$pressure.3 <- pressure3$pressure^3  # cubic



###################################################
### code chunk number 17: multiple-lm
###################################################

## ~. means using all the variables in the data frame; of
#   course you can write them out explicitly as:
#   temperature ~ pressure + pressure.2 + pressure.3
fit3 <- lm(temperature ~ ., data = pressure3)
summary(fit3)



###################################################
### code chunk number 18: multiple-lm-plot
###################################################

plot(temperature ~ pressure, data = pressure3, ylab = "temperature (Fahrenheit scale)", 
    pch = 20)
lines(pressure3$pressure, fitted(fit3), col = "blue")



###################################################
### code chunk number 19: res-vs-fitted2
###################################################

plot(fitted(fit3), resid(fit3), xlab = "fitted values", 
    ylab = "residuals (multiple regression)", pch = 20)



###################################################
### code chunk number 20: install-formatR (eval = FALSE)
###################################################

## install.packages('formatR')



###################################################
### code chunk number 21: formatR-usage (eval = FALSE)
###################################################

## library(formatR)
## ## now copy your R code to clipboard (usually Ctrl+C),
#   then
## tidy.source()
## ## or if you have an R script under a directory, say,
#   C:/abc/def/my-script.R, you can use
## tidy.source('C:/abc/def/my-script.R') 

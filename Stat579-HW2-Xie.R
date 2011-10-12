### R code from vignette source 'Stat579-HW2-Xie.Rnw'


###################################################
### code chunk number 3: sd-formula
###################################################

## the long formula
sd_long <- function(x) {
    sqrt(sum((x - mean(x))^2)/(length(x) - 1))
}
## the short formula
sd_short <- function(x) {
    n <- length(x)
    sqrt((sum(x^2) - n * mean(x)^2)/(n - 1))
}



###################################################
### code chunk number 4: small-numbers
###################################################

d1 <- rep(c(1001, 1002), times = 5)  # replicate a vector for 5 times



###################################################
### code chunk number 5: small-results
###################################################

sd_long(d1)  # using long formula
sd_short(d1)  # using short formula



###################################################
### code chunk number 6: small-sd
###################################################

sd(d1)  # inbuilt function is sd()



###################################################
### code chunk number 7: big-numbers
###################################################

d2 <- rep(c(100000000000001, 100000000000002), times = 5)



###################################################
### code chunk number 8: big-results
###################################################

sd_long(d2)  # using long formula
sd_short(d2)  # using short formula



###################################################
### code chunk number 9: big-sd
###################################################

sd(d2)



###################################################
### code chunk number 10: wind-data
###################################################

wind <- read.csv("wind.csv")  # assume wind.csv is under getwd()
str(wind)



###################################################
### code chunk number 11: wind-summary
###################################################

## sapply() can be used to a data frame, in which a
#   function is applied to each column (here IQR means
#   inter-quartile range, i.e. 3rd quartile minus 1st
#   quartile)
sapply(wind, function(x) {
    c(mean = mean(x), sd = sd(x), median = median(x), quantile(x, 
        probs = c(0.25, 0.5, 0.75)), IQR = IQR(x))
})



###################################################
### code chunk number 12: 180-degrees
###################################################

## code provided here only for fun on playing with
#   low-level plotting commands (please ignore it if not
#   interested)
set.seed(123)  # for reproducibility
par(mfrow = c(1, 2), ann = FALSE, mar = rep(0, 4))
n <- 30
x1 <- runif(n/2, 0, 0.1 * pi)
x1 <- c(x1, 2 * pi - x1)
x2 <- runif(n, 0, 2 * pi)
plot(cos(x1), sin(x1), type = "n", axes = FALSE, xlim = c(-1, 
    1), ylim = c(-1, 1))
arrows(0, 0, cos(x1), sin(x1), length = 0.05)
plot(cos(x2), sin(x2), type = "n", axes = FALSE, xlim = c(-1, 
    1), ylim = c(-1, 1))
arrows(0, 0, cos(x2), sin(x2), length = 0.05)
## what are the means and standard deviations?
mean(x1)/pi * 180
mean(x2)/pi * 180
sd(x1)
sd(x2)
## the right plot seems to have larger variability, but the
#   left plot has a larger variance!



###################################################
### code chunk number 13: season-direction
###################################################

## turn original data into a vector and convert to radians
#   from degrees
theta <- as.vector(as.matrix(wind)) * pi/180
## create a new dataset, with seasons in it
wind2 <- data.frame(x = cos(theta), y = sin(theta), 
    season = factor(rep(names(wind), each = nrow(wind)), levels = names(wind)))
plot(y ~ x, data = wind2, col = as.integer(wind2$season), 
    pch = 20)
legend(-0.35, 0.35, names(wind), fill = 1:4, bty = "n", 
    cex = 0.8)



###################################################
### code chunk number 14: season-coplot
###################################################

coplot(y ~ x | season, data = wind2, col = as.integer(wind2$season), 
    xlab = "$\\cos(\\theta)$", ylab = "$\\sin(\\theta)$", pch = 20, 
    xlim = c(-1, 1), ylim = c(-1, 1), rows = 1, columns = 4)



###################################################
### code chunk number 15: sd-formula2
###################################################

sd_short2 <- function(x) {
    sqrt(sum(x^2 - mean(x)^2)/(length(x) - 1))
}
sd_short2(d2)  # now we get NaN



###################################################
### code chunk number 16: na-nan
###################################################

sqrt(-2)
x <- NA
x == NA  # wrong way of checking if x is NA
is.na(x)  # correct way
x <- NaN
is.nan(x)
is.na(x)  # NaN also implies NA
## you may wonder if 1/0 is NaN, the answer is no
1/0  # it is infinite
is.finite(1/0)



###################################################
### code chunk number 17: floating-arith
###################################################

a <- sqrt(2)
a * a == 2  # mathematically, should be exactly equal to 2
a * a - 2
## another surprising example
x <- 0.3 - 0.7 + 0.4
x == 0  # FALSE; what?!
x 

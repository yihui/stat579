### R code from vignette source 'Stat579-HW5-Xie.Rnw'


###################################################
### code chunk number 3: Cars-data
###################################################

data(Cars93, package = "MASS")
mycars <- Cars93[, c("Min.Price", "Max.Price", "MPG.city", 
    "MPG.highway", "EngineSize", "Length", "Weight")]
## (a)
Cars.Means <- apply(mycars, 2, mean)  # or use colMeans(mycars)
## (b)
Cars.Std.Errors <- apply(mycars, 2, function(x) {
    sd(x)/sqrt(length(x))
})
## (c)
t99 <- qt(p = 0.995, df = nrow(mycars) - 1)
## I'm slightly tricky here with matrix multiplication:
Cars.CI.99 <- t(Cars.Means + t99 * Cars.Std.Errors %*% 
    t(c(-1, 1)))
cars.stats <- list(Cars.Means = Cars.Means, Cars.Std.Errors = Cars.Std.Errors, 
    Cars.CI.99 = Cars.CI.99)
str(cars.stats)





###################################################
### code chunk number 4: state-x77-data
###################################################

## (a)
(region.income <- tapply(state.x77[, "Income"], state.region, 
    mean))
## (b)
(illit.max <- tapply(state.x77[, "Illiteracy"], state.division, 
    max))
## sometimes you may want tapply(..., max, na.rm = TRUE) to
#   remove NA's
## (c)
## the first argument can be anything of length 50 (I used
#   state.region)
(region.freq <- tapply(state.region, state.region, 
    length))
## should be equivalent to
table(state.region)
## (d)
## identity() is a function as its name indicates
div.states <- tapply(rownames(state.x77), state.division, 
    identity)
str(div.states)
## (e)
state.size <- cut(state.x77[, "Population"], breaks = c(0, 
    2000, 10000, Inf), labels = c("Small", "Medium", "Large"))
(grad.median <- tapply(state.x77[, "HS Grad"], list(state.region, 
    state.size), median))





###################################################
### code chunk number 5: Cov2Cor-function
###################################################

COV2COR <- function(x) {
    v <- sqrt(diag(x))
    ## divide each row by standard deviation
    tmp <- sweep(x, 1, v, FUN = "/")
    ## then divide each column
    sweep(tmp, 2, v, FUN = "/")
}
## a simple test case
z <- matrix(c(64, 1, 1, 4), 2)
COV2COR(z)
## there is actually a function in R that does this job
cov2cor(z)  # why did I use upper cases in my version?





###################################################
### code chunk number 6: MAD-calculation
###################################################

## (a)
(mad.a <- apply(mycars, 2, mad))
## (b): step by step according to the formula
m0 <- apply(mycars, 2, median)
m1 <- sweep(mycars, 2, m0, FUN = "-")
m2 <- abs(m1)
m3 <- apply(m2, 2, median)
1.4826 * m3  # done; the same as (a)?

 

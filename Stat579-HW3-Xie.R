### R code from vignette source 'Stat579-HW3-Xie.Rnw'


###################################################
### code chunk number 3: senate-data
###################################################

## note the separator is the tab character, and quote
#   should be the double quote character
senate <- read.table("http://maitra.public.iastate.edu/stat579/datasets/senate-109.txt", 
    header = TRUE, sep = "\t", quote = "\"")



###################################################
### code chunk number 4: senate-structure
###################################################

str(senate[, 1:4])



###################################################
### code chunk number 5: bill-type
###################################################

## a dot . matches any characters, and a modifier * means
#   match the previous pattern (i.e. the dot) for arbitrary
#   times
bill.type <- sub("_.*", "", senate[, 1])



###################################################
### code chunk number 6: tab-bills
###################################################

table(bill.type)



###################################################
### code chunk number 7: data-quality
###################################################

X <- as.matrix(senate[, -c(1, 2)])  # remove the first 2 columns and assign to X
vote1 <- diag(X %*% t(X))  # non-missing votes
vote2 <- senate$missing_votes  # missing votes
## they should add up to 100
head(vote1 + vote2, 30)  # result too long; only print the first 30 elements
## or simply ask R 'are all of them equal to 100'?
all(vote1 + vote2 == 100)  # the answer is YES!



###################################################
### code chunk number 8: frist-matrix
###################################################

## we need to remove the 0 votes of Frist (in the last
#   column)
ncol(senate)  # number of columns in the data
## note how to use logical values as row indices
cond <- senate[, 102] != 0
senate2 <- senate[cond, ]
## an equivalent but probably more intuitive way to write
#   this is: senate2 <- subset(senate, cond); you can read
#   the R code as 'subset the data according to this
#   condition', which is nearly plain English
frist <- senate2[, 3:101] * senate2[, 102]  # this is what we want
## randomly check a few columns to make sure we are correct
head(senate2[, c(3, 87, 102)], 3)  # first 3 rows
head(frist[, c(1, 85)], 3)  # column 3 in senate2 is really column 1 in frist



###################################################
### code chunk number 9: tab-frist
###################################################

votes.frist <- t(apply(frist, 1, function(x) {
    ## why not table(x) directly? because some rows may not
    #   have all of -1, 0 and 1, then we will run into
    #   troubles; factor() here explicitly converts each row to
    #   a factor with 3 levels, so table() will not ignore any
    #   type of votes
    x <- factor(x, levels = c(-1, 0, 1), labels = c("against", 
        "neutral", "for"))
    table(x)
}))
## how the data looks like now (together with bill types);
#   note we have to index bill.type by cond too (this is
#   for demo purpose; not part of the solutions)
head(data.frame(bill.type = bill.type[cond], votes.frist))
## we aggregate the data by bill.type using the function
#   mean()
aggregate(votes.frist, by = list(bill.type = bill.type[cond]), 
    mean)



###################################################
### code chunk number 10: fbp-data
###################################################

x <- as.matrix(read.table("http://maitra.public.iastate.edu/stat579/datasets/fbp-img.dat"))
str(x)



###################################################
### code chunk number 11: fbp-image-orig
###################################################

par(mar = rep(0, 4))
image(x, axes = FALSE)



###################################################
### code chunk number 12: bin-range
###################################################

## define a function that bins data according to breaks
bin_data <- function(breaks) {
    z <- as.vector(x)
    ## labels=FALSE means to map x to integer code;
    #   include.lowest means the left-most bin should include
    #   the minimum of the data (the intervals are of the form
    #   (low, high])
    idx <- cut(z, breaks = breaks, labels = FALSE, include.lowest = TRUE)
    m <- (breaks[idx] + breaks[idx + 1])/2  # mid-points
    matrix(m, nrow = nrow(x), ncol = ncol(x))
}
b1 <- seq(min(x), max(x), length = 9)  # 9 points define 8 bins
x1 <- bin_data(b1)
par(mar = rep(0, 4))
image(x1, axes = FALSE)



###################################################
### code chunk number 13: bin-quantile
###################################################

b2 <- quantile(x, seq(0, 1, length = 9))
x2 <- bin_data(b2)
par(mar = rep(0, 4))
image(x2, axes = FALSE)



###################################################
### code chunk number 14: fbp-hist
###################################################

par(mfrow = c(1, 3))
hist(x, xlab = "original values", main = "")
hist(x1, xlab = "bin by range", main = "")
hist(x2, xlab = "bin by quantiles", main = "")



###################################################
### code chunk number 15: recycling-short
###################################################

## a simple example about vectors
x1 <- 1:10
x2 <- 1:2
## x1 is of length 10; x2 is of length 2, so x2 is extended
#   to match the length of x1
x1 + x2
## equivalent to
x1 + rep(x2, 5)
## what about matrices?
(x3 <- matrix(1:6, 2))
x3 + c(10, 20)  # the second object is treated as a single column and added to x3



###################################################
### code chunk number 16: cut-demo
###################################################

x <- c(1, 6, 4, 9, 2)
b <- c(0, 3, 5, 10)  # breaks
cut(x, b)
(idx <- cut(x, b, labels = FALSE))  # what do these integers mean?
b[idx]
b[idx + 1]
(b[idx] + b[idx + 1])/2 

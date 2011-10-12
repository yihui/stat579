### R code from vignette source 'Stat579-HW4-Xie.Rnw'


###################################################
### code chunk number 3: gene-data
###################################################

## the first column contains row names, so read.csv(...,
#   row.names = 1); in this case, I only have 22 columns
#   here (instead of 23 which you normally would get)
gene <- read.csv("http://maitra.public.iastate.edu/stat579/datasets/diurnaldata.csv", 
    row.names = 1)



###################################################
### code chunk number 4: average-rep
###################################################

gene.rep <- array(as.matrix(gene), c(22810, 11, 2))
mean.rep <- apply(gene.rep, c(1, 2), mean)



###################################################
### code chunk number 5: average-time
###################################################

## average in each row
mean.gene <- apply(mean.rep, 1, mean)



###################################################
### code chunk number 6: eliminate-mean
###################################################

mean0.gene <- mean.rep - matrix(mean.gene, nrow = 22810, 
    ncol = 11)



###################################################
### code chunk number 7: gene-sd1
###################################################

sd.gene <- apply(mean0.gene, 1, sd)
gene.scaled <- mean0.gene/matrix(sd.gene, nrow = 22810, 
    ncol = 11)



###################################################
### code chunk number 8: small-gene
###################################################

## same idea as above
small <- read.table("http://maitra.public.iastate.edu/stat579/datasets/micromeans.dat")
small <- as.matrix(small)
small.mean <- apply(small, 1, mean)
small.sd <- apply(small, 1, sd)
small.scaled <- (small - small.mean)/small.sd



###################################################
### code chunk number 9: euclidean-distances
###################################################

## set up data from 1(e)
gene2 <- array(gene.scaled, dim = c(22810, 11, 20))
## set up data from 1(f)
small2 <- array(dim = c(22810, 11, 20))
## replicate the k-th row in small.scaled for 22810 times;
#   note byrow = TRUE is extremely important!!
for (k in 1:20) {
    small2[, , k] <- matrix(small.scaled[k, ], nrow = 22810, 
        ncol = 11, byrow = TRUE)
}
## distances between genes in big data and those in small
#   data
gene.dist <- apply(gene2 - small2, c(1, 3), function(x) sqrt(sum(x^2)))
dim(gene.dist)  # dimensions should be 22810x11
## which column is the minimum in each row?
gene.nearest <- apply(gene.dist, 1, which.min)
(gene.freq <- table(gene.nearest))
pie(sort(gene.freq), col = NA)  # no colors
## STATISTICIANS SHOULD AVOID PIE CHARTS IN GENERAL (SEE
#   ?pie FOR REASONS); IF YOU HAVE TO DRAW A PIE CHART, YOU
#   ALMOST ALWAYS NEED TO SORT THE DATA FIRST! PIE CHARTS
#   WITH MANY CATEGORIES ARE HORRIBLE TO READ.



###################################################
### code chunk number 10: HW4-examples
###################################################

set.seed(20101011)  # for reproducibility
(x <- matrix(sample(12), ncol = 4))  # a small matrix; treat it as the gene expression data
(x.rep <- array(x, c(3, 2, 2)))  # what happens when putting it into an array?
(x.mean <- apply(x.rep, c(1, 2), mean))  # does apply() get correct means?
## how about replicating data in the 3rd dimension?
array(x, c(3, 4, 2))
matrix(1:12, nrow = 3, ncol = 4)  # default is byrow = FALSE
matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)  # what does 'byrow' mean?
## you can go on to verify other things



###################################################
### code chunk number 11: scale-demo
###################################################

## scale() applies to the columns of data while in this
#   case we are supposed to scale the rows, so need to
#   transpose, scale, then transpose back!
t(scale(t(x.mean))) 

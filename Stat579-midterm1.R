

## @knitr Q1
exam=read.table('http://maitra.public.iastate.edu/madeup.dat', header = TRUE)

## @knitr Q2
hw.names=paste('H',1:12,sep='') # homework variable names
hw=as.matrix(exam[, hw.names]) # use the column names to extract HW scores
## what we need is hw[c(n, n, ..., n), ] where n is nrow(hw) (i.e. last row index) and this matrix matches the size of hw
hw.sd=100*hw/hw[rep(nrow(hw), nrow(hw)), ] # standardized HW scores

## @knitr Q3
## why need t()? because apply() arranges results in columns and we really want them to be in rows!
hw.best = t(apply(hw.sd, 1, sort, decreasing = TRUE))[, 1:10]
hw.best = as.data.frame(hw.best)
colnames(hw.best)=paste('BH',1:10,sep='') # column names: BH+i

## @knitr Q4
hscore = rowMeans(hw.best) # row means

## @knitr Q5
## Q5 (a)
qscore = scan('http://maitra.public.iastate.edu/qscore.dat')
pscore = scan('http://maitra.public.iastate.edu/pscore.dat')
## put together all variables
allscore = data.frame(UID = exam$UID, hscore = hscore, qscore = qscore, pscore = pscore, exam[, c('M1', 'M2', 'FN')])

## Q5 (b)
allscore[, c('M1', 'M2')] = allscore[, c('M1', 'M2')]/40*100

## @knitr Q6
## Q6 (a)
pairs(allscore[, c('hscore','qscore','FN')], pch=20, panel=panel.smooth)
## I added smooth lines to the scatter plots to see the positive correlation more clearly

## Q6 (b)
## matrix multiplication is handy here: MATRIX %*% VECTOR
fscore = as.matrix(allscore[,-1]) %*% c(.15,.1,.1,.2,.2,.25)

## Q6 (c)
## the intervals are of the form (low, high] (i.e. right-closed)
grade = cut(fscore, breaks = c(0,59,69,79,89,100), labels=c('F','D','C','B','A'))

## Q6 (d)
table(grade)

## @knitr Q7
last4 = substr(allscore$UID, 7, 10) # last four digits, i.e. from 7 to 10
## put together the three variables to be released
res = data.frame(ID4 = last4, fscore = fscore, grade = grade)
## index by the order of last 4 digits
res[order(last4), ]

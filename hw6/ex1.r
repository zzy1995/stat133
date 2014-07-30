library(RUnit)
errMsg <- function(err) print(err)
load('hw6-tests.rda')

# Oftentimes, classifiers output the probability that an observation belongs to
# a specific class. We can use this probability along with some threshold to
# determine class membership for each observation. In the binary class case, the
# intuitive threshold for classifying a particular observation as 1 (as opposed
# to 0) is 0.5. However, one can vary this discriminative threshold depending on
# the context of the problem (eg. if misclassifying a 1 as a 0 is much more
# costly, we would classify observations as class 1 even when their probability
# of being in class 1 is below 0.5). The ROC curve is a graphical representation
# of the performance of a classifier as the discriminative threshold is
# varied. It plots the true positive rate (y-axis) agains the false positive
# rate (x-axis). The general idea is that if we can increase tpr while keeping
# fpr constant (or at least faster than we are increasing fpr) our classifier
# will be better. A line with slope 1 on this plot is equivalent to a classifier
# that makes random guesses. When our ROC curve falls above this line, our
# classifier is performing better than a random guess.

# Implement the function tpr. Your function should take the folowing
# arguments:
#
# <threshold>: a numeric value between 0 and 1 that gives the probability
#   cutoff where points are predicted to belong to class 1
#   (i.e. observations whose predicted probability exceeds <threshold> are
#   predicted to be from class 1)
# <predicted>: a numeric value giving the predicted probability for each
#   observation belonging to class 1.
# <truth>: a binary vector giving true label (0 or 1) for each observation
#
# Your function should return the true positive rate at <threshold>. The
# tpr is defined as the number of "true" observations (i.e. from class 1)
# that are predicted to be "true" divided by the total number of "true"
# observations.

tpr <- function(threshold, predicted, truth) {

    return(sum(predicted>threshold & truth==1)/sum(truth==1))
}

tryCatch(checkEquals(hw6$tpr.t1, tpr(0.5, hw6$tpr.pr, hw6$tpr.tr)),
         error=function(err) errMsg(err))


# Implement the function fpr. Your function should take the following
# arguments:
#
# <threshold>: a numeric value between 0 and 1 that gives the probability
#   cutoff where points are predicted to belong to class 1
#   (i.e. observations whose predicted probability exceeds <threshold> are
#   predicted to be from class 1)
# <predicted>: a numeric value giving the predicted probability for each
#   observation belonging to class 1.
# <truth>: a binary vector giving true label (0 or 1) for each observation
#
# Your function should return the false positive rate. The fpr is defined
# as the number of "false" observations (i.e. from class 0) predicted to be
# from class 1 divided by the total number of false observations.

fpr <- function(threshold, predicted, truth) {

    return(sum(predicted>threshold & (truth==0))/sum(truth==0))
}

tryCatch(checkEquals(hw6$fpr.t1, fpr(0.5, hw6$tpr.pr, hw6$tpr.tr)),
         error=function(err) errMsg(err))


# Implement the function plotROC. Your function should take the following
# arguments:
#
# <predicted>: a numeric value giving the predicted probability for each
#   observation belonging to class 1 (as determined by some classifier).
# <truth>: a numeric vector giving true label (0 or 1) for each observation
# <add>: a logical that indicates whether a new plot should be created (F)
#   or if the curve should be added to an existing plot (T)
# <...>: additional parameters to pass to the plot
#
# If add is false, your function should plot the ROC curve for your
# data. The tpr and fpr should be calculated using the <predicted> and
# <truth> variables along with a grid of threshold values that ranges from
# 0 to 1 by 0.01. In addition, you should add a dashed black line with
# slope 1 and intercept 0. The title should be "ROC curve" and the x and y
# axes should range from 0 to 1 and be labeled fpr and tpr respectively.
#
# If add is true, your function should add the ROC curve to an existing
# plot. Your function should also return a list with two elements:
# tprs = the true positive rates over your threshold grid
# fprs = the false positive rates over your threshold grid

plotROC <- function(predicted, truth, add=F, ...) {

    tprs=sapply(0:100,function(i) tpr(i/100,predicted,truth))
    fprs=sapply(0:100,function(i) fpr(i/100,predicted,truth))
    if(!add){
    	plot(fprs,tprs,main='ROC curve',xlim=c(0,1),ylim=c(0,1),xlab='fpr',ylab='tpr',type='l',...)
    	lines(c(1,0),c(1,0))
    } else{
    	lines(fprs,tprs,...)
    }
    return(list(tprs=tprs,fprs=fprs))
    
}

tryCatch(checkEquals(hw6$plotROC.t1, plotROC(hw6$tpr.pr, hw6$tpr.tr)),
         error=function(err) errMsg(err))

# We will use your previously defined functions to compare recursive
# partitioning with random forests by trying to classify spam emails. In your
# folder, you will find the "spam-data.csv" which we have read in for you and
# stored as the variable "data" in a list called "spam". This file contains 57
# variables related to the content of 4601 emails. The last column is a variable
# indicating whether the email was spam (1) or not (0). For the purposes of
# classification, please convert the spam variable to a factor and name this
# column "spam" (do not create a new column).

library(rpart)
library(randomForest)
spam <- list()
spam$data <- read.csv('spam-data.csv', header=F)
spam$data$V58 <- factor(spam$data$V58)
colnames(spam$data)[58] <- "spam"


# Before running a classifier, please split your data into training and test
# sets. To do this, randomly sample 3,500 observtions from your dataset using
# R's sample function. Store this as an element named "train" in the list
# "spam". Store the remaining observations as the element "test" in the list
# "spam".
# ***Make sure to set your seed to 47 before sampling***

set.seed(47)
idcs=sample(1:nrow(spam$data),3500)
spam$train=spam$data[idcs,]
spam$test=spam$data[-idcs,]

# fit a recursive partitioning classifier and a random forest classifier to your
# training data. Use these to predict the probability that each observation is a
# spam email. Store your predictions as the variables <pred.rp> and <pred.rf>
# for recursive partitioning and random forests respectively (these should be
# vectors that are equal in length to nrow(spam$data)). Set the ntree
# argument of randomForest to 250, but do not change any of the other model
# parameters.  *** Make sure to set your seed to 47 before fitting your
# models***

set.seed(47)
p.rp = rpart(spam~., method='class', data=spam$train)

pred.rp=predict(p.rp,spam$test,type='prob')

p.rf=randomForest(spam~.,method='class',data=spam$train,ntree=250)

pred.rf=predict(p.rf,spam$test,type='prob')


# Evaluate your t`wo models using your plotROC function. Store the outputs as
# <rp.output> and <rf.output> respectively. Please plot the ROC curves in the
# same plot with the rp curve blue and the rf curve red.  NOTE: your may need to
# convert your spam variable back to a 0-1 valued vector depending on how you
# wrote your plotROC function.
# Add a legend in the bottom right indicating the the model that each curve represents.

rp.output=plotROC(pred.rp[,2],spam$test$spam,add=F,col='blue')
rf.output=plotROC(pred.rf[,2],spam$test$spam,add=T,col='red')
legend("bottomright",c('recursive partitioning','random forest'),col=c('blue','red'),lty=1)

# In the spam scenario, a true positive represents classifying spam email correctly
# while a false positive represents classifying a good email as spam. For each
# of the models, what is the lowest we can make fpr while still catching all
# spam emails? In other words, how many good emails must we be willing to
# misclassify if we want to catch all spam emails? 
#
# Implement the function constrainedFPR. Your function should take the following
# arguments:
#
# <tpr>: a numeric vector of true positive rates
# <fpr>: a vector of false positive rates that is the same length as <tpr>
# <tpr.constraint> : a numeric value indicating the lowest acceptable true
#   positive rate
#
# Your function should find the smallest value that <fpr> can be while keeping
# <tpr> greater than or equal to <tpr.constraint>. In addition, your function
# should execute "stop" with the message "unequal input lengths" if <tpr> and
# <fpr> are not the same length.

constrainedFPR <- function(tpr, fpr, tpr.constraint) {

	if(length(tpr)!=length(fpr)) stop('unequal input lengths')
    return(min(fpr[tpr>=tpr.constraint]))
}

tryCatch(checkEquals(hw6$constrainedFPR.t1, constrainedFPR(hw6$plotROC.t1$tprs,
                                                           hw6$plotROC.t1$fprs,
                                                           0.5)),
         error=function(err) errMsg(err))


# In each of the models, what was the lowest we could make fpr while classifying
# 95 percent of spam emails correctly? Create variables for these values and
# store them as <min.fpr.rp> and <min.fpr.rf>. In each model what discriminative
# thresholds did these minimum fprs occur at? In other words, at what
# probabilities of being spam (as determined by each model) should we classify
# an email as being spam to achieve a tpr of 0.95 and fprs of <min.fpr.rp> and
# <min.fpr.rf>? Use the same grid of probabilities as in your plotROC function
# and store these values as <rp.threshold>.

min.fpr.rp=constrainedFPR(rp.output$tprs,rp.output$fprs,0.95)
min.fpr.rf=constrainedFPR(rf.output$tprs,rf.output$fprs,0.95)

rp.threshold= which(rp.output$fprs==min.fpr.rp & rp.output$tprs>=0.95)/100
rf.threshold= which(rf.output$fprs==min.fpr.rf & rf.output$tprs>=0.95)/100

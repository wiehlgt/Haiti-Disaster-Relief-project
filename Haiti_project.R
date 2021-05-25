# Gavin Wiehl (gtw4vx)
# Disaster project
# SYS 6018
# Part 2 begins at line 289

####################################    Exploratory   #######################################

haiti <- read.table('HaitiPixels.csv',sep=",",header = TRUE) # read in the haiti.csv data
attach(haiti) # attach the haiti dataset
fix(haiti) # a quick look into the data
summary(haiti) # summarizing the data
names(haiti) # column names: Class, Red, Green, and Blue
str(haiti) # a look into the datatypes of the variables

plot(Class,las="2")
par(mfrow=(c(1,3)))
boxplot(Red~Class,las="2") # Soil is mostly red, rooftop and various non-tarp have a lot of red. 
boxplot(Green~Class,las="2") # Soil has a lot of green, as well as various non-tarp and blue tarp
boxplot(Blue~Class,las="2") # Blue tarp has the most blue, vegetation the least

# Lets create a binary variable, where blue tent equals 1 and not blue tent equals 0
items = rep(0,dim(haiti)[1])
items[Class=="Blue Tarp"] = 1
items = as.factor(items) # Blue tent should be classified as 1, other/not blue tent should be classified as 0
haiti$is_blu_trp = items
attach(haiti)

####################################    Logit   #######################################

haiti.glm = glm(is_blu_trp~Red+Green+Blue,family="binomial")
summary(haiti.glm)

haiti.glm.inter = glm(is_blu_trp~Red+Green+Blue+Red*Green*Blue,family="binomial")
summary(haiti.glm.inter) # using interaction terms seems to decrease the residual deviance and the AIC

# lets try to find the most appropriate threshhold value
# We want to maximze the number of true blue tents we find, it is better to guess a blue tent and be
# wrong than guess there isn't a blue tent and there actually be one (this is true to a certain extent)
thresh = seq(from=0.05,to=.5,by=0.05)
correct_blu = c()
false_blu = c()
for (i in 1:length(thresh)){
  glm.prob = predict(haiti.glm.inter,haiti,type="response")
  glm.pred<-rep(0,dim(haiti)[1])
  glm.pred[glm.prob >thresh[i]]=1
  correct_blu[i] = c((table(glm.pred,is_blu_trp)[4] / 2022))
  false_blu[i] = c(table(glm.pred,is_blu_trp)[2])
}
correct_blu
false_blu
# I am going to choose a threshhold that keeps the false positive predictions under 200
# lets choose the threshhold as .2
glm.prob = predict(haiti.glm,haiti,type="response")
glm.pred<-rep(0,dim(haiti)[1])
glm.pred[glm.prob >.2]=1
table(glm.pred,is_blu_trp)
mean(glm.pred==is_blu_trp) # the model without interaction has an accuracy of 99.154%

glm.prob.inter = predict(haiti.glm.inter,haiti,type="response")
glm.pred.inter<-rep(0,dim(haiti)[1])
glm.pred.inter[glm.prob.inter >.2]=1
table(glm.pred.inter,is_blu_trp)
mean(glm.pred.inter==is_blu_trp) # the model with interaction has an accuracy of 99.633%
# Here we miss only 50 blue tents

# Lets plot the ROC and print the AUC
par(mfrow=(c(1,1)))
library(pROC)
g <- roc(is_blu_trp~glm.pred, plot = TRUE, print.auc = TRUE)
plot(g,main="Logistic Regression ROC")
# The AUC is 0.986

# lets cross validate with K=10 folds
n = nrow(haiti)
permutation = sample(n)
slice = n/10
acc = 0
tru_blu = 0
for (i in 1:10) {
  test = permutation[((i-1)* slice + 1) : (i*slice)]
  train = c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])
  haiti.glm.inter = glm(is_blu_trp~Red+Green+Blue+Red*Green*Blue,family="binomial",subset=train)
  glm.prob = predict(haiti.glm.inter, haiti[test,])
  glm.pred<-rep(0,length(test))
  glm.pred[glm.prob >.2]= 1 
  acc = acc + mean(glm.pred==is_blu_trp[test])
  tru_blu = tru_blu + ((table(glm.pred,is_blu_trp[test])[4]) / (table(glm.pred,is_blu_trp[test])[3]+table(glm.pred,is_blu_trp[test])[4]))
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here is the average accuracy for the 10-fold cross validation. 99.57 percent accuracy is quite good.
tru_blu # Here is the sensitivity at 91.03 percent. This is the true positive over all over the total observed  blue tents


####################################    K-nearest neighbors   #######################################


library(class)
set.seed(1)
k_seq <- seq(3,51,by=4) # we are going to look at a spread of different k's starting at 3 and ending at 51. We will go by 4 each time.
acc_df <- data.frame(matrix(ncol = 2, nrow = length(k_seq)),row.names = k_seq) # a dataframe to hold the accuracy metrics for different k's
colnames(acc_df) <- c("acc","sensitivity") # renaming the columns properly
for (j in 1:length(k_seq)){
  acc = 0 
  tru_blu = 0
  for (i in 1:10) {
    test = na.omit(permutation[((i-1)* slice + 1) : (i*slice)]) # make sure there is no NA's
    train = na.omit(c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])) # make sure there is no NA's
    train.X<-cbind(Green[train],Red[train],Blue[train])
    test.X<-cbind(Green[test],Red[test],Blue[test])
    train.is_blu_trp<-is_blu_trp[train]
    knn.pred<-knn(train.X,test.X,train.is_blu_trp,k=k_seq[j])
    acc = acc + mean(knn.pred==is_blu_trp[test])
    tru_blu = tru_blu + ((table(knn.pred,is_blu_trp[test])[4]) / (table(knn.pred,is_blu_trp[test])[3]+table(knn.pred,is_blu_trp[test])[4]))
  }
  acc = acc/10 # get the average accuracy of certain k
  tru_blu = tru_blu/10 # get the average sensitivity of certain k
  acc_df[j,"acc"] = acc
  acc_df[j,"sensitivity"] = tru_blu
}

acc_df
which.max(acc_df$acc)
which.max(acc_df$sensitivity)
# Here we find the most accurate k is equal to 7. The average test accuracy is 99.73 percent,
# and the best sensitivity is equal to k=7. The average test sensitivity is 96.17 percent.
# Since I am looking to max out the sensitivity, we will pick k=7 for the KNN model.
# (sometimes we get models with k=3 or k=11)


# Lets plot the ROC and print the AUC
# This time with the ROCR package
library(ROCR)
library(base)
par(mfrow=(c(1,1)))

n = nrow(haiti)
permutation = sample(n)
slice = n/10
acc = 0
test = na.omit(permutation[((1-1)* slice + 1) : (1*slice)]) # make sure there is no NA's
train = na.omit(c(permutation[1:((1-1) * slice)], permutation[(1 * slice + 1):n])) # make sure there is no NA's
train.X<-cbind(Green[train],Red[train],Blue[train])
test.X<-cbind(Green[test],Red[test],Blue[test])
train.is_blu_trp<-is_blu_trp[train]
knn.pred<-knn(train.X,test.X,train.is_blu_trp,k=7,prob=TRUE)
knn.prob = attr(knn.pred, 'prob')
knn.probof1 = knn.prob * (knn.pred==1) + (1-knn.prob) * (knn.pred==0)
roc_knn = prediction(knn.probof1, is_blu_trp[test])
roc_knn = performance(roc_knn, "tpr", "fpr")
plot(roc_knn,main="Knn ROC")
#Here's the AUC
roc_knn = prediction(knn.probof1, is_blu_trp[test])
auc = performance(roc_knn, measure = "auc")
auc@y.values
# the AUC is 0.999

####################################    LDA Model   #######################################

# Lets check the assumptions for the LDA model first
par(mfrow=(c(1,3)))

hist(Green)
hist(Red)
hist(Blue) # None of the predictor variables look normal. Lets check within classes

par(mfrow=(c(1,3)))

hist(haiti[haiti$is_blu_trp==1,]$Green,xlab="Blue Tent",main="Blue Tent, Green distribution") # Green is somewhat normal
hist(haiti[haiti$is_blu_trp==1,]$Red,xlab="Blue Tent",main="Blue Tent, Red distribution") # Red is the most normal
hist(haiti[haiti$is_blu_trp==1,]$Blue,xlab="Blue Tent",main="Blue Tent, Blue distribution") # Blue is not normal at all

par(mfrow=(c(1,3)))

hist(haiti[haiti$is_blu_trp==0,]$Green,xlab="Not Blue Tent",main="Not Blue Tent, Green distribution") # Green is not normal at all
hist(haiti[haiti$is_blu_trp==0,]$Red,xlab="Not Blue Tent",main="Not Blue Tent, Red distribution") # Red is not normal at all
hist(haiti[haiti$is_blu_trp==0,]$Blue,xlab="Not Blue Tent",main="Not Blue Tent, Blue distribution") # Blue is not normal at all

# It does not look like the normality assumption holds


boxplot(Green~is_blu_trp)
boxplot(Red~is_blu_trp)
boxplot(Blue~is_blu_trp)
# The lengths of the boxplots differ greatly, showing us that the variances differ for each class


# Here is a simple LDA model with Green, Red, and Blue as the predictors
library(MASS)
n = nrow(haiti)
permutation = sample(n)
slice = n/10
acc = 0
tru_blu = 0
for (i in 1:10) {
  test = permutation[((i-1)* slice +1) : (i*slice)]
  train = c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])
  lda.fit = lda(is_blu_trp~Green+Red+Blue, data=haiti, subset=train)
  lda.pred = predict(lda.fit, haiti[test,])
  acc = acc + mean(lda.pred$class==haiti[test,]$is_blu_trp)
  tru_blu = tru_blu + ((table(lda.pred$class,is_blu_trp[test])[4]) / (table(lda.pred$class,is_blu_trp[test])[3]+table(lda.pred$class,is_blu_trp[test])[4]))
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we have a 98.39 percent estimated test accuracy
tru_blu # and here we have a 80.31 percent estimated sensitivity rate

# Lets try it with sum interaction terms

acc = 0
tru_blu = 0
for (i in 1:10) {
  test = permutation[((i-1)* slice +1) : (i*slice)]
  train = c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])
  lda.fit = lda(is_blu_trp~Green+Red+Blue+Green*Red*Blue, data=haiti, subset=train)
  lda.pred = predict(lda.fit, haiti[test,])
  acc = acc + mean(lda.pred$class==haiti[test,]$is_blu_trp)
  tru_blu = tru_blu + ((table(lda.pred$class,is_blu_trp[test])[4]) / (table(lda.pred$class,is_blu_trp[test])[3]+table(lda.pred$class,is_blu_trp[test])[4]))
}
acc = acc/10
tru_blu = tru_blu/10
acc # for the interaction model, we have a 99.36 percent accuracy rate
tru_blu # and we have a 82.11 percent estimated sensitivity rate


# Lets plot the ROC and print the AUC.
# Lets do the training on 9/10ths of the data, while testing on 1/10th of the data
n = nrow(haiti)
permutation = sample(n)
slice = n/10
test = permutation[((1-1)* slice +1) : (1*slice)]
train = c(permutation[1:((1-1) * slice)], permutation[(1 * slice + 1):n])
lda.fit = lda(is_blu_trp~Green+Red+Blue+Green*Red*Blue, data=haiti, subset=train)
lda.pred = predict(lda.fit, haiti[test,])
par(mfrow=(c(1,1)))
g <- roc(is_blu_trp[test]~lda.pred$posterior[,"1"], plot = TRUE, print.auc = TRUE)
plot(g,main="LDA ROC plot")
# The AUC is 0.997

####################################    QDA Model   #######################################

acc = 0
tru_blu = 0
for (i in 1:10) {
  test = permutation[((i-1)* slice +1) : (i*slice)]
  train = c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])
  qda.fit = qda(is_blu_trp~Green+Red+Blue, data=haiti, subset=train)
  qda.pred = predict(qda.fit, haiti[test,])
  acc = acc + mean(qda.pred$class==haiti[test,]$is_blu_trp)
  tru_blu = tru_blu + ((table(qda.pred$class,is_blu_trp[test])[4]) / (table(qda.pred$class,is_blu_trp[test])[3]+table(qda.pred$class,is_blu_trp[test])[4]))
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 99.46 percent
tru_blu # and here we see an estimated test sensitivity rate of 84.00 percent

# Next lets try QDA with interaction terms

acc = 0
tru_blu = 0
for (i in 1:10) {
  test = permutation[((i-1)* slice +1) : (i*slice)]
  train = c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n])
  qda.fit = qda(is_blu_trp~Green+Red+Blue+Green*Red*Green, data=haiti, subset=train)
  qda.pred = predict(qda.fit, haiti[test,])
  acc = acc + mean(qda.pred$class==haiti[test,]$is_blu_trp)
  tru_blu = tru_blu + ((table(qda.pred$class,is_blu_trp[test])[4]) / (table(qda.pred$class,is_blu_trp[test])[3]+table(qda.pred$class,is_blu_trp[test])[4]))
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 99.46 percent
tru_blu # and here we see an estimated test sensitivity rate of 86.58 percent,


# Lets plot the ROC and print the AUC.
# Lets do the training on 9/10ths of the data, while testing on 1/10th of the data
n = nrow(haiti)
permutation = sample(n)
slice = n/10
test = permutation[((1-1)* slice +1) : (1*slice)]
train = c(permutation[1:((1-1) * slice)], permutation[(1 * slice + 1):n])
qda.fit = qda(is_blu_trp~Green+Red+Blue+Green*Red*Blue, data=haiti, subset=train)
qda.pred = predict(qda.fit, haiti[test,])
par(mfrow=(c(1,1)))
g <- roc(is_blu_trp[test]~qda.pred$posterior[,"1"], plot = TRUE, print.auc = TRUE)
plot(g,main="QDA ROC plot")
# The AUC is 0.999

####################################    Random Forest   #######################################

# Since random forest produces Out-of-the-bag test error predictions
# 10-fold cross validation is normally not necessary, however we'll go ahead and try
# lets begin with a 50-50 test/train split
library(randomForest)
set.seed(1)
train = sample(1:nrow(haiti),nrow(haiti)/2)

# number of trees = 1000 and number of features is 1
rf.haiti=randomForest(is_blu_trp~Red+Blue+Green,data=haiti,subsets=train,
                        mtry=1,ntree=1000,importance=TRUE)
rf.haiti
plot(rf.haiti)
rf.haiti$importance 
# here we see that Red is most important feature for increasing accuracy, since 
# on average the accuracy drops 0.1616 without that feature. This is followed by Blue and
# then followed by Green. Blue causes a signicant decrease in GINI when left out of the model, which makes sense


yhat.rf=predict(rf.haiti,newdata=haiti[-train,])
haiti.test=haiti[-train,"is_blu_trp"]
mean((yhat.rf==haiti.test))

# The OOB accuracy estimate is 1-0.3 = 99.699%,
# while the validation test set estimate is 99.946%
# Lets try another with interaction terms

set.seed(1)

# number of trees = 1000 and number of features availabe per tree is 1
rf.haiti=randomForest(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti,subsets=train,mtry=1
                      ,ntree=1000,importance=TRUE)
rf.haiti
plot(rf.haiti) # here we see the error go down and is stable as the number of trees approaches 1000, I am confortable with 1000 trees

yhat.rf=predict(rf.haiti,newdata=haiti[-train,])
haiti.test=haiti[-train,"is_blu_trp"]
mean((yhat.rf==haiti.test))

# The OOB accuracy estimate is 1-.28= 99.71 percent, a slight improvement
# The validation test set estimate is 99.945, same as the model without interaction terms
# lets try the same thing, but with the number of features available per tree as 2
# if this does not decrease the OOB error rate, lets stick with mtry=1
set.seed(1)

# number of trees = 1000 and number of features availabe per tree is 2
rf.haiti2=randomForest(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti,subsets=train,mtry=2
                      ,ntree=1000,importance=TRUE)
rf.haiti2
# The OOB error rate is 0.3%, much like the model without interaction terms
# So we will stick with mtry=1

# Now lets try 10-fold cross validation
set.seed(1)
acc = 0
tru_blu = 0
for (i in 1:10) {
  test = na.omit(permutation[((i-1)* slice +1) : (i*slice)])
  train = na.omit(c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n]))
  # number of trees = 1000 and number of features availabe per tree is 1
  rf.haiti=randomForest(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti,
                        subsets=train,mtry=1,ntree=1000,importance=TRUE)
  yhat.rf=predict(rf.haiti,newdata=haiti[test,])
  haiti.test=haiti[test,"is_blu_trp"]
  acc= acc + mean((yhat.rf==haiti.test))
  rf.table = table(yhat.rf,haiti.test)
  tru_blu = tru_blu + (rf.table[4]) / (rf.table[3]+rf.table[4])
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 99.945 percent
tru_blu # and here we see an estimated test sensitivity rate of 98.404 percent

predictions=as.vector(rf.haiti$votes[,2])
pred=prediction(predictions,is_blu_trp)

perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]

perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="RF ROC plot")
lines(x = c(0,1), y = c(0,1),col="blue")
text(0.3,0.8,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
# Here we see the AUC is .999

####################################   SVM-linear   #######################################
# With both interaction and non interaction terms

train = sample(1:nrow(haiti),nrow(haiti)/2)
haiti.test = haiti[-train,]

haiti.svm=svm(is_blu_trp~Red+Blue+Green,data=haiti,subset=train,kernel="linear",cost=10,scale=TRUE)

ypred=predict(haiti.svm ,haiti.test)
table(predict=ypred , truth= haiti.test$is_blu_trp)
mean(ypred==haiti.test$is_blu_trp)
# the accuracy for the model with no interaction is 99.5414 percent

# SVM with interaction terms

train = sample(1:nrow(haiti),nrow(haiti)/2)
haiti.test = haiti[-train,]

haiti.svm.inter=svm(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti,subset=train,kernel="linear",cost=10,scale=TRUE)

ypred.inter=predict(haiti.svm.inter ,haiti.test)
table(predict=ypred.inter , truth= haiti.test$is_blu_trp)
mean(ypred.inter==haiti.test$is_blu_trp)
# including interaction terms improved the accuracy to 99.576 percent

# Now we will cross validate the non-interaction terms model to 
# find out the optimal cost parameter

set.seed(1)
tune.out=tune(svm,is_blu_trp~Red+Blue+Green,data=haiti,kernel ="linear",scaled=TRUE,
              ranges=list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100,1000,5000,10000))) #here we try different cost parameters

summary(tune.out)
bestmod=tune.out$best.model
bestperf=tune.out$best.performance
summary(bestmod) # the best cost parameter was 5000
bestperf # the best error rate was 0.458 percent
ypred=predict(bestmod ,haiti)
table(predict=ypred ,truth= haiti$is_blu_trp)
mean(ypred==haiti$is_blu_trp) # accuracy is 99.533 percent

# Now let's try the same process with interaction terms
set.seed(1)
tune.out.inter=tune(svm,is_blu_trp~Red+Blue+Green*Red*Blue*Green,data=haiti,kernel ="linear",scaled=TRUE,
                    ranges=list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100,1000,5000,10000))) #here we try different cost parameters

summary(tune.out.inter)
bestmod.inter=tune.out.inter$best.model
bestperf.inter=tune.out.inter$best.performance
summary(bestmod.inter) # the best cost parameter was 5000
bestperf.inter # the best error rate was 0.3637 percent
ypred.inter=predict(bestmod ,haiti)
table(predict=ypred.inter ,truth=haiti$is_blu_trp)
mean(ypred.inter==haiti$is_blu_trp) # accuracy is 99.533 percent


# the best model has interactions and has a cost parameter of 5000 and estimated accuracy of 99.6363 percent on cv and 99.533 on the whole dataset
# Now lets try 10-fold cross validation the same way as the other models
# to get the estimate of the test accuracy and sensitivity and make sure its the same/similar

set.seed(1)
acc = 0
tru_blu = 0
for (i in 1:10){
  test = na.omit(permutation[((i-1)* slice +1) : (i*slice)])
  train = na.omit(c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n]))
  haiti.svm2=svm(is_blu_trp~Red+Blue+Green*Red*Blue*Green,data=haiti,subset=train,kernel="linear",cost=5000,scale=TRUE)
  yhat.svm=predict(haiti.svm2,newdata=haiti[test,])
  haiti.test=haiti[test,"is_blu_trp"]
  acc= acc + mean((yhat.svm==haiti.test))
  svm.table = table(yhat.svm,haiti.test)
  tru_blu = tru_blu + (svm.table[4]) / (svm.table[3]+svm.table[4])
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 99.628 percent
tru_blu # and here we see an estimated test sensitivity rate of 94.699 percent

p=predict(bestmod.inter,haiti[test,], type="response")
pred=prediction(as.numeric(p), haiti[test,]$is_blu_trp)
perf=performance(pred,'tpr','fpr')
plot(perf)
lines(x = c(0,1), y = c(0,1),col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# the AUC for the svm model with interaction is 0.9805

####################################   SVM-polynomial   #######################################

set.seed(1)
train = sample(1:nrow(haiti),nrow(haiti)/2)
haiti.test = haiti[-train,]

# Now we will cross validate the non-interaction terms model to 
# find out the optimal cost and degree parameter

poly.out=tune(svm,is_blu_trp~Red+Blue+Green,data=haiti,kernel ="polynomial",scaled=TRUE,
              ranges=list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100,1000,5000),degree=c(2,3,4,5)))

summary(poly.out)
bestmod.poly=poly.out$best.model #best parameters are a cost of 5000 and degree 3
bestperf.poly=poly.out$best.performance
summary(bestmod.poly) # the best cost parameter was 1000
bestperf.poly # the best error rate was 0.458 percent
ypred.poly=predict(bestmod.poly ,haiti) # best error rate was 0.4127 percent
table(predict=ypred.poly ,truth=haiti$is_blu_trp)
mean(ypred.poly==haiti$is_blu_trp) # accuracy is 99.597 percent

# Now lets try 10-fold cross validation the same way as the other models
# to get the estimate of the test accuracy and sensitivity and make sure its the same/similar

set.seed(1)
acc = 0
tru_blu = 0
for (i in 1:10){
  test = na.omit(permutation[((i-1)* slice +1) : (i*slice)])
  train = na.omit(c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n]))
  haiti.svmp=svm(is_blu_trp~Red+Blue+Green,data=haiti,subset=train,kernel="polynomial",cost=5000,scale=TRUE,degree=3)
  yhat.svmp=predict(haiti.svmp,newdata=haiti[test,])
  haiti.test=haiti[test,"is_blu_trp"]
  acc= acc + mean((yhat.svm==haiti.test))
  svmp.table = table(yhat.svmp,haiti.test)
  tru_blu = tru_blu + (svm.table[4]) / (svm.table[3]+svm.table[4])
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 94.47 percent
tru_blu # and here we see an estimated test sensitivity rate of 96.296 percent

p=predict(bestmod.poly,haiti[test,], type="response")
pred=prediction(as.numeric(p), haiti[test,]$is_blu_trp)
perf=performance(pred,'tpr','fpr')
plot(perf)
lines(x = c(0,1), y = c(0,1),col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# the AUC for the svm model with interaction is 0.9650

####################################   SVM-radial   #######################################


set.seed(1)
train = sample(1:nrow(haiti),nrow(haiti)/2)
haiti.test = haiti[-train,]

# Now we will cross validate the non-interaction terms model to 
# find out the optimal cost and gamma parameter

rad.out=tune(svm,is_blu_trp~Red+Blue+Green,data=haiti,kernel ="radial",scaled=TRUE,
              ranges=list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100,1000,5000),gamma=c(0.5,1,2,3,4)))

summary(rad.out)
bestmod.rad=rad.out$best.model
bestperf.rad=rad.out$best.performance
summary(bestmod.rad) # the best cost parameter was 5000
bestmod.rad$gamma # the best gamma parameter is 4
bestperf.rad # the best error rate was 0.255 percent
ypred.rad=predict(bestmod.rad ,haiti)
table(predict=ypred.rad ,truth=haiti$is_blu_trp)
mean(ypred.rad==haiti$is_blu_trp) # accuracy is 99.788 percent

# Now lets try 10-fold cross validation the same way as the other models
# to get the estimate of the test accuracy and sensitivity and make sure its the same/similar

set.seed(1)
acc = 0
tru_blu = 0
for (i in 1:10){
  test = na.omit(permutation[((i-1)* slice +1) : (i*slice)])
  train = na.omit(c(permutation[1:((i-1) * slice)], permutation[(i * slice + 1):n]))
  haiti.svmr=svm(is_blu_trp~Red+Blue+Green,data=haiti,subset=train,kernel="radial",cost=5000,gamma=4,scale=TRUE)
  yhat.svmr=predict(haiti.svmr,newdata=haiti[test,])
  haiti.test=haiti[test,"is_blu_trp"]
  acc= acc + mean((yhat.svmr==haiti.test))
  svmr.table = table(yhat.svmr,haiti.test)
  tru_blu = tru_blu + (svmr.table[4]) / (svmr.table[3]+svmr.table[4])
}
acc = acc/10
tru_blu = tru_blu/10
acc # Here we see an estimated test accuracy rate of 99.731 percent
tru_blu # and here we see an estimated test sensitivity rate of 96.46 percent

p=predict(bestmod.rad,haiti[test,], type="response")
pred=prediction(as.numeric(p), haiti[test,]$is_blu_trp)
perf=performance(pred,'tpr','fpr')
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
plot(perf,main="SVM-radial ROC")
lines(x = c(0,1), y = c(0,1),col="blue")
text(0.3,0.8,paste("AUC = ",format(auc_ROCR, digits=5, scientific=FALSE)))
# the AUC for the svm model-radial model is 0.9888

####################################  Hold-out   #######################################

hold_out = read.csv("holdout.csv", header = TRUE)
hold_out
detach(haiti)
attach(hold_out)

par(mfrow=(c(1,3)))

hist(Green)
hist(Red)
hist(Blue) # The hold out data looks somewhat normal

par(mfrow=(c(1,3)))

hist(hold_out[hold_out$is_blu_trp==1,]$Green,xlab="Blue Tent",main="Blue Tent, Green distribution") # Green is somewhat normal
hist(hold_out[hold_out$is_blu_trp==1,]$Red,xlab="Blue Tent",main="Blue Tent, Red distribution") # Red is somewhat normal
hist(hold_out[hold_out$is_blu_trp==1,]$Blue,xlab="Blue Tent",main="Blue Tent, Blue distribution") # Blue is somewhat normal

par(mfrow=(c(1,3)))

hist(hold_out[hold_out$is_blu_trp==0,]$Green,xlab="Not Blue Tent",main="Not Blue Tent, Green distribution") # Green is somewhat normal
hist(hold_out[hold_out$is_blu_trp==0,]$Red,xlab="Not Blue Tent",main="Not Blue Tent, Red distribution") # Red is somewhat normal 
hist(hold_out[hold_out$is_blu_trp==0,]$Blue,xlab="Not Blue Tent",main="Not Blue Tent, Blue distribution") # Blue is not very normal

# The normality assumption for LDA/QDA holds better for the hold out data than the training data


boxplot(Green~is_blu_trp)
boxplot(Red~is_blu_trp)
boxplot(Blue~is_blu_trp)
# The lengths of the boxplots differ mostly for the color Blue. Red variance looks similar, as does Green.


#################################### Hold-out Logit #######################################

haiti.glm.hold = glm(is_blu_trp~Red+Green+Blue+Red*Green*Blue,data=haiti,family="binomial")
hold.inter.prob = predict(haiti.glm.hold,hold_out,type="response")
hold.inter.pred<-rep(0,dim(hold_out)[1])
hold.inter.pred[hold.inter.prob >.2]=1
table(hold.inter.pred,is_blu_trp)
mean(hold.inter.pred==is_blu_trp) # Hold-out accuracy is 99.21%
14100/(14100+380) # sensitivity is 97.38%
# this model predicts 15389 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
g <- roc(is_blu_trp~hold.inter.pred, plot = TRUE, print.auc = TRUE)
plot(g,main="Logistic Regression ROC")
# The AUC is 0.983

#################################### Hold-out KNN #######################################

train.X.hold<-cbind(haiti$Red,haiti$Green,haiti$Blue)
test.X.hold<-cbind(hold_out$Red,hold_out$Green,hold_out$Blue)
train.is_blu_trp.haiti<-haiti$is_blu_trp
knn.pred.hold<-knn(train.X.hold,test.X.hold,train.is_blu_trp.haiti,k=7,prob=TRUE)

table(knn.pred.hold,hold_out$is_blu_trp)
mean(knn.pred.hold==hold_out$is_blu_trp) # accuracy is 99.23%
(12020)/(12020+2460) # sensitivity is 83.01%
# this model predicts 12970 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
hold.prob = attr(knn.pred.hold, 'prob')
hold.probof1 = hold.prob * (knn.pred.hold==1) + (1-hold.prob) * (knn.pred.hold==0)
roc_hold = prediction(hold.probof1, is_blu_trp)
roc_hold = performance(roc_hold, "tpr", "fpr")
plot(roc_hold,main="Knn ROC")
#Here's the AUC
roc_hold = prediction(hold.probof1, is_blu_trp)
auc.hold = performance(roc_hold, measure = "auc")
auc.hold@y.values
# AUC is 0.952
#################################### Hold-out LDA #######################################

lda.fit.inter = lda(is_blu_trp~Green+Red+Blue+Green*Red*Blue, data=haiti)
lda.pred.hold = predict(lda.fit.inter, hold_out)
table(lda.pred.hold$class,is_blu_trp)
mean(lda.pred.hold$class==is_blu_trp) # the accuracy is 99.73%
(13210)/(13210+1270) # sensitivity is 91.23%
# this model predicts 4089 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
g.hold <- roc(is_blu_trp~lda.pred.hold$posterior[,"1"], plot = TRUE, print.auc = TRUE)
plot(g.hold,main="LDA ROC plot")
# AUC is 0.998

#################################### Hold-out QDA #######################################

qda.fit.inter = qda(is_blu_trp~Green+Red+Blue+Green*Red*Green, data=haiti)
qda.pred.hold = predict(qda.fit.inter, hold_out)
table(qda.pred.hold$class,is_blu_trp)
mean(qda.pred.hold$class==is_blu_trp) # the accuracy is 99.39%
(8476)/(8476+6004) # sensitivity is 58.54%
# this model predicts 6196 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
q.hold <- roc(is_blu_trp~qda.pred.hold$posterior[,"1"], plot = TRUE, print.auc = TRUE)
plot(q.hold,main="QDA ROC plot")
# AUC is 0.794

#################################### Hold-out Random Forest #######################################

rf.haiti.inter=randomForest(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti
                      ,mtry=1,ntree=1000,importance=TRUE)
yhat.rf.hold=predict(rf.haiti.inter,newdata=hold_out)
table(yhat.rf.hold,is_blu_trp)
mean(yhat.rf.hold==is_blu_trp) # the accuracy is 99.48%
(11364)/(11364+3116) # the sensitivity is 78.48%
# this model predicts 7233 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
pred.hold=predict(rf.haiti.inter,newdata=hold_out,type="prob")
predictions.hold=as.vector(pred.hold[,2])
pred.hold=prediction(predictions.hold,is_blu_trp)
perf_ROC.hold=performance(pred.hold,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC.hold, main="RF ROC plot")
lines(x = c(0,1), y = c(0,1),col="blue")
text(0.3,0.8,paste("AUC = ",format(AUC.hold, digits=5, scientific=FALSE)))
# Here we see the AUC is 0.982

#################################### Hold-out SVM-linear #######################################

haiti.svm.hold=svm(is_blu_trp~Red+Blue+Green+Red*Blue*Green,data=haiti
                    ,kernel="linear",cost=5000,scale=TRUE)
ypred.inter.hold=predict(haiti.svm.hold ,hold_out)
table(ypred.inter.hold ,is_blu_trp)
mean(ypred.inter.hold==is_blu_trp) # the accuracy is 99.31%
(14227)/(14227+253) # the sensitivity is 98.25%
# this model predicts 13642 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
l=predict(haiti.svm.hold,hold_out, type="response")
pred=prediction(as.numeric(l), is_blu_trp)
perf=performance(pred,'tpr','fpr')
plot(perf)
lines(x = c(0,1), y = c(0,1),col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# the AUC is 0.988

#################################### Hold-out SVM-polynomial #######################################

haiti.svmp.hold=svm(is_blu_trp~Red+Blue+Green,data=haiti,kernel="polynomial",cost=5000,scale=TRUE,degree=3)
yhat.svmp.hold=predict(haiti.svmp.hold,newdata=hold_out)
table(yhat.svmp.hold,is_blu_trp)
mean(yhat.svmp.hold==is_blu_trp) # the accuracy is 99.18%
(14348)/(14348+132) # the sensitivity is 99.09
# this model predicts 16218 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
p=predict(haiti.svmp.hold,hold_out, type="response")
pred=prediction(as.numeric(p), is_blu_trp)
perf=performance(pred,'tpr','fpr')
plot(perf)
lines(x = c(0,1), y = c(0,1),col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# the AUC is 0.991

#################################### Hold-out SVM-radial #######################################

haiti.svmr.hold=svm(is_blu_trp~Red+Blue+Green,data=haiti,kernel="radial",cost=5000,gamma=4,scale=TRUE)
yhat.svmr.hold=predict(haiti.svmr.hold,newdata=hold_out)
table(yhat.svmr.hold,is_blu_trp)
mean(yhat.svmr.hold==is_blu_trp) # the accuracy is 99.07%
(9699)/(9699+4781) # the sensitivity is 66.98%
# this model predicts 13787 pixels as blue tarp when they are not blue tarp

# Now for the AUC and ROC plot
r=predict(haiti.svmr.hold,hold_out, type="response")
pred=prediction(as.numeric(r), is_blu_trp)
perf=performance(pred,'tpr','fpr')
plot(perf)
lines(x = c(0,1), y = c(0,1),col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# the AUC is 0.831

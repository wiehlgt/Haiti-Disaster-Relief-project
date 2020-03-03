# Gavin Wiehl (gtw4vx)
# Disaster project part 1
# SYS 6018

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
glm.prob = predict(haiti.glm.inter,haiti,type="response")
glm.pred<-rep(0,dim(haiti)[1])
glm.pred[glm.prob >.2]=1
table(glm.pred,is_blu_trp)
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

# K-nearest neighbors

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


#LDA Model
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

#QDA Model

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


rm(list=ls(all=TRUE))#to remove all the items


#############################################################################################
##############################K Nearest Neighbors############################################
#############################################################################################
df=data(iris) ##load data
head(iris) ## see the studcture

ran=sample(1:nrow(iris), 0.7 * nrow(iris)) 

X=iris[,-5]
X_mean=apply(X,2,mean)
X_sd=apply(X,2,sd)
iris_norm=(X-as.matrix(rep(1,dim(X)[1]))%*%c(X_mean))/as.matrix(rep(1,dim(X)[1]))%*%c(X_sd)
scale(X)
sum(rowSums((iris_norm-scale(X))^2))

X_mean_by_group=apply(X,2,function(x)tapply(x,iris[,5],mean))


Banana=split(X,iris[,5])
sapply(Banana,function(x)apply(x,2,mean))
sapply(Banana,function(x)apply(x,2,sd))

##extract training set
iris_train=iris_norm[ran,] 
##extract testing set
iris_test=iris_norm[-ran,]

##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category=iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category=iris[-ran,5]

##load the package class
library(class)

##run knn function
pr=knn(iris_train,iris_test,cl=iris_target_category,k=13)

tab=table(pr,iris_test_category)

pr1=knn(iris_train,iris_test,cl=iris_target_category,k=10)

tab1=table(pr1,iris_test_category)
tab1

sum(diag(tab))/sum(tab)*100

library(caret)
confusionMatrix(pr,iris_test_category)

i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:28){ 
  knn.mod=knn(iris_train,iris_test,cl=iris_target_category, k=i)
  tab=table(knn.mod,iris_test_category)
  k.optm[i]=100 * sum(diag(tab))/sum(tab)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

#############################################################################################
##############################K Nearest Neighbors example 2##################################
#############################################################################################

email=read.csv("/Users/hugo/Desktop/Classes/S2/Econometrics II (ECON 422)/RCourse/7 - Classification/spam.csv")
names(email)
X=email[,-58]
n_fold=15
fold=createFolds(c(1:dim(X)[1]), k = n_fold, list = F)

X=scale(X)

k.optim=rep(0,50)

for (i in 1:50){ 
  accuracy=rep(0,n_fold)
  for (j in 1:n_fold)
  {
    ##extract training set
    email_train=X[fold!=j,] 
    ##extract testing set
    email_test=X[fold==j,]
    
    ##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
    email_target_category=email[fold!=j,58]
    ##extract 5th column if test dataset to measure the accuracy
    email_test_category=email[fold==j,58]
    
    knn.mod=knn(email_train,email_test,cl=email_target_category, k=i)
    tab=table(knn.mod,email_test_category)
    accuracy[j]=100 * sum(diag(tab))/sum(tab)
  }
  
  k.optm[i]=mean(accuracy)
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")
k.optm

100*table(email[,58])/dim(email)[1]

#####################################################################################################
#################################################SVM#################################################
#####################################################################################################
set.seed(10111)
library(e1071)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)

dat = data.frame(x, y = as.factor(y))
svmfit = svm(y ~ ., data = dat, kernel = "linear")
print(svmfit)
predict(svmfit)
sum(diag(table(predict(svmfit),y)))/length(y)
plot(svmfit, dat)


email$spam_1=as.factor(ifelse(email$spam==1,1,-1))
email1=email[-58]
svmfit_email = svm(spam_1 ~ ., data=email1, kernel = "linear")
predict(svmfit_email)
sum(diag(table(predict(svmfit_email),email1$spam_1)))/length(email1$spam_1)

svmfit_email1 = svm(spam_1 ~ ., data=email1, kernel = "radial", cost = 5)
sum(diag(table(predict(svmfit_email1),email1$spam_1)))/length(email1$spam_1)

load(file = "/Users/hugo/Desktop/Classes/S2/Econometrics II (ECON 422)/RCourse/7 - Classification/ESL.mixture.rda")
names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)

plot(x, col = y + 1)

dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data = dat, scale = FALSE, kernel = "radial", cost = 5)

xgrid = expand.grid(X1 = px1, X2 = px2)
ygrid = predict(fit, xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2)
points(x, col = y + 1, pch = 19)
rm(list=ls(all=TRUE))#to remove all the items

####################################################################
####################LASSO Regression##################################
####################################################################

## Browsing History. 
## The table has three colums: [machine] id, site [id], [# of] visits
web <- read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/6/browser-domains.csv")
## Read in the actual website names and relabel site factor
sitenames <- scan("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/6/browser-sites.txt", what="character")
web$site <- factor(web$site, levels=1:length(sitenames), labels=sitenames)

## also factor machine id
web$id <- factor(web$id, levels=1:length(unique(web$id)))

## get total visits per-machine and % of time on each site
## tapply(a,b,c) does c(a) for every level of factor b.
machinetotals <- as.vector(tapply(web$visits,web$id,sum)) 
visitpercent <- 100*web$visits/machinetotals[web$id]

## use this info in a sparse matrix
## this is something you'll be doing a lot; familiarize yourself.
xweb <- sparseMatrix(
  i=as.numeric(web$id), j=as.numeric(web$site), x=visitpercent,
  dims=c(nlevels(web$id),nlevels(web$site)),
  dimnames=list(id=levels(web$id), site=levels(web$site)))

# what sites did household 1 visit?
head(xweb[1, xweb[1,]!=0])
# what sites that household 1 didn't visit?
head(xweb[1, xweb[1,]==0])


## now read in the spending data 
yspend <- read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/6/browser-totalspend.csv", row.names=1)  # us 1st column as row names
yspend <- as.matrix(yspend) ## good practice to move from dataframe to matrix
summary(yspend)
## run a lasso path plot
spender <- gamlr(xweb, log(yspend), verb=TRUE)
plot(spender) ## path plot


B <- coef(spender) ## the coefficients selected under AICc

## a few examples
B=B[-1,] # drop intercept and remove STM formatting
B[which.min(B)] ## low spenders spend a lot of time here
B[which.max(B)] ## big spenders hang out here

coef(spender, select=which.min(BIC(spender))) ## and BIC instead

cv.spender <- cv.gamlr(xweb, log(yspend),nfold=10, verb=TRUE)
beta1se <- coef(cv.spender) ## 1se rule; see ?cv.gamlr
betamin <- coef(cv.spender, select="min") ## min cv selection
cbind(beta1se,betamin)[c("tvguide.com","americanexpress.com"),]

## plot them together
par(mfrow=c(1,2))
plot(cv.spender)
plot(cv.spender$gamlr) ## cv.gamlr includes a gamlr object

## log lambdas selected under various criteria
log(spender$lambda[which.min(AICc(spender))])
log(spender$lambda[which.min(AIC(spender))])
log(spender$lambda[which.min(BIC(spender))])
log(cv.spender$lambda.min)
log(cv.spender$lambda.1se)

n=10000
## plot CV results and the various IC
ll <- log(spender$lambda) ## the sequence of lambdas
par(mfrow=c(1,2))
plot(cv.spender)
plot(ll, AIC(spender)/n, 
     xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(spender))], col="orange", lty=3)
abline(v=ll[which.min(BIC(spender))], col="green", lty=3)
abline(v=ll[which.min(AICc(spender))], col="black", lty=3)
points(ll, BIC(spender)/n, pch=21, bg="green")
points(ll, AICc(spender)/n, pch=21, bg="black")
legend("topleft", bty="n",
       fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))


## all metrics, together in a path plot.
plot(spender, col="grey")
abline(v=ll[which.min(AICc(spender))], col="black", lty=2)
abline(v=ll[which.min(AIC(spender))], col="orange", lty=2)
abline(v=ll[which.min(BIC(spender))], col="green", lty=2)
abline(v=log(cv.spender$lambda.min), col="blue", lty=2)
abline(v=log(cv.spender$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
rm(list=ls(all=TRUE))#to remove all the items


#############################################################################################
##############################K Nearest Neighbors############################################
#############################################################################################
df=data(iris) ##load data
head(iris) ## see the studcture
table(iris$Species) # is data.frame with 'Species' factor

plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main="Edgar Anderson's Iris Data")
ran=sample(1:nrow(iris), 0.7 * nrow(iris)) 

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2;
  vertical <- (par("usr")[3] + par("usr")[4]) / 2;
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2))
}
pairs(iris[1:4], main = "Edgar Anderson's Iris Data", pch = 21, bg = c("red","green3","blue")[unclass(iris$Species)], upper.panel=panel.pearson)

X=iris[,-5]
X_mean=apply(X,2,mean)
X_sd=apply(X,2,sd)
iris_norm=(X-as.matrix(rep(1,dim(X)[1]))%*%c(X_mean))/as.matrix(rep(1,dim(X)[1]))%*%c(X_sd)


X_mean_by_group=apply(X,2,function(x)tapply(x,iris[,5],mean))


Banana=split(X,iris[,5])
unlist(Banana)
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
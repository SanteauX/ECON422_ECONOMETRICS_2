rm(list=ls(all=TRUE))#to remove all the items
library(Hmisc)

###############################################################################################
#######################################Regression Models#######################################
###############################################################################################
data<-read.csv("~/Dropbox/Concordia University/Econometrics 422/2020_Winter/20200106/mroz87.txt", sep=";")

head(data)
names(data)

plot(density(data$wage))
data$lwage=log(data$wage)

plot(density(data$lwage))
summary(data)

describe(data)
attach(data)

plot(educ, lwage)
plot(lwage~educ)
plot(lwage~educ,xlab="education in years", ylab="log hourly wage")

lm(lwage~educ)
lm(lwage~educ, data=data)

wage_lm<-lm(lwage~educ)
summary(wage_lm) #shows the output

names(wage_lm)
wage_lm$coefficients
wage_lm$fitted.values
names(summary(wage_lm))
summary(wage_lm)$cov.unscaled

betastar<-coef(wage_lm) #extracts the estimates
betastar
Vbetastar<-vcov(wage_lm) #extracts the covariance matrix of the estimates
Vbetastar
sqrt(diag(Vbetastar))#square roots of the diagonal elements are the standard errors
diag(1,4)
res<-residuals(wage_lm)
fit<-fitted(wage_lm)
sum((res+fit-lwage)^2)
#without the intercept
lm(lwage~educ-1)
summary(lm(lwage~educ-1))
data=data[,-1]
summary(lm(lwage~.,data=data))
#########################################################################################
X=cbind(rep(1,dim(data)[1]),data$educ)
y=data$lwage

solve(t(X)%*%X)%*%t(X)%*%y
betastar

sigma2=sum(res^2)/(dim(data)[1]-2)
sigma2*solve(t(X)%*%X)
Vbetastar
######################################################################################
###########################Bootstrap##################################################
######################################################################################
B=1000
n=dim(data)[1]
B_sample=matrix(sample(c(1:n),n*B,replace=T),nrow=n,ncol=B)
Boot_coef=matrix(0,nrow=2,ncol=B)
for (i in 1:B)
{
  Boot_coef[,i]=coef(lm(data[B_sample[,i],]$lwage~data[B_sample[,i],]$educ)) 
}
Vbetastar
plot(Boot_coef[1,],Boot_coef[2,],xlab="beta0", ylab="beta1",type="p")
cov(t(Boot_coef))

quantile(Boot_coef[2,],c(0.05,0.95))

quantile(Boot_coef[2,],c(0.025,0.975))
###################################################
############# Orange Juice Regression #############
###################################################
## read in the data
oj=read.csv("~/Dropbox/Concordia University/Econometrics 422/2020_Winter/20200120/oj.csv")
head(oj)
levels(oj$brand)
unique(oj$brand)

table(oj$brand)

print(table(oj$brand)/length(oj$brand),digit=4)
print(table(oj$brand)/length(oj$brand),digit=2)

## create some colors for the brands
brandcol <- c("green","red","gold")
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=oj, col=brandcol)
plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand])

## simple regression
reg = glm(log(sales) ~ log(price) + brand, data=oj)

## use the fitted model
summary(reg) ## coef, tests, fit
coef(reg) ## just coefficients

beta <- coef(reg)

plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
     cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))


## Interactions
## note that `*' also adds the main effects automatically
reg_interact = glm(log(sales) ~ log(price)*brand, data=oj)
coef(reg_interact)
## compare brand-specific log(price) slopes to our earlier elasticity (-3.1)
beta <- coef(reg_interact)

plot(log(sales) ~ log(price), data=oj, col=brandcol[oj$brand], 
     cex=.1, pch=20, bty="n")
abline(a=beta[1], b=beta[2], col=brandcol[1], lwd=2)
abline(a=beta[1]+beta[3], b=beta[2]+beta[5], col=brandcol[2], lwd=2)
abline(a=beta[1]+beta[4], b=beta[2]+beta[6], col=brandcol[3], lwd=2)
legend("bottomleft", bty="n", lwd=2, col=brandcol, legend=levels(oj$brand))

## and finally, consider 3-way interactions
ojreg <- glm(log(sales) ~ log(price)*brand*feat, data=oj)
coef(ojreg)
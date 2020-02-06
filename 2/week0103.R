rm(list=ls(all=TRUE))#to remove all the items

y=c(1:100)
mean(y)
c=0
for (i in 1:100)
{
  c=c+y[i]
}
c/length(y)
sum(y)/length(y)

mean((y-mean(y))^2)
var(y)
var(y)*(length(y)-1)/length(y)

y=c(3,4,5,42,18,95)
z=c(1,14,53,52,26,16)
cov(y,z)

b1=cov(y,z)/var(z)
b0=mean(y)-b1*mean(z)
b0
b1

lm(y~z) #linear model#


help(cbind)
X<-cbind(z,y)
X
dim(X)[1]

rbind(z,y)
t(rbind(z,y))

########Generate Random Variables######################
plot(hist(runif(100)))

set.seed(123)
Unif=runif(1000)
plot(hist(Unif))


n<-2000

par(mfrow=c(1,2))
plot(density(rexp(n,3)))

Z=-log(1-runif(n))/3
plot(density(Z))

help(rnorm)
plot(density(rnorm(n,0,1)))
plot(density(rnorm(n,2,3)))

U1=runif(n)
U2=runif(n)
Z1=sqrt(-2*log(U1))*cos(2*pi*U2)
Z2=sqrt(-2*log(U1))*sin(2*pi*U2)  

par(mfrow=c(2,2))
plot(density(Z1))
plot(density(Z2))
plot(density(rnorm(n)))
plot(Z1,Z2)

cor(Z1,Z2)
################################################
########What's the value of Pi##################
################################################
Pi=function(n)
{
  x=runif(n,-1,1)
  y=runif(n,-1,1)
  z=ifelse(sqrt(x^2+y^2)<=1,1,0)
  4*sum(z)/n
}
Pi(100000)
pi
################################################

set.seed(123)
number_obs=200*100
u=rnorm(number_obs)
ubars<-rep(NA,200)
for(i in 1:200)
{
  ubars[i]<-mean(u[1:i*(number_obs/200)])
}

system.time( for(i in 1:200)
{
  ubars[i]<-mean(u[1:i*(number_obs/200)])
}
)
plot(c(1:200),ubars,type="l", col = "skyblue", lwd = 2)
abline(h=0, col = "red",lwd=2)

mean(u)
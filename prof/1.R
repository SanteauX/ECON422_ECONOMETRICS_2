x=1
x

rm(list=ls(all=TRUE))#to remove all the items
?rm
??average
###Short Introduction to R

#Exercise 1
x1<-0.12/0.3
x1=0.12/0.3
x1
x2<-sqrt(2)#square root function
x2
x3<-log(85)
x3
x4<-exp(4)
x4
x5=log(exp(1))

y=c(1,2,3)
y
y=c(1:100)
y
y=seq(from=1,to=100,by=0.5)
?seq
seq(from=1,to=100,length.out=30)

length(y)
c(x1,y)
y[20]
y[20]+y[21]

y=c(1:100)
z=c(20,31,2,4,5)
y[z]
print(x1)
print(c(x1,x2))
print("Hello World")

#######How can we swap the values of a and b?########
a=3
b=4
print(c(a,b))
c=a
a=b
b=c
print(c(a,b))


#Exercise 2
mu<-c(0,0) # or mu<-rep(0,2)
mu
rep(0,2)
rep(c(1:10),5)

w<-rep(NA,200)
w

sigma<-matrix(c(1,0,0,1), nrow=2, ncol=2,byrow=T) 
sigma

Z1=matrix(c(1:4),2,2,byrow=T)
Z1
Z2=matrix(c(1:4),2,2,byrow=F)
Z2
Z3=t(Z1)

Z3-Z2
dim(Z1)

Z1[1,2]
Z1[,1]
Z1[1,]

#Exercise 3
z=c(1:100)
sum(z)
length(z)
z_bar=sum(z)/length(z)
z_bar
sum(z/length(z))
mean(z)

ORANGE=function(z)#####x is the input of the function ORANGE
{
  n=length(z)
  S=0
  for (i in 1:n)
  {
    S=S+z[i]
  }
  S/n
  return (list(input=z,output=S/n,num_obs=n,summ=S))
}
ORANGE(z)
ORANGE(z)$summ
mean(z)
var(z)
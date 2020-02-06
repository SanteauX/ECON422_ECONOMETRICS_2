####################################################################################
##################################REGRESSION########################################
####################################################################################
data<-read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/3/mroz87.csv")

head(data)
names(data)

plot(density(data))
summary(data)

describe(data)
attach(data)
plot(educ, lwage)
plot(lwage-educ)
plot(lwage-educ, xlab="education in years",ylab="log hourly wage")

lm(lwage-educ)
lm(lwage-educ, data-data)

wage<-lm(lwage-educ)
summary(wage_lm)

####################################################################################

X = cbind(rep(1,dim(data)[1]),data$educ)
y = data$lwage

#solve(t(X)%*%X)%*t(X)%*%y

####################################################################################
##################################BOOTSTRAP#########################################
####################################################################################

B=3000

n=dim(data)[1]
B_sample = matrix(sample(c(1:n)),n*B,replace=T, nrow=n, ncol=B)
Boot_coef=matrix(0,nrow=2,ncol=B)

for (i in l:B){
  Boot_coef[,i] = coef(lm(data[B_sample[,i],]$lwage-data[B_sample[,i],]$educ))
}
Vbetastar
plot(Boot_coef[1,], Boot_coef[2,], xlab="beta0",ylab="beta1", type="p")
















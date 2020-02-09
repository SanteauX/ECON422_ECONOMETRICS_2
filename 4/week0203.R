rm(list=ls(all=TRUE))#to remove all the items

#############################################################################################################
###########################Coronavirus#######################################################################
#############################################################################################################
library(remotes)
install_github("Guangchuangyu/nCov2019")
##################https://github.com/GuangchuangYu/nCov2019############################
library(nCov2019)
x=get_nCov2019()
x=load_nCov2019()
head(x[])
levels(x[]$province)
x[levels(as.factor(x[]$province))[1],]
#########################################################################################
library(ggplot2)
require(ggrepel)

d=x['??????',]

ggplot(d,
       aes(time, as.numeric(cum_confirm), group=city, color=city)) +
  geom_point() + geom_line() +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL)
###################################################################################################
###############################Binary Choice Data####################################################
######################################################################################################

set.seed(123)
n=10000

price=rexp(n,1/3)
y=2-0.3*price+rnorm(n)
decision=ifelse(y>=0,1,0)

table(decision)

Probit=glm(decision~price,family =binomial(link = "probit"))

summary(Probit)

Logit=glm(decision~price,family =binomial(link = "logit"))
summary(Logit)

pred_p=predict(Probit,type="response")
pred_p=c(pred_p)
p_hat=ifelse(pred_p>0.5,1,0)

Tab=table(ifelse(pred_p>0.5,1,0),decision)
Tab
sum(diag(Tab))/n*100

pred_p=predict(Logit,type="response")
Tab=table(ifelse(pred_p>0.5,1,0),decision)
Tab
sum(diag(Tab))/n*100
####Demand curve simulation#####
seq_p=seq(0.5,20,by=0.5)
length(seq_p)
Demand=rep(0,length(seq_p))
coef(Probit)
for( i in 1:length(seq_p))
{
  Demand[i]=sum(ifelse(coef(Probit)+coef(Probit)*seq_p[i]+rnorm(n)>0.5,1,0))
}

plot(seq_p,Demand,"l")
############################################################################
####################Bootstrap variance-covariance###########################
############################################################################

Vbetastar=vcov(Probit)
B=1000
B_sample=matrix(sample(c(1:n),n*B,replace=T),nrow=n,ncol=B)
Boot_coef=matrix(0,nrow=2,ncol=B)
for (i in 1:B)
{
  Boot_coef[,i]=coef(glm(decision[B_sample[,i]]~price[B_sample[,i]],
                         family =binomial(link = "probit"))) 
}
Vbetastar
plot(Boot_coef[1,],Boot_coef[2,],xlab="beta0", ylab="beta1",type="p")
cov(t(Boot_coef))
vcov(Probit)

############################################################################
####### Logistic regression: Spam data######################################
############################################################################
email <- read.csv("~/Dropbox/Concordia University/Econometrics 422/2020_Winter/20200120/spam.csv")
head(email)
names(email)
## fit the full model
spammy <- glm(spam ~ ., data=email, family='binomial')
## you don't need to worry about this warning.  
## It says that some covariates are nearly perfect predictors.


## the guy is named george
table(email$spam, email$word_george)
table(email$spam, email$word_free)


## the coefficients
b <- coef(spammy)
exp(b["word_george"]) # George => !SPAM
exp(b["word_free"]) # Free => SPAM

phat=predict(spammy,type="response")
spam_hat=ifelse(phat>0.5,1,0)
sum(diag(table(email$spam,spam_hat)))/length(spam_hat)*100

library(texreg)
screenreg(spammy)
plotreg(spammy)

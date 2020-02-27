rm(list=ls(all=TRUE))#to remove all the items

Wage<-read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/Ressources/mroz87.csv", sep=";")
Wage$wage<-log(Wage$wage)
names(Wage)
LM_full=lm(wage~., data=Wage)
summary(LM_full)
deviance(LM_full)

1-deviance(LM_full)/(420*var(Wage$wage))
#1-deviance(LM_full)/((dim(Wage)[1]-1)*var(Wage*wage))


#############################################################################################
###############In sample performance and out of sample prediction############################
#############################################################################################
email <- read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/Ressources/spam.csv")

spammy_full <- glm(spam ~ ., data=email, family='binomial')
logLik(spammy_full)

spammy_null <- glm(spam ~1, data=email, family='binomial')
logLik(spammy_null)

R_mck=1-logLik(spammy_full)/logLik(spammy_null)

1-deviance(spammy_full)/deviance(spammy_null)

############################ K FOL D#########################################################

email1=email[-1,]
n=dim(email1)[1]

K=10

Select=data.frame(ind=sample(c(1:n),n),fold=rep(1:K,n/K))
table(Select$fold)

library(caret)
createFolds(c(1:n), k = 10, list = F)

R2=rep(0,K)

for (i in 1:K)
{
  newData=email1[Select$ind[Select$fold!=i],]
  out_newData=email1[Select$ind[Select$fold==i],]
  model=glm(spam ~ ., data=newData, family='binomial')
  pred=predict(model,newdata = out_newData,type="response")
  dv1=sum(log(pred[out_newData$spam==1])+log(1-pred[out_newData$spam==0]))
  dv0=sum(log(ifelse(out_newData$spam==1,mean(out_newData$spam),1-mean(out_newData$spam))))
  R2[i]=1-dv1/dv0
}

plot(hist(R2,breaks=10))
R2
sum((R2-mean(R2))^2)


R2_1=rep(0,K)
for (i in 1:K)
{
  newData=email1[Select$ind[Select$fold!=i],]
  out_newData=email1[Select$ind[Select$fold==i],]
  model=glm(spam ~ word_remove + char_dollar + word_hp + word_edu + 
              word_george + word_free + capital_run_length_longest + word_meeting + 
              word_money + word_our + word_1999 + word_re + word_internet + 
              word_conference + word_cs + word_business + word_people + 
              word_project + word_650 + word_85 + word_000 + word_will, data=newData, family='binomial')
  pred=predict(model,newdata = out_newData,type="response")
  dv1=sum(log(pred[out_newData$spam==1])+log(1-pred[out_newData$spam==0]))
  dv0=sum(log(ifelse(out_newData$spam==1,mean(out_newData$spam),1-mean(out_newData$spam))))
  R2_1[i]=1-dv1/dv0
}
sum((R2-mean(R2))^2)
sum((R2_1-mean(R2_1))^2)


#######################################################################
####################Model Selection####################################
#######################################################################


null=glm(spam ~ 1, data=email, family='binomial')
full=glm(spam ~ ., data=email, family='binomial')
model_select=step(null,scope=formula(full),dir="forward")
length(coefficients(model_select))
dim(email)
names(email)

summary(model_select)

library(olsrr)
LM_full=lm(wage~., data=Wage)
model_select_1=ols_step_all_possible(LM_full)
plot(model_select_1)


model_select_2=ols_step_best_subset(LM_full)
plot(model_select_2)

# stepwise forward regression
model_select_3=ols_step_forward_p(LM_full, details = TRUE)
plot(model_select_3)

##Binary Choice data

set.seed(123)
n = 10000

price = rexp(n, 1/3)
y=2-0.3*price*rnorm(n)
decision = ifelse(y>=0,1,0)

table(decision)

Probit = glm(decision-price, family =binomial(link = "probit"))

summary(Probit)

Logit = glm(decision-price, family =binomial(link = "logit"))

summary(Logit)

pred_p = predict(Probit, type="response")
pred_p=c(pred_p)
p_hat = c(pred_p)
p_har = ifelse(pred_p>0.5,1,0)

Tab = table(ifelse(pred_p>0.5,1,0), decision)
Tab
sum((diag(Tab))/n*100)
#demand curve simulation#
seq_p = seq(0.5, 20)
length(seq_p)
coef(Probit)
for(i in 1:length(seq_p)){
  Demand[i] = sum(ifelse(coef(Probit)+coef(Probit)*seq_p[i]+rnorm(n)>0.5, 1, 0))
}
plot(seq_p, demand, "l")
#####Bootstrap variance-covariance

vcov(Probit)
B=1000
B_sample=matrix(sample(c(1:n), n*B, replace=T), nrow = n, ncol = B)
Boot_coef =matrix(0, nrow=2, ncol=B)
for(i in 1:B){
  Boot_coef[,i] = coef(glm(decision[B_sample[,i]-price[B_sample[,i]],family = binormial(link = "probit")]))
}
Vbetastar
plot(Boot_coef[,1], Boot_coef[2,], xlab="betal", ylab="betal", type="p")
cov(t(Boot_coef))
vcov(Probit)


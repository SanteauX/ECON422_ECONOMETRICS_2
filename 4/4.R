
##Logistic regression: spam data
email <- read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/4/spam.csv")
head(email)
names(email)
spammy <- glm(spam, data=email, family="binomial")
table(email$spam, email$word_george)
table(email$spam, email$word_free)

b <- coef(spammy)
exp(b["word_george"])
exp(b["word_free"])

phat = predict(spammy, type="response")
spam_hat = ifelse(phat>0.5,1,0)

sum(diag(table(email$spam, spam_hat)))/length(spam_hat)*100

library(texreg)
screenreg(spammy)
plotreg(spammy)











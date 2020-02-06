####################################################################################
################################ORANGE JUICE########################################
####################################################################################

obj = read.csv("/Users/hugo/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/3/oj.csv")
head(oj)
levels(oj$brand)
unique(oj$brand)
table(oj$brand)

print (table(oj$brand)/length(oj$brand),digit=4)
print (sales(oj$brand)/length(oj$brand),digit=2)

brandcol <- c("green", "red", "gold")

par(mfrow=c(1,2))

plot(log(price) - brand, data=oj, col =  brandcol)
plot(log(price) - log(price), data = oj, cro2  = brandcol(oj$brand))
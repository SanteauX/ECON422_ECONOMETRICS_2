rm(list=ls(all=TRUE))#to remove all the items
library(Hmisc)


####################################################################################
##################################Bootstrap#########################################
####################################################################################
set.seed(123)
number_obs=200
u=rnorm(number_obs)

mean(u)
var(u)/number_obs #estimated variance of x_bar
1/number_obs #theoritical value

B=1000 #number of bootstrapped samples
sample(c(1,2,3),2,replace=T)

bootstrapped_sample=matrix(sample(u,B*length(u),
                                  replace=T),nrow=length(u),ncol=B)
dim(bootstrapped_sample)
bootstrapped_mean=apply(bootstrapped_sample,2,mean)
length(bootstrapped_mean)
plot(density(bootstrapped_mean))

var(bootstrapped_mean)
var(u)/length(u)
1/length(u)
###############################################################################################
###################################Black Jack (21)#############################################
###############################################################################################
licensing=function()
{t=sample(1:13,1)
if(t==1)
  t=11
if(t>=10)
  t=10
t}
licensing()

BJ=function(n,s)
{
  e=0
  for(i in 1:n)
  {
    x=licensing()
    y=licensing()
    x=x+licensing()
    y=licensing()
    y=y+licensing()
    while(x<=s)
    {x=x+licensing()
    }
    if(x>21)
    {e=e-1}
    else
    {
      while(y<=s)
      {y=y+licensing()}
      if(x>=y||y>21)
        e=e+1
      if(y>=x&y<=21)
        e=e-1
    }
  }
  e=e/n
  e}
BJ(10000,16)

strategy=c(12:20)
opt_strategy=rep(0,length(strategy))
for (i in 1:length(strategy))
{
  opt_strategy[i]=BJ(10000,strategy[i])
}
opt_strategy
names(opt_strategy)=strategy
opt_strategy
#######################################################################################
library(quantmod)
##########Load data##############
NASDAQ_AAPL=getSymbols("AAPL",from="2015-07-01",auto.assign=F,OpCl=T) 
##########Let have a nice chart#############
chartSeries(NASDAQ_AAPL,theme=chartTheme("white"),TA="addSMA(4);addSMA(21);
            addCCI(20);addVo();addBBands();addDEMA();addRSI();addMACD();addSMI()")

stock_return=read.csv("~/Dropbox/Concordia University/Econometrics 422/2020_Winter/20200106/stock_return.csv")
head(stock_return)
stock_return=stock_return[,-1]
head(stock_return)
names(stock_return)
stock_return$AAPL
dim(stock_return)

mkt=stock_return$mkt
plot(ts(mkt))
plot(mkt)
exr=as.matrix(stock_return[,1:25])
head(exr)
capm=lm(exr ~ mkt)
cov(stock_return$AAPL,stock_return$mkt)/var(stock_return$mkt)
coef(capm)[,1]
ab=t(coef(capm))[,2:1]

ab=ab[-9,]

par(mai=c(.8,.8,0,0), xpd=FALSE)
plot(ab, type="n", bty="n", xlab="beta", ylab="alpha")
abline(v=1, lty=2, col=8)
abline(h=0, lty=2, col=8)
text(ab, labels=rownames(ab), col="navy") 


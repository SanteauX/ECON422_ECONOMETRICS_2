#NASDAQ_AAPL = getSymbols("AAPL", from "2010-01-01", auto.assign=F, OpCl=T)
#chqrtSeries(NASDAQ_AAPL, theme = chartTheme("white", TQ="aqqSMA(4);addSMA(21);"))
stock_return = read.csv("~/Desktop/Classes/S2/S2 - Econometrics II (ECON 422)/Rcourse/3/stock_return.csv")
stock_return = stock_return[,-1]
head(stock_return)
names(stock_return)
stock_return$AAPL
dim(stock_return)

mkt = stock_return$mkt
plot(ts(mkt))


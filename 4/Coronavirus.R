#Coronavirus


library(remotes)
install_github("Guangchuangyu/nCov2019")
Library(nCov2019)
x = getnCov2019()
x = load_nCov2019()
head(x[])
Levels(x[]$province)
x[levels(as.factor(x[]$province))[1],]

library(ggplot2)
require(ggrepel)

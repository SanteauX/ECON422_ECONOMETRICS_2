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

d=x['湖北',]

ggplot(d,
       aes(time, as.numeric(cum_confirm), group=city, color=city)) +
  geom_point() + geom_line() +
  theme_minimal(base_size = 14) + theme(legend.position='none') +
  xlab(NULL) + ylab(NULL)
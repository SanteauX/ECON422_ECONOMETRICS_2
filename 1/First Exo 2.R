x = 1

x


rm(list=ls(all=TRUE))

### SHORT INTRO

##EXO1

x1 <- .12/.3

x1

x1 <- sqrt(2)
x2

x3 <- log(85)
x3

x4 <- exp(4)
x4

x5 = log(exp(1))

y = c(1,2,3)
y

xx = c(1:100)
xx

y seq(from=1, to=100, by=0.5)
?seq

seq(from=1, to=100, length.out=30)

Length(y)
##How many components in y

print(x1)

c(x1,y)

print(c(x1,x2))

##20th value of y:
y[20]
##summation of two components
y[20]+y[21]



z = c(20, 31, 2, 4, 5)
y[z]


##EXO 2

mu <- c(0,0)
mu

rep(0,2)

rep(c(1:10),5)

w <- rep(NA, 200)
w

sigma <- matrix(c(1,0,0,1), nrow = 2, ncol = 2, byrow = T)
##byrow : by row => 10 first line, 01, second line
sigma

s <- seq(100, 1000, by=100)
s

v[108] <- 6
v

z1 = matrix(c(1:4), 2, 2, byrow=T)
z1

z2 = matrix(c(1:4), 2, 2, byrow=F)
z2

z3=t(z1)
##Transposée de la matrice z1

z3-z2
dim(z1)

z1(1,2)

s<-seq(100, 1000, by=100)
s

z_bar = sum(z)/length(z)
z_bar

sum(z/length(z))

mean(y)


ORANGE = function (z)
	{
		n = length(z)
		S = 0
		for(i in l:n)
		{
			S = S+z[i]
		}
		S/n
		return (list(input=z, output=S/n, num_obs = n, summ = S))
	}
	
ORANGE(z)
ORANGE(z)$summ
mean(z)


##EMPIRICAL VARIANCE OD Z

#
#EMPIRICAL AVERAGE

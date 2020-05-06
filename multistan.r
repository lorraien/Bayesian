
set.seed(5)
n <- 50
a <- 2
A <- matrix(0,nrow=n,ncol=n)
for(i in 1:n){
  A[i,] <- rnorm(n)+a*rnorm(1)
}
A <- A%*%t(A)
D_half <- diag(diag(A)^(-0.5))
C <- D_half%*%A%*%D_half
C.cor <- cov2cor(C)
C.low <- C.cor[lower.tri(C.cor)]
hist(abs(C.low))

####generate data

set.seed(5)
library(mvtnorm)
sim <- rmvnorm(100,mean=rep(0,n),sigma=C)




data <- list(N=100,D=n,x=sim,C=C)
library(rstan)
fit <- stan("multi.stan",data=data,algorithm="NUTS",iter=500,chains = 1,control = list(max_treedepth=20))

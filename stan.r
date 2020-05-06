data <- read.csv("")
data$Gender <- as.numeric(data$Gender)-1
data <- list(
  N=dim(data)[1],
  K1=4,
  K2=10,
  X=cbind(data[,2:15],1),
  y=data[,16]
)



library(rstan)
fit1 <- stan(
  file = "stroke.stan",  # Stan program
  data = data,
  chains = 1,
  warmup = 1000,
  iter = 10000,
  thin=10,
  )




full.result <- extract(fit1)$beta
design <- data$X
design <- as.matrix(data$X)
full.p <-full.result%*%t(design)
outer(1:)
full.p[,data$y==0] <- 1-full.p[,data$y==0]
log(full.p)











```{r}
data <- read.csv("C:/Users/canga/Desktop/Stroke.csv")
data <- list(
  N=dim(data)[1],
  K1=4,
  X=cbind(data[,2:5],1),
  y=data[,16]
)
fit2 <- stan(
  file = "stroke1.stan",  # Stan program
  data = data,
  chains = 1,
  warmup = 1000,
  iter = 10000,
  thin=10,
  )

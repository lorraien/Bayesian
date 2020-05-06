

library(rstan)

data <- list(
  N=80,
  D=14,
  x=Xr,
  y=Yr,
  N_pred=20,
  x_pred=Xt
)


fit1 <- stan(
  file = "GPC2.stan",  # Stan program
  data = data,
  chains = 1,
  warmup = 1000,
  iter = 10000,
  thin=10,
)

install.packages("rstan")
library("rstan")

stanmodelcode <- "
data {
  int<lower=0> N;
  real y[N];
} 

parameters {
  real mu;
} 

model {
  target += normal_lpdf(mu | 0, 10);
  target += normal_lpdf(y  | mu, 1);
} 
"
y <- rnorm(20) 
dat <- list(N = 20, y = y); 
fit <- stan(model_code = stanmodelcode, model_name = "example", 
            data = dat, iter = 2012, chains = 3, verbose = TRUE,
            sample_file = file.path(tempdir(), 'norm.csv')) 
print(fit)

# extract samples 
e <- extract(fit, permuted = FALSE) # return a list of arrays 
str(e)

arr <- as.array(fit) # return an array 
str(arr)

mat <- as.matrix(fit) # return a matrix
str(mat)

## End(Not run)

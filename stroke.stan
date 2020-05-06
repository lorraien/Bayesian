//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//
// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;      //number of subjects
  int<lower=0> K1;     //number of ind predictors for clinical variables
  int<lower=0> K2;     //number of ind predictors for EEG
  matrix[N, K1+K2+1] X;    //predictor matrix for clinical variables and EGG
  int<lower=0,upper=1> y[N];         //outcome vector
}
parameters {
  vector[K1+K2+1] beta;    //coefficients for predictors
  real<lower=0> sigma1;//error scale for clinical variables
  real<lower=0> sigma2;//error scale for EEG
}
model {
  //hyper priors
  sigma1 ~ gamma(1,1);
  sigma2 ~ gamma(1,1);
  //priors
  beta[K1+K2+1] ~ normal(0,100);
  for (i in 1:K1){
    beta[i] ~ normal(0,sigma1);
  }
  for (j in (K1+1):(K1+K2)){
    beta[j] ~ normal(0,sigma2);
  }
  //likelihood
  
  for (n in 1:N){
    y[n] ~ bernoulli_logit(dot_product(X[n],beta));
  }
}

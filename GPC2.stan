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
functions {
  vector gp_pred_rng(vector[] x_pred,
                     vector y1, vector[] x,
                     real magnitude, real length_scale) {
    int N = rows(y1);
    int N_pred = size(x_pred);
    vector[N_pred] f2;
    {
      matrix[N, N] K = cov_exp_quad(x, magnitude, length_scale);
      matrix[N, N] L_K = cholesky_decompose(K);
      vector[N] L_K_div_y1 = mdivide_left_tri_low(L_K, y1);
      vector[N] K_div_y1 = mdivide_right_tri_low(L_K_div_y1', L_K)';
      matrix[N, N_pred] k_x_x_pred = cov_exp_quad(x, x_pred, magnitude, length_scale);
      f2 = (k_x_x_pred' * K_div_y1);
    }
    return f2;
  }
}
data {
  int<lower=1> N;
  int<lower=1> D;
  vector[D] x[N];
  int<lower=0,upper=1> y[N];

  int<lower=1> N_pred;
  vector[D] x_pred[N_pred];
}
transformed data{
  real delta = 1e-9;
}
parameters {
  real<lower=0> magnitude;
  real<lower=0> length_scale; // Next release, we can use length_scale[D] for multiple length scales, (ARD)
  vector[N] eta;
}
transformed parameters {
  vector[N] f;
  {
    matrix[N, N] K;
    matrix[N, N] L_K;   
    K = cov_exp_quad(x, magnitude, length_scale);
    for (n in 1:N)
      K[n, n]=K[n, n] + delta;
      
    L_K = cholesky_decompose(K);
    f = L_K * eta;
  }
}
model {
  magnitude ~ std_normal();
  length_scale ~ inv_gamma(5, 5);
  
  eta ~ normal(0, 1);

  y ~ bernoulli_logit(f);
}
generated quantities {
  vector[N_pred] f_pred = gp_pred_rng(x_pred, f, x, magnitude, length_scale);
  real y_pred[N_pred];
  real y_pred_in[N];
  
  for (n in 1:N) y_pred_in[n] = inv_logit(f[n]); // in sample prediction
  for (n in 1:N_pred) y_pred[n] = inv_logit(f_pred[n]); // out of sample predictions
}



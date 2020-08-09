functions {
  // prior^{1/k}; k = #subsets
  real approx_normal_prior_log (real beta1, real mn1, real sigma1, real nsub) {
    return (normal_log(beta1, mn1, sigma1) / nsub);
  }  
}

data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0, upper=100> sigma;
  real<lower=0> nsub;
}

parameters {
  real mu;
}

model {
  mu ~ approx_normal_prior(0.0, 10, nsub);  
  y ~ normal(mu, sigma);
}

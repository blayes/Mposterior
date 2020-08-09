functions {
  // takes care of the modified likelihood
  real stoc_approx_log (real y, real mu, real sigma, real nrep) {
    return (nrep * normal_log(y, mu, sigma));
  }  
}

data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0, upper=100> sigma;
  real<lower=0> nrep;
}

parameters {
  real mu;
}

model {
  mu ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ stoc_approx(mu, sigma, nrep);
  }
}

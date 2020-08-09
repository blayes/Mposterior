functions {
  // takes care of the modified likelihood
  real stoc_approx_log (real y, real df, real mu, real sigma, real nrep) {
    return (nrep * student_t_log(y, df, mu, sigma));
  }  
}

data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0, upper=10> df;
  real<lower=0, upper=100> sigma;
  real<lower=0> nrep;
}

parameters {
  real mu;
}

model {
  mu ~ normal(0, 5);

  for (n in 1:N) {
    y[n] ~ stoc_approx(df, mu, sigma, nrep);
  }
}

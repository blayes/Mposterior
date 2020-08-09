data {
  int<lower=0> N;
  vector[N] y;
  real<lower=0, upper=100> sigma;
}

parameters {
  real mu;
}

model {
  mu ~ normal(0, 5);
  y ~ normal(mu, sigma);
}

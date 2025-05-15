data{
  int N;
  // predictors
  array[N] int sex; // 1: fem, 2: male
  array[N] int spec; // 1: Adel, 2: Chin, 3: Gent
  vector[N] l; // bill length (cm)
  // outcome
  vector[N] m; // log-body mass (kg)
}
parameters{
  vector[2] B;
  real a;
  real<lower = 0> sigma;
}
model{
  B ~ normal(0, 1);
  a ~ normal(0, 1);
  sigma ~ exponential(1);
  for(i in 1:N) m[i] ~ normal( B[ sex[i]] + a * l[i], sigma  );
}


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
  matrix[ 3, 2 ] B;
  real a;
  real<lower = 0> sigma;
}
model{
  for(k in 1:3) B[k,] ~ normal(0, 1);
  a ~ normal(0, 1);
  sigma ~ exponential(1);
  for(i in 1:N) m[i] ~ normal( B[ spec[i], sex[i]] + a * l[i], sigma  );
}


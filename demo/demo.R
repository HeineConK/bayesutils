library(palmerpenguins)
library(dplyr)
d <- penguins %>% na.omit %>% mutate(
  log.bm.kg = log( body_mass_g / 1000 ),
  bl.cm = bill_length_mm / 10
)

library("bayesutils")



cdat <- "
  int N;
  // predictors
  array[N] int sex; // 1: fem, 2: male
  array[N] int spec; // 1: Adel, 2: Chin, 3: Gent
  vector[N] l; // bill length (cm)

  // outcome
  vector[N] m; // log-body mass (kg)
"


cmdl <- "
  for(k in 1:3) B[k,] ~ normal(0, 1);
  a ~ normal(0, 1);
  sigma ~ exponential(1);
  for(i in 1:N) m[i] ~ normal( B[ spec[i], sex[i]] + a * l[i], sigma  );
"



cpar <- "
  matrix[ 3, 2 ] B;
  real a;
  real<lower = 0> sigma;
"

code <- stancode( cdat, cpar, cmdl )
sc.to.stan.file("peng.stan", code = code)

library("cmdstanr")
m <- cmdstan_model("peng.stan")

dat <- list(
  N = nrow(d),
  sex = as.numeric(d$sex),
  spec = as.numeric(d$species),
  l = d$bl.cm,
  m = d$log.bm.kg
)

fit <- m$sample(data = dat, parallel_chains = 2)
fit <- sample.with.monitor( m, dat, parallel_chains = 2, chains = 2, iter_sampling = 1e4 )

fit <- sample.with.monitor( m, dat,
                            parallel_chains = 2,
                            chains = 2,
                            iter_sampling = 5000,
                            iter_warmup = 5000,
                            save_warmup = T,
                            adapt_delta = 0.999)

fit$diagnostic_summary()
fit$summary()
fit
bayesplot::mcmc_trace(fit$draws())

mcmc.summary( fit )
mcmc.forestplot(fit, vlines.at = 0)
library(ggplot2)

draws <- extract_samples( fit )

svglite::svglite(filename = "forestplot.svg", width=5, height=5)
mcmc.forestplot(draws, vlines.at = 0)
dev.off()

altnames <- expand.grid( levels(d$sex), levels(d$species)) %>%
  apply(1, paste,simplify = F, collapse = "-") %>%
  unlist() %>%
  c("alpha", "sigma")

svglite::svglite(filename = "forestplot.alt.svg", width=5, height=5)
mcmc.forestplot(draws, vlines.at = 0,labels = altnames)
dev.off()



d$log.bm.kg.hat <- sapply(1:nrow(d), \(i){
  is <- sample(1:numdraws(draws),1)
  with(draws, rnorm(1,
                    B[is, d$species[i], d$sex[i]] + a[is] * d$bl.cm[ i],
                    draws$sigma[is]
                    )
       )
})





x11()
ggplot(d, aes(x = log.bm.kg, y = log.bm.kg.hat)) + geom_point() + facet_wrap(~sex + ~species)


# second model

cmdl2 <- "
  B ~ normal(0, 1);
  a ~ normal(0, 1);
  sigma ~ exponential(1);
  for(i in 1:N) m[i] ~ normal( B[ sex[i]] + a * l[i], sigma  );
"



cpar2 <- "
  vector[2] B;
  real a;
  real<lower = 0> sigma;
"



code2 <- stancode( cdat, cpar2, cmdl2 )

sc.to.stan.file("peng2.stan", code = code2)
m2 <- cmdstan_model("peng2.stan")


fit2 <- m2$sample(data = dat, parallel_chains = 2)

svglite::svglite(filename = "forestplot.compare.svg", width=6, height=12)
mcmc.forestplot( list(fit, fit2), xlim = c(-0.4, 1.5) )
dev.off()

svglite::svglite(filename = "forestplot.compare.lbl.svg", width=6, height=12)
mcmc.forestplot( list("Sex+Spec" = fit, "Sex" = fit2), xlim = c(-0.4, 1.5) )
dev.off()

svglite::svglite(filename = "forestplot.compare.sel.svg", width=5, height=5)
mcmc.forestplot( list(fit, fit2), vars = c("a", "sigma") )
dev.off()

svglite::svglite(filename = "forestplot.compare.sel.col.svg", width=5, height=5)
piplotter <- PI.boxed.plotter( p = c(0.5, 0.8), col = c(bu.color(2, alpha = 1), bu.color(2, alpha = 0.5)))
mcmc.forestplot( list(fit, fit2), vars = c("a", "sigma"), pi.plotter = piplotter, xlim = c(0.05, 0.2))
dev.off()

draws2 <- extract_samples( fit2 )

svglite::svglite(filename = "dens.plot.svg", width=5, height=5)
plot.dens( draws2$a, col = bu.color(1), xlim = c(0, 0.25))
lines.dens(draws$a, col = bu.color(2))
polyg.intv(x = draws2$a, prob = 0.90, col = acol( bu.color(1) ))
polyg.intv(x = draws$a, prob = 0.90, col = acol( bu.color(2) ))
dev.off()

pdens <- ggplot.densities(
  xs = list( "alpha (model 1)" = draws$a,
             "alpha (model 2)" = draws2$a
  ),
  col_densline = c(bu.color(2), bu.color(1)),
  pi_lvls = c(0.5, 0.90)
)

print( pdens )

svglite::svglite(filename = "ggdens.plot.svg", width=7, height=5)
print( pdens )
dev.off()


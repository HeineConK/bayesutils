# functions for computing equally-tailed (posterior) intervals a.k.a percentile intervals and highest-(posterior)-density-intervals
# all credit to Prof. R. McElreath
# https://github.com/rmcelreath/rethinking
PI <- function (samples, prob = 0.90){
  x <- sapply(prob, function(p) {
    a <- (1 - p)/2
    quantile(samples, probs = c(a, 1 - a))
  })
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    a <- (1 - prob[i])/2
    names(result)[low_idx] <- paste0(round(a * 100, 0), "%", collapse = "", sep = "")
    names(result)[up_idx] <-  paste0(round((1 - a) * 100,
                                          0), "%", collapse = "", sep = "")
  }
  return(result)
}

# all credit to Prof. R. McElreath
# https://github.com/rmcelreath/rethinking
HDI <- function (samples, prob = 0.90){
  coerce.list <- c("numeric", "matrix", "data.frame", "integer",
                   "array")
  if (inherits(samples, coerce.list)) {
    samples <- coda::as.mcmc(samples)
  }
  x <- sapply(prob, function(p) coda::HPDinterval(samples,
                                                  prob = p))
  n <- length(prob)
  result <- rep(0, n * 2)
  for (i in 1:n) {
    low_idx <- n + 1 - i
    up_idx <- n + i
    result[low_idx] <- x[1, i]
    result[up_idx] <- x[2, i]
    names(result)[low_idx] <- paste0("|", prob[i], collapse = "", sep = "")
    names(result)[up_idx] <- paste0(prob[i], "|", collapse = "", sep = "")
  }
  return(result)
}

inv.logit <- function (x){
  p <- 1/(1 + exp(-x))
  p <- ifelse(x == Inf, 1, p)
  p
}

logit <- function (x){
  log(x) - log(1 - x)
}

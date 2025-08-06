#' Apply Min-Max Normalization to Sample Data
#'
#' @export minmax.norm
#'
minmax.norm <- function(x){

  (x - min(x)) / (max(x) - min(x))

}

#' Compute Standard Error of the Arith. Mean
#'
#' @param x Sample data
#' @param na.rm Ignore missing data
#'
#' @return
#' @export se
#'
se <- function(x, na.rm = FALSE){

  if(na.rm == T){
    x <- x[-which(is.na(x))]
  }

  N <- length(x)

  if(N <= 1){
    return(NA)
  }

  return(sd(x) / sqrt(N))
}

#' Compute Confidence Interval of the Arith. Mean
#'
#' @param x Sample data
#' @param alpha Level for which the (1 - alpha) quantiles are computed.
#' @param na.rm Ignore missing data
#'
#' @return
#' @export ci.mean
#'
ci.mean <- function(x, alpha = .05, na.rm = F){

  if(na.rm == T){
    x <- x[-which(is.na(x))]
  }

  N <- length(x)

  if(N <= 1){
    return(NA)
  }

  x.se <- se(x)
  z <- qnorm(p = alpha/2,lower.tail = F)

  return(c(
    lb = mean(x) - z * x.se,
    ub = mean(x) + z * x.se
  ))
}


#' Compute empirical Variance (without Bessel Adjustement)
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export varN
#'
varN <- function(x, na.rm = FALSE){

  if(na.rm == T){
    x <- x[-which(is.na(x))]
  }

  N <- length(x)

  if(N <= 1){
    return(NA)
  }

  v <- var(x)

  v2 <- (N-1)/N * v

  return(v2)

}

#' Compute empirical Standard Deviation (without Bessel Adjustement)
#'
#' @param x
#' @param na.rm
#'
#' @return
#' @export sdN
#'
sdN <- function(x, na.rm = FALSE){
  return( sqrt( varN(x, na.rm) ))
}


#' Compute empirical Covariances (without Bessel Adjustments)
#'
#'
#' @export covN
#'
covN <- function(X){
  N <- nrow(X)
  C <- cov(X)
  C2 <- (N-1)/N * C
  return(C2)
}

#' Compute centered Data
#'
#'
#'
#' @export center
#'
center <- function(X){
  return( sweep(X, 2, colMeans(X)) )
}

#' Formatter for genrating scientific Numeric Notations for Roundings with Zero-underflow
#'
#'
#' @export round.format
#'
round.format <- function(x,
                         digits = 2,
                         threshold = 0.01) {
  ifelse(
    abs(x) < threshold & x != 0,
    formatC(x, format = "e", digits = digits),
    formatC(x, format = "f", digits = digits)
  )
}


#'
#'
#' @export minmax.norm
#'
minmax.norm <- function(x){

  (x - min(x)) / (max(x) - min(x))

}

#' Title
#'
#' @param x
#' @param na.rm
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

#' Title
#'
#' @param x
#' @param alpha
#' @param na.rm
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


#' Title
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

#' Title
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


#'
#'
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

#'
#'
#'
#'
#' @export center
#'
center <- function(X){
  return( sweep(X, 2, colMeans(X)) )
}

#'
#'
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


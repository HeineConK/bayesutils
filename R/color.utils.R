..ca <- function(x, alpha = 0.2){
  x <- adjustcolor(x, alpha.f = alpha)
  x
}

ccol <- function(x, rot = 1){
  x <- col2rgb(x) / 255
  if(rot == 1) ri <- c(2,3,1)
  else if(rot == 2) ri <- c(3,1,2)
  else stop(paste("Not shure what to do with rotation value", rot))

  x <- rgb(x[ri[1]], x[ri[2]], x[ri[3]])
  x
}

acol <- function (x, alpha = 0.2){
  if (length(x) == 1) {
    ..ca(x, alpha)
  }
  else {
    if(length(alpha) > 1) sapply(1:length(x), \(i) ..ca(x[i], alpha[i]))
    else sapply(x, ..ca, alpha)
  }
}

ablack <- function (alpha = 0.2){
  ..ca("black", alpha)
}

bu.color <- function(i = 1, alpha = 1){

  c1 <- rgb( 86/ 256, 116/ 256, 233/ 256) # light blue
  c2 <- rgb( 241/ 256, 97/ 256, 54/ 256) # terracotta
  c3 <- rgb( 56/ 256, 56/ 256, 56/ 256) # silver grey
  c4 <- rgb( 171/ 256, 190/ 256, 105/ 256) # light green
  c5 <- "#0e2635"
  c6 <- "#ae8b2d"

  cv <- c(
    c1,
    c2,
    c3,
    c4,
    c5,
    c6 # orange
  )

  if(i > 6){
    warning("Max. number of colors is 6. Returning default (1).")
    return(c1)
  }

  return( ..ca(cv[ i ], alpha) )
}

..ca <- function(x, alpha = 0.2){
  x <- adjustcolor(x, alpha.f = alpha)
  x
}


#' Produce a Simple RGB Complement Color given an Input Color
#'
#' Generate an RGB complement by transposing channel values.
#'
#' @param x A color, can be a color name or hex code.
#' @param rot A numeric value. Can be either 1 or 2. Then the channels are rotated by that number.
#' @return A character vector of complement color `x`.
#' @export
ccol <- function(x, rot = 1){
  x <- col2rgb(x) / 255
  if(rot == 1) ri <- c(2,3,1)
  else if(rot == 2) ri <- c(3,1,2)
  else stop(paste("Not shure what to do with rotation value", rot))

  x <- rgb(x[ri[1]], x[ri[2]], x[ri[3]])
  x
}

#' Apply Alpha Channel to Colors
#'
#' Adds an alpha transparency to one or more colors.
#' @param x A color or a vector of colors. Can be color names or hex codes.
#' @param alpha A numeric value (between 0 and 1) or a vector of values representing the
#'   transparency level(s) to apply. Must be the same length as `x` if vectorized.
#'
#' @return A character vector of colors with alpha applied, in the same length as `x`.
#' @export
acol <- function (x, alpha = 0.2){
  if (length(x) == 1) {
    ..ca(x, alpha)
  }
  else {
    if(length(alpha) > 1) sapply(1:length(x), \(i) ..ca(x[i], alpha[i]))
    else sapply(x, ..ca, alpha)
  }
}

#' Quick Helper function to generate an opaque Black Color Value.
#'
#' @param alpha Alpha value (between 0 and 1) applied to black.
#' @export
ablack <- function (alpha = 0.2){
  ..ca("black", alpha)
}


#' Retun bayesutil's Default Colors
#'
#' @param i Color integer index, between 1 - 9.
#' @param alpha Alpha color value to apply if needed. Defaults to 1.
#' @export
bu.color <- function(i = 1, alpha = 1){

  c1 <- rgb( 86/ 256, 116/ 256, 233/ 256) # light blue
  c2 <- rgb( 241/ 256, 97/ 256, 54/ 256) # terracotta
  c3 <- rgb( 56/ 256, 56/ 256, 56/ 256) # silver grey
  c4 <- rgb( 171/ 256, 190/ 256, 105/ 256) # light green
  c5 <- "#0e2635"
  c6 <- "#ae8b2d"
  c7 <- "#406580"
  c8 <- "#931d62"
  c9 <- "#8db259"

  cv <- c(
    c1,
    c2,
    c3,
    c4,
    c5,
    c6,
    c7,
    c8,
    c9
  )

  if(i > 9){
    warning("Max. number of colors is 6. Returning default (1).")
    return(c1)
  }

  return( ..ca(cv[ i ], alpha) )
}

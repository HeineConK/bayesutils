#' Create Posterior Interval Segment Plotter
#'
#' Returns a plotter object that draws horizontal segments representing posterior intervals onto an MCMC forestplot.
#'
#' @param p Numeric vector of posterior interval probabilities (e.g., c(0.9, 0.5)).
#' @param col Color(s) for the segments. Can be a single value or a vector matching `p`.
#' @param lwd Line width(s) for the segments. Can be a single value or a vector matching `p`.
#'
#' @return A list containing:
#' \describe{
#'   \item{p}{The interval probabilities.}
#'   \item{args}{A list with plotting parameters (`col`, `lwd`).}
#'   \item{plot.fun}{A function used internally to draw the segments.}
#' }
#'
#' @export
PI.segment.plotter <- function(p = c(0.9, 0.5), col = 1, lwd = c(2,4)){
  pi.plot <- function(i, args, x0, x1, y0, y1){
    col = args$col
    lwd = args$lwd

    segments(x0 = x0,
             x1 = x1,
             y0 = y0,
             y1 = y1,
             col = ifelse(length(col) > 1, col[i], col),
             lwd = ifelse(length(lwd) > 1, lwd[i], lwd)
    )
  }
  return(
    list(
      p = p,
      args = list(
        col = col,
        lwd = lwd
      ),
      plot.fun = pi.plot
    )
  )
}


#' Create Posterior Interval Horizontal Rectangle Plotter
#'
#' Returns a plotter object that draws horizontal rectangles representing posterior intervals onto an MCMC forestplot.
#'
#' @param p Numeric vector of posterior interval probabilities.
#' @param col Fill color(s) for rectangles.
#' @param heights Numeric vector specifying vertical heights for the rectangles.
#' @param border Border color(s) for the rectangles (default: NA).
#'
#' @return A list with interval settings and a rectangle-based plotting function.
#'
#' @export
PI.hrect.plotter <- function(p = c(0.9, 0.5), col = c(8,1), heights = c(0.06, 0.12), border = NA){
  pi.plot <- function(i, args, x0, x1, y0, y1){

    col = args$col
    border  = args$border
    hheights = args$heights / 2

    rect(xleft = x0,
         xright = x1,
         ybottom = y0 - ifelse(length(heights) > 1, hheights[i], hheights),
         ytop = y1 + ifelse(length(heights) > 1, hheights[i], hheights),
         col = ifelse(length(col) > 1, col[i], col),
         border = ifelse(length(border) > 1, border[i], border)
    )
  }
  return(
    list(
      p = p,
      args = list(
        col = col,
        border = border,
        heights = heights
      ),
      plot.fun = pi.plot
    )
  )
}

#' Create Boxed Posterior Interval Plotter
#'
#' Draws horizontal rectangles (with constant height) to represent posterior intervals in an MCMC forestplot.
#'
#' @param p Numeric vector of posterior interval probabilities.
#' @param col Fill color(s) with optional transparency (e.g., using `acol()`).
#' @param height Numeric scalar or vector for the box height.
#' @param border Border color(s) for the boxes (default: NA).
#'
#' @return A list with interval settings and a boxed rectangle plotting function.
#'
#' @export
PI.boxed.plotter <- function(p = c(0.99, 0.9, 0.5), col = c(acol(4,0.1),
                                                            acol(4,0.6),
                                                            acol(4,1)),
                             height = c(0.08), border = NA){

  pi.plot <- function(i, args, x0, x1, y0, y1){

    col = args$col
    border  = args$border
    hheights = args$heights / 2

    rect(xleft = x0,
         xright = x1,
         ybottom = y0 - height,
         ytop = y1 + height,
         col = ifelse(length(col) > 1, col[i], col),
         border = ifelse(length(border) > 1, border[i], border)
    )
  }

  return(
    list(
      p = p,
      args = list(
        col = col,
        border = border,
        height = height
      ),
      plot.fun = pi.plot
    )
  )


}

#' MCMC Forest Plot for Posterior Intervals
#'
#' Creates a forest plot showing posterior intervals and point estimates for one or more models.
#'
#' @param x A `CmdStanFit` object, `mcmc.draws`, a summary `data.frame`, or a list of such objects.
#' @param vars Optional vector of variable names to include.
#' @param col.means Color(s) for the posterior mean points.
#' @param pch.means Plotting character(s) for means.
#' @param cex.means Point size(s) for means.
#' @param lwd.means Line width(s) for points.
#' @param pi.plotter A plotter object from `PI.segment.plotter()`, `PI.hrect.plotter()`, or `PI.boxed.plotter()`.
#' @param vlines.at Numeric positions for vertical reference lines.
#' @param vlines.col Color(s) for vertical lines.
#' @param vlines.lwd Line width(s) for vertical lines.
#' @param vlines.lty Line type(s) for vertical lines.
#' @param axis Integer vector indicating where to draw axes (1 = bottom, 3 = top).
#' @param ignore.lp Logical; whether to exclude `lp__` from summaries.
#' @param labels Optional custom labels for variables.
#' @param xlim X-axis limits.
#'
#' @return Generates a forest plot of posterior intervals and means.
#'
#' @export
mcmc.forestplot <- function(x,
                            vars = NULL,
                            col.means = 1,
                            pch.means = 1,
                            cex.means = 1,
                            lwd.means = 1,
                            pi.plotter = PI.boxed.plotter(),
                            vlines.at = numeric(0),
                            vlines.col = 8,
                            vlines.lwd = 1,
                            vlines.lty = 2,
                            axis = 1,
                            ignore.lp = TRUE,
                            labels = NULL,
                            xlim = NULL
){

  fit_obj <- NULL
  fit_obj_list <- NULL
  draws <- NULL
  draws_list <- NULL
  stanreg_list <- NULL

  summaries <- NULL

  if(any(class(x) == "CmdStanFit")) fit_obj <- x
  else if("stanreg" %in% class(x)) draws <- extract_samples_from_rstan( x )
  else if(class(x) == "mcmc.draws") draws <- x
  else if(class(x) == "data.frame"){
    if(ignore.lp) summaries <- list( x[rownames(x) != "lp__", ] )
    else summaries <- list( x )
  }
  else{
    are.fitobs <- all( sapply(x, function(xi) any(class(xi) == "CmdStanFit")) )
    are.draws  <- all( sapply(x, function(xi) class(xi) == "mcmc.draws") )
    are.stanregs <- all( sapply(x, function(xi)  "stanreg" %in% class(x) ))
    are.summaries <- all( sapply(x, function(xi) class(xi) == "data.frame") )

    if(are.fitobs) fit_obj_list <- x
    else if(are.draws) draws_list <- x
    else if(are.stanregs) stanreg_list <- x
    else if(are.summaries){
      if(ignore.lp) summaries <- lapply(x, \(xi) xi[rownames(xi) != "lp__",])
      else summaries <- x
    }
  }

  mdl_names <- NULL
  if(!is.null(fit_obj)){
    fit_obj_list <- list(fit_obj)
  }

  if(!is.null(draws)){
    draws_list <- list(draws)
  }

  if(is.null(summaries))
    if(!is.null(fit_obj_list)){
      summaries <- lapply(fit_obj_list, function(fit_obj) mcmc.summary(fit_obj = fit_obj, draws = draws, vars = vars, ignore.lp = ignore.lp, PI.lvls = pi.plotter$p))
      if(length(fit_obj_list) > 1){
        if(is.null(names( fit_obj_list))) mdl_names <- paste0("M", 1:length(fit_obj_list))
        else mdl_names <- names(fit_obj_list)
      }
    }

  if(!is.null(draws_list)){
    summaries <- lapply(draws_list, function(draws) mcmc.summary(fit_obj = fit_obj, draws = draws, vars = vars, ignore.lp = ignore.lp, PI.lvls = pi.plotter$p))
    if(length(draws_list) > 1){
      if(is.null(names( draws_list))) mdl_names <- paste0("M", 1:length(draws_list))
      else mdl_names <- names(draws_list)
    }
  }

  if(!is.null(stanreg_list)){
    summaries <- lapply(stanreg_list, function(stanregobj) mcmc.summary(fit_obj = fit_obj, draws = draws, vars = vars, ignore.lp = ignore.lp, PI.lvls = pi.plotter$p))
    if(length(stanreg_list) > 1){
      if(is.null(names( stanreg_list))) mdl_names <- paste0("M", 1:length(stanreg_list))
      else mdl_names <- names(stanreg_list)
    }
  }


  if(!is.null(summaries) && is.null(mdl_names)){
    if(length(summaries) > 1){
      if(is.null(names( summaries))) mdl_names <- paste0("M", 1:length(summaries))
      else mdl_names <- names(summaries)
    }
  }

  varsp <- character()
  for(s in summaries) varsp <- c(varsp, rownames(s))
  varsp <- unique(varsp)


  xrange <- if(is.null(xlim)){
    xrange <- c(0,0)
    xranges <- matrix(nc = 2, nr = 0)
    # for(s in summaries){
    for(is in 1:length(summaries)){
      s <- summaries[[is]]
      scc <- s[complete.cases(s),]
      if(nrow(scc) > 0){
        rn <- rownames( scc )
        for(ri in 1:nrow(scc)){
          if(! rn[ri] %in% varsp) next
          xranges <- rbind(xranges, range(scc[ri,-2]))
        }
      }
    }
    xrange <- range(xranges)
    extendrange(xrange)
  }else xlim


  # xrange <- if(is.null(xlim)){
  #   xrange <- c(0,0)
  #   for(s in summaries){
  #     scc <- s[complete.cases(s),]
  #     if(nrow(scc) > 0) xrange <- range(c(xrange, range(scc[,-2])))
  #   }
  #   extendrange(xrange)
  # }else xlim


  # stop()

  nlines <- length(varsp)


  dotchart(rep(-Inf, nlines),
           xlim = xrange,
           lcolor = par("bg"),
           labels = if(!is.null(labels)){labels}else{varsp},
           xaxt = "n")
  if(1 %in% axis) axis(1)
  if(3 %in% axis) axis(3)

  if(length(col.means) == 1) col.means <- rep(col.means, nlines)
  if(length(pch.means) == 1) pch.means <- rep(pch.means, nlines)
  if(length(cex.means) == 1) cex.means <- rep(cex.means, nlines)
  if(length(lwd.means) == 1) lwd.means <- rep(lwd.means, nlines)
  # stop()

  vspace <- 0.8 / length(summaries)
  vspace <- ifelse(vspace > 0.2, 0.2, vspace)

  if(length(pi.plotter) == 1) pi.plotter <- rep(pi.plotter, length(summaries))

  for(k in 1:nlines){

    varnk <- varsp[ k ]

    for(is in 1:length(summaries)){
      yk <-  k - (is-1)*vspace
      abline(h = yk, lty = 3, col = 8)

      sm <- summaries[[is]]

      if(!(varnk %in% rownames(sm))) next

      for(s in 1:length(pi.plotter$p)){
        ps <- pi.plotter$p[s]
        pi.plotter$plot.fun(i = s,
                            args = pi.plotter$args,
                            x0 = sm[which(varnk == rownames(sm)), paste0("PI", 100 * ps, ".lwr")],
                            x1 = sm[which(varnk == rownames(sm)), paste0("PI", 100 * ps, ".upr")],
                            y0 = yk,
                            y1 = yk)
      }
      points(sm[which(varnk == rownames(sm)), "mean"], y = yk, col = col.means, cex = cex.means, lwd = lwd.means, pch = pch.means)
      if(!is.null(mdl_names)) text(xrange[1], yk + vspace/2, mdl_names[is], pos = 4)
    }
  }
  if(length(vlines.at) > 0){
    if(length(vlines.col) == 1) vlines.col <- rep(vlines.col, length(vlines.at))
    if(length(vlines.lwd) == 1) vlines.lwd <- rep(vlines.lwd, length(vlines.at))
    if(length(vlines.lty) == 1) vlines.lty <- rep(vlines.lty, length(vlines.at))

    abline(v = vlines.at, col = vlines.col, lwd = vlines.lwd, lty = vlines.lty)
  }
}



#' Add Density Curve or Eye-Shaped Polygon to Plot
#'
#' Adds a density representation to an existing plot, optionally mirrored around the x-axis.
#'
#' @param x Numeric vector; ignored if `d` is provided.
#' @param normalize Logical; whether to normalize the density.
#' @param d Precomputed density object.
#' @param y0 Vertical offset for positioning.
#' @param shape Either `"distrib"` for standard density or `"eye"` for mirrored polygon.
#' @param lwd Line width (ignored for `"eye"`).
#' @param ... Additional graphical parameters passed to `lines()` or `polygon()`.
#'
#' @export

lines.dens <- function(x = NULL, normalize = NULL, d, y0 = 0, shape = "distrib", lwd = 2, ...){
  if(missing(d)) d <- dens(x, normalize = normalize)
  if(shape == "distrib") lines(d$x, d$y + y0, lwd = lwd, ...)
  else if(shape == "eye") polygon( x = c(d$x, rev(d$x)), y = c(d$y, rev(-d$y)), ...)
}

#' Plot Density Curve with Optional Histogram
#'
#' Plots a kernel density estimate with optional histogram overlay and eye-style shapes.
#'
#' @param x Numeric vector of values.
#' @param normalize Logical; whether to normalize the density.
#' @param show.hist Logical; whether to overlay a histogram.
#' @param breaks.hist Number of histogram bins.
#' @param border.hist Border color for histogram.
#' @param col.hist Fill color for histogram.
#' @param col Color for density line.
#' @param lwd Line width for density.
#' @param y0 Vertical offset.
#' @param xlim, ylim Axis limits.
#' @param xlab, ylab Axis labels.
#' @param ... Additional plot arguments.
#'
#' @return A base R plot with density and optional histogram.
#'
#' @export
plot.dens <- function(x, normalize = NULL, show.hist = F, breaks.hist = 10, border.hist = FALSE, col.hist = "grey80", col = 1, lwd = 2, y0 = 0, xlim, ylim, xlab = "x", ylab = "density", ...){
  d <- dens( x, normalize = normalize)

  if(missing(ylim)) ylim <- range(d$y)
  if(missing(xlim)) xlim <- range(d$x)
  plot(1,1, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, ...)

  if(show.hist) hist(x, probability = T, breaks = breaks.hist, col = col.hist, border = border.hist, add = T)

  lines.dens(d = d, y0 = y0, lwd = lwd, col = col,...)
}

#' Draw Polygon for Posterior Interval on Density Plot
#'
#' Adds a shaded polygon showing a posterior interval under a density curve.
#'
#' @param d A density object (optional if `x` is provided).
#' @param x Numeric data vector used if `d` is not supplied.
#' @param y0 Vertical offset for the polygon.
#' @param interval Numeric vector of interval bounds (e.g., `c(2,4)`).
#' @param prob Posterior probability level (e.g., `0.8`). Ignored if `interval` is supplied.
#' @param intv.fun Function used to compute the interval (default: `PI`).
#' @param shape Either `"distrib"` or `"eye"` for polygon style.
#' @param col Fill color.
#' @param normalize Logical; normalize density if computing from `x`.
#' @param border Border color.
#' @param ... Additional graphical parameters passed to `polygon()`.
#'
#' @return Adds a shaded polygon to the current plot.
#'
#' @export
polyg.intv <- function(d, x, y0 = 0, interval = NULL, prob = NULL, intv.fun = PI,
                       shape = "distrib",
                       col = "grey80",
                       normalize = NULL,
                       border = 1,
                       ...){
  if(is.null(interval) && is.null(prob)) stop("No interval bounds or probability level given for plotting")
  if(missing(d) && missing(x)) stop("Please provide density object or data x to infer density")
  if(missing(d)) d <- dens(x, normalize = normalize, ...)


  if(!is.null(interval)){
    x_at <- interval
  }
  if(!is.null(prob)){
    x_at <- intv.fun(x, prob = prob)
  }
  d_at <- dens.at(d = d, x.at = x_at)

  wi <- which(d$x > x_at[1] & d$x < x_at[2])
  wx <- d$x[ wi ]
  wy <- d$y[ wi ]

  x1 <- c(x_at[1], x_at[1], wx, x_at[2], x_at[2])
  y1 <- c(0, d_at[1], wy, d_at[2], 0) + y0

  if(shape == "distrib"){
    polygon(x = x1, y = y1, col = col, border = border)
  }else if(shape == "eye"){
    polygon(x = c(x1, rev(x1)), y = c(y1, rev(-y1)), col = col, border = border)
  }

}


if(FALSE){
  plot(1,1, type = "n", xlim = c(0,10), ylim = c(0,10))
  x <- rnorm(1000, 5, 1)
  #polygon(1:3, c(2,2,4))
  dx <- density(x)
  polygon(x = dx$x, y = dx$y)
  polygon(dens2(x))


  plot(1,1, type = "n", xlim = c(0,10), ylim = c(-1,1))
  plot.dens(x, normalize = T, ylim = c(-1,1))
  lines.dens(x, shape = "eye")

  polyg.intv(x = x, prob = 0.8, shape = "eye", col = "grey80")

  plot.dens(x, normalize = T, ylim = c(-1,1), col = "white")
  lines.dens(x)
  polyg.intv(x = x, prob = 0.8, shape = "distrib", col = "grey80")
}



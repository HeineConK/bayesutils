#'
#' @export ggplot.densities
ggplot.densities <- function(xs = NULL,
                             dens_list = NULL,
                             xnames = NULL,
                             pi_lvls = c(0.5, 0.9, 0.99),
                             pi_fun = PI,
                             pi_lab_digits = 2,
                             ymax = NULL,
                             xlim = NULL,
                             show_densline = T,
                             show_pi_segments = T,
                             show_pi_labels   = T,
                             show_mean = T,
                             show_yaxis = T,
                             col_densline = bu.color(7),
                             col_mean = "white",
                             col_pi_labels = "grey20",
                             bins_hist = 30,
                             fontface_pi_labels = "italic",
                             pi_label_formatter = round.format,
                             ylab = "density",
                             xlab = "x",
                             ggtheme = ggplot2::theme_classic(),
                             ...) {

  if (is.null(xs) && is.null(dens_list)) {
    stop("Provide at least 'xs' (a list of numeric vectors) or 'dens_list'.")
  }

  Nx <- length( xs )
  if(length(col_densline) == 1) col_densline <- rep(col_densline, Nx)

  if(is.null(dens_list)){
    dens_list <- list()
    for(i in 1:Nx) dens_list[[i]] <- density(xs[[i]], ...)
  }

  if(is.null(xnames) ){
    if(is.null(names(xs))){
      xnames <- paste0("x", 1:Nx)
    }else{
      xnames <- names(xs)
    }
  }

  if (is.null(ymax))
    drange <- c(0, max( unlist(lapply(dens_list, \(l) max(l$y))) ))
  else
    drange <- c(0, ymax)





  # grouping histogram data
  # dat <- data.frame(
  #   x = c(unlist(lapply(xs, \(x) x))),
  #   X = as.factor(unlist(sapply(1:Nx, function(i) rep(xnames[i], length(xs[[i]])))))
  # )

  # grouping densitiy data
  dat <- data.frame(
    x = c(unlist(lapply(dens_list, \(dx) dx$x))),
    y = c(unlist(lapply(dens_list, \(dx) dx$y))),
    X = as.factor(unlist(sapply(1:Nx, function(i) rep(xnames[i], length(dens_list[[i]]$x)))))
  )

  plt <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y, color = X, group = X)) + ggplot2::ylab(ylab) + ggplot2::xlab(xlab)
  # if (show_hist){
  #   plt <- plt +  geom_histogram(aes(y = ..density.., fill = X), bins = bins_hist)
  # }

  if (show_densline){
    plt <- plt + ggplot2::geom_line(linewidth = 1.2)
  }


  segment_heights <- rep(1, Nx)
  if (show_pi_segments){

    pi_list <- lapply(xs, function(x){
      sapply(pi_lvls, \(p) pi_fun(x, p))
    })

    # visually deconflicting overlapping intervals
    ip_lmax <- which.max( pi_lvls )
    intervals <-
      t(sapply(pi_list, \(pi) pi[, ip_lmax]))
    assign_interval_heights <- function(intervals) {
      if (!is.matrix(intervals) || ncol(intervals) != 2) {
        stop("Input must be a matrix with two columns (start, end).")
      }

      n <- nrow(intervals)
      heights <- integer(n)

      # sort
      order_idx <- order(intervals[, 1])
      intervals_sorted <- intervals[order_idx, , drop = FALSE]


      level_ends <- numeric(0)

      for (i in seq_len(n)) {
        start <- intervals_sorted[i, 1]
        end <- intervals_sorted[i, 2]

        # Try to assign to the first non-conflicting level
        assigned <- FALSE
        for (lvl in seq_along(level_ends)) {
          if (start > level_ends[lvl]) {  # strict inequality to prevent touching
            level_ends[lvl] <- end
            heights[order_idx[i]] <- lvl
            assigned <- TRUE
            break
          }
        }

        # If none found, create new level
        if (!assigned) {
          level_ends <- c(level_ends, end)
          heights[order_idx[i]] <- length(level_ends)
        }
      }

      return(heights)
    }

    segment_heights <- assign_interval_heights(intervals)
    max_height <- max(segment_heights)
    ymin_extend <- -(max_height+1) * 0.08 * drange[2]  # adjust as needed
    ymax_extend <- drange[2]

    plt <- plt + ggplot2::coord_cartesian(ylim = c(ymin_extend, ymax_extend))


    for(k in 1:Nx){
      px <- pi_list[[k]]

      ybase <- ymin_extend/max_height * (segment_heights[k] - 1)
      pi_seg_ymin <- ybase  -0.05 * drange[2]
      pi_seg_ymax <- ybase  -0.01 * drange[2]

      for (i in 1:length(pi_lvls)) {
        plt <- plt + ggplot2::annotate(
          "rect",
          xmin = px[1, i],
          xmax = px[2, i],
          ymin = pi_seg_ymin,
          ymax = pi_seg_ymax,
          # just below 0
          fill = acol(col_densline[k], alpha = 1 / i)
        )

        if (show_pi_labels) {
          plt  <- plt + ggplot2::annotate(
            "text",
            x = px[1, i],
            y = pi_seg_ymin - 0.025*drange[2],
            label = pi_label_formatter(px[1, i], pi_lab_digits),
            col = col_pi_labels,
            fontface = fontface_pi_labels
          )
          plt  <- plt + ggplot2::annotate(
            "text",
            x = px[2, i],
            y = pi_seg_ymin - 0.025*drange[2],
            label = pi_label_formatter(px[2, i], pi_lab_digits),
            col = col_pi_labels,
            fontface = fontface_pi_labels
          )
        }

      }

    }
  } # end segment plotting

  if(show_mean){
    for(k in 1:Nx){
      ybase <- ymin_extend/max_height * (segment_heights[k] - 1)
      pi_seg_ymin <- ybase  -0.05 * drange[2]
      pi_seg_ymax <- ybase  -0.01 * drange[2]

      d <- data.frame(
        x = mean(xs[[k]]),
        y = mean(c(
          pi_seg_ymin, pi_seg_ymax
        ))
      )

      plt <- plt +
        ggplot2::geom_point(
          data = d,
          ggplot2::aes(x = x, y = y),
          col = "grey10",
          inherit.aes = F,
          size = 2.5
        ) +
        ggplot2::geom_point(
          data = d,
          ggplot2::aes(x = x, y = y),
          col = col_mean,
          inherit.aes = F,
          size = 2.1
        )
    }
  }


  plt <- plt + ggplot2::scale_color_manual(values = col_densline)

  plt <- plt + ggtheme
  if (!show_yaxis)
    plt <- plt + ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  if (!is.null(xlim))
    plt <- plt + ggplot2::xlim(xlim)

  return(plt)
}



#'
#' @export ggplot.dens
ggplot.dens <- function(x = NULL,
                        dens = NULL,
                        pi_lvls = c(0.5, 0.9, 0.99),
                        pi_fun = PI,
                        pi_lab_digits = 2,
                        ymax = NULL,
                        xlim = NULL,
                        show_densline = T,
                        show_hist = T,
                        show_pi_segments = T,
                        show_pi_labels   = T,
                        show_mean = T,
                        show_yaxis = T,
                        col_densline = bu.color(7),
                        col_hist = bu.color(7, 0.4),
                        col_mean = "white",
                        col_pi_segment_base = "grey20",
                        col_pi_labels = "grey20",
                        bins_hist = 30,
                        fontface_pi_labels = "italic",
                        pi_label_formatter = round.format,
                        ylab = "density",
                        xlab = "x",
                        ggtheme = ggplot2::theme_classic(),
                        ...) {
  if (is.null(dens)) {
    if (is.null(x))
      stop("Invalid data. You must at least provide a numeric vector x.")

    dx <- density(x, ...)
  } else{
    dx <- dens
  }


  if (is.null(ymax))
    drange <- c(0, max(dx$y))
  else
    drange <- c(0, ymax)

  px <- sapply(pi_lvls, \(l) pi_fun(x, prob = l))


  ymin_extend <- -0.08 * drange[2]  # adjust as needed
  ymax_extend <- drange[2]


  plt <- ggplot2::ggplot(data = data.frame(x = x), ggplot2::aes(x = x)) + ggplot2::ylab(ylab) + ggplot2::xlab(xlab)

  if (show_hist)
    plt <- plt +  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), fill = col_hist, bins = bins_hist)
  if (show_densline)
    plt <- plt + ggplot2::geom_line(
      data = data.frame(x = dx$x, y = dx$y),
      ggplot2::aes(x = x, y = y),
      col = col_densline,
      linewidth = 1.2
    )

  pi_seg_ymin <- -0.05 * drange[2]
  pi_seg_ymax <- -0.01 * drange[2]

  if (show_pi_segments){
    plt <- plt + ggplot2::coord_cartesian(ylim = c(ymin_extend, ymax_extend))

    for (i in 1:length(pi_lvls)) {
      plt <- plt + ggplot2::annotate(
        "rect",
        xmin = px[1, i],
        xmax = px[2, i],
        ymin = pi_seg_ymin,
        ymax = pi_seg_ymax,
        # just below 0
        fill = acol(col_pi_segment_base, alpha = 1 / i)
      )

      if (show_pi_labels) {
        plt  <- plt + ggplot2::annotate(
          "text",
          x = px[1, i],
          y = -0.072 * drange[2],
          label = pi_label_formatter(px[1, i], pi_lab_digits),
          col = acol(col_pi_labels, 1),
          fontface = fontface_pi_labels
        )
        plt  <- plt + ggplot2::annotate(
          "text",
          x = px[2, i],
          y = -0.072 * drange[2],
          label = pi_label_formatter(px[2, i], pi_lab_digits),
          col = acol(col_pi_labels, 1),
          fontface = fontface_pi_labels
        )
      }

    }
  }

  if (show_mean)
    plt <- plt + ggplot2::geom_point(
      data = data.frame(x = mean(x), y = mean(c(
        pi_seg_ymin, pi_seg_ymax
      ))),
      ggplot2::aes(x = x, y = y),
      col = col_mean,
      size = 2
    )


  plt <- plt + ggtheme
  if (!show_yaxis)
    plt <- plt + ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()
    )

  if (!is.null(xlim))
    plt <- plt + ggplot2::xlim(xlim)

  return(plt)
}

#'
#' @export ggplot.pi
ggplot.pi <- function(x,
                      pi_lvls = c(0.5, 0.9, 0.99),
                      pi_fun = PI,
                      show_pi_labels = T,
                      pi_lab_digits = 2,
                      col_pi_segment_base = "grey20",
                      col_pi_labels = "grey20",
                      fontface_pi_labels = "italic",
                      pi_label_formatter = round.format,
                      xlab = "x",
                      xlim = NULL) {
  ggplot.dens(
    x = x,
    pi_fun = pi_fun,
    pi_lvls = pi_lvls,
    show_densline = F,
    show_hist = F,
    show_yaxis = F,
    ymax = 1,
    col_pi_segment_base = col_pi_segment_base,
    show_pi_labels = show_pi_labels,
    pi_lab_digits = pi_lab_digits,
    fontface_pi_labels = fontface_pi_labels,
    pi_label_formatter = pi_label_formatter,
    xlab = xlab,
    xlim = xlim
  )
}

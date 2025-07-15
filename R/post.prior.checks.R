#' @export post.prior.check
#' @export
post.prior.check <- function(prior_draws, post_draws, qseq = seq(.1, .9, .1)) {
  post_vars <- draws.varnames(post_draws)
  prior_vars <- draws.varnames(prior_draws)

  postd_list <- list()

  k <- 1
  for (vi in 1:length(post_draws)) {
    di <- post_draws[[vi]]
    dv <- dim(di)
    if (is.null(dv)) {
      stop(paste0("Draws missing in ", names(post_draws)[vi]))
    }
    if (length(dv) == 2) {
      if (dv[2] == 1) {
        postd_list[[k]] <- di[, 1]
        k <- k + 1
      }
      if (dv[2] > 1) {
        for (i in 1:dv[2]) {
          postd_list[[k]] <- di[, i]
          k <- k + 1
        }
      }
      if (length(dv) == 3) {
        for (i in 1:dv[2]) {
          for (j in 1:dv[3]) {
            postd_list[[k]] <- di[, i, j]
            k <- k + 1
          }
        }
      }
    }
  }

  names(postd_list) <- post_vars


  # prior draws
  priord_list <- list()
  k <- 1
  for (vi in 1:length(prior_draws)) {
    di <- prior_draws[[vi]]
    dv <- dim(di)
    if (is.null(dv)) {
      stop(paste0("Draws missing in ", names(prior_draws)[vi]))
    }
    if (length(dv) == 2) {
      if (dv[2] == 1) {
        priord_list[[k]] <- di[, 1]
        k <- k + 1
      }
      if (dv[2] > 1) {
        for (i in 1:dv[2]) {
          priord_list[[k]] <- di[, i]
          k <- k + 1
        }
      }
      if (length(dv) == 3) {
        for (i in 1:dv[2]) {
          for (j in 1:dv[3]) {
            priord_list[[k]] <- di[, i, j]
            k <- k + 1
          }
        }
      }
    }
  }

  names(priord_list) <- prior_vars

  if (length(priord_list) != length(postd_list)) {
    warning(paste0(
      "Missmatch in variable numbers. \nPrior: ",
      length(priord_list),
      " Post: ",
      length(postd_list)
    ))
  }

  Nvars_prior <- length(priord_list)

  checks <- lapply(1: Nvars_prior, function(vi) {
    vn <- names(priord_list)[vi]
    d1 <- priord_list[[vn]]
    d2 <- postd_list[[which(names(postd_list) == vn)]]

    pi50_1 <- PI(d1, 0.5)
    pi50_2 <- PI(d2, 0.5)

    d1q <- quantile(d1, qseq)
    d2q <- sapply(d1q, function(quant)
      sum(d2 < quant)) / length(d2)
    qq_sq_residuals <- sum((d2q - qseq)^2)

    qq_sq_residuals_max <- sum(qseq^2)

    list(
      qq_points = cbind(quantile_prior = qseq, quantile_post = d2q),
      qq_sqr_residuals = qq_sq_residuals,
      qq_norm_sqr_residuals = qq_sq_residuals / qq_sq_residuals_max
    )
  })

  names(checks) <- names( priord_list )

  qq_nsq_resids <- sapply(checks, \(ch) ch$qq_norm_sqr_residuals )
  names(qq_nsq_resids) <- names(checks)

  resid_order <- order( qq_nsq_resids )
  imax <- ifelse(Nvars_prior> 10, 10, Nvars_prior)


  checks$summary__ <- list(
    sum_resids = summary(qq_nsq_resids),
    min10_resids = qq_nsq_resids[ resid_order[1:imax] ]
  )

  class(checks) <- "post.prior.check"

  return( checks )
}


#' @export print.post.prior.check
#' @export
print.post.prior.check <- function(check){
  cat("Summary statistics for normalized quantile residuals:", "\n\n")
  print(check$summary__$sum_resids)
  cat("\n\n")
  cat("Variables with smallest normalized quantile residuals:", "\n\n")
  print(check$summary__$min10_resids)
}

#' @export plot.post.prior.check
#' @export
plot.post.prior.check <- function(check){
  plot(1,1, xlim=c(0,1), ylim=c(0,1), type = "n", xlab = "emp. quantiles of prior draws", ylab = "emp. quantiles of posterior draws")
  abline(a = 0, b = 1, col = "grey80")

  vn_min <- names(check$summary__$min10_resids)
  for(vn in vn_min){
    checkv <- check[[vn]]
    qq_coords <- checkv$qq_points

    points(qq_coords, type = "b", col = bu.color(1, alpha = 1 - checkv$qq_norm_sqr_residuals))
  }
}

#' @export post.prior.check
#' @export
post.prior.check <- function(prior_draws, post_draws, qseq = seq(.1, .9, .1), vars = NULL) {

  df_pri <- as.data.frame(..mcmc.gather(draws = prior_draws, vars = vars, gather = "matrix")$draws_mat)
  df_pos <- as.data.frame(..mcmc.gather(draws = post_draws, vars = vars, gather = "matrix")$draws_mat)

  if(ncol(df_pri) != ncol(df_pos)){
    warning(paste0(
      "Missmatch in variable numbers. \nPrior: ",
      length(ncol(df_pri)),
      " Post: ",
      length(ncol(df_pos))
    ))
  }

  Nvars_prior <- ncol( df_pri )
  checks <- lapply(1:Nvars_prior, function(vi) {
    vn <- names(df_pri)[vi]
    d1 <- df_pri[,vn]
    d2 <- df_pos[,vn]


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

  names(checks) <- names( df_pri )

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
plot.post.prior.check <- function(check, annotate = F){
  plot(1,1, xlim=c(0,1), ylim=c(0,1), type = "n", xlab = "emp. quantiles of prior draws", ylab = "emp. quantiles of posterior draws")
  abline(a = 0, b = 1, col = "grey80")

  vn_min <- names(check$summary__$min10_resids)

  if(annotate){
    cols_annot <- c(
      sapply(1:6, bu.color),
      "grey20",
      "grey40",
      "grey60",
      "grey70"
    )
  }

  for(i in 1:length(vn_min)){
    vn <- vn_min[ i ]
    checkv <- check[[vn]]
    qq_coords <- checkv$qq_points

    if(annotate){
      cols_annot[ i ] <- acol(cols_annot[ i ], alpha = 0.8*(1 - checkv$qq_norm_sqr_residuals)+0.2)
      points(qq_coords,
             type = "b",
             col = cols_annot[i],
             pch = i)

    }else{
      points(qq_coords, type = "b", col = bu.color(1, alpha = 0.8*(1 - checkv$qq_norm_sqr_residuals)+0.2))
    }
  }

  if(annotate){
    legend("bottomright",
           legend = vn_min,
           pch = 1:length(vn_min),
           col = cols_annot[1:length(vn_min)])
  }

}

#' Start browser-based monitoring process for ongoing HMC sampling
#'
#' Invokes external R process to read HMC sampler data and provide simple browser-based trace plot visualizations, while the sampler is still doing it's thing. Helpful for
#' checking if divergent transitions or any other pathological chain behaviour are present without to wait for the sampler to finish. Requires the draws to be written locally to CSV. This functions effectively
#' wraps [m$sample], where [m] is a compiled CmdStan model, whereas an outside process is started to read generated CSV draws data, open your default webbrowser and
#' generate simple traceplot visualizations of obtained draws. The data and visualizations are refreshed continously. \bold{Warning: If the sampler is stopped, the monitor process keeps going and needs to be shut down manually as well.}
#'
#' @param m A CmdStanR model object.
#' @param data A list structure one would provide to the model and sample call through [m$sample(data = data)].
#' @param pallalel_chains See CmdStanR doc. Mind that the monitor process takes away one effective CPU core.
#' @param save_warmup See CmdStanR doc. If TRUE, the monitor will start plotting warumup draws as well (highlighted though to distringuish them from prod. draws). The monitor starts drawing after at least 100 production (or warumup) draws were generated.
#' @param refresh See CmdStanR doc.
#' @param init See CmdStanR doc.
#' @param iter_warmup See CmdStanR doc.
#' @param iter_sampling See CmdStanR doc.
#' @param sampling.monitor.dir The directory where all monitoring data is saved. Defaults to \emph{sampling.monitor.dir}
#' @param sampling.monitor.csv The subdirectory name below [sampling.monitor.dir] where draws CSV files are written to. Defaults to \emph{/sampling.csv/}.
#' @param basename.csv The draws CSV files prefix.
#' @param sampling.plots.dir The subdirectory name below [sampling.monitor.dir] where draws plot PNG files are written to. Defaults to \emph{/sampling.plots/}.
#' @param ... Other CmdStanR-related options passed to [m$sample(...)].
#' @return A CmdStanR fit object.
#' @export
sample.with.monitor <- function(m, data, chains = 4, parallel_chains = 1, save_warmup = FALSE,
                                refresh = NULL, init = NULL, iter_warmup = NULL, iter_sampling = NULL,
                                sampling.monitor.dir = "sampling.monitor.dir",
                                sampling.monitor.csv = paste0(sampling.monitor.dir, "/sampling.csv/"),
                                basename.csv = "sampling",
                                sampling.plots.dir = paste0(sampling.monitor.dir, "/sampling.plots"),
                                ...){

  if(!dir.exists(sampling.monitor.dir)){
    dir.create(sampling.monitor.dir)
  }else{
    warning("Directory ", sampling.monitor.dir, " already exists. Overwriting.")
  }

  if(!dir.exists(sampling.plots.dir)){
    dir.create(sampling.plots.dir)
  }


  if(!dir.exists(sampling.monitor.csv)){
    dir.create(sampling.monitor.csv)
  }

  mf <- system.file("cmdstan.monitor/monitor.R", package = "bayesutils")
  mft <- paste0("monitor-", as.numeric(Sys.time()), ".R")
  file.copy(mf, mft)

  mon_cmd <- sprintf("Rscript %s %s %s %s %s %s", mft, sampling.monitor.dir,
                     sampling.monitor.csv,
                     basename.csv,
                     sampling.plots.dir,
                     chains)

  system(mon_cmd, wait = F)

  fitobj <- m$sample(
    data = data,
    chains = chains,
    parallel_chains = parallel_chains,
    output_dir = sampling.monitor.csv,
    output_basename = basename.csv,
    save_warmup = save_warmup,
    refresh = refresh,
    init = init,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    ...
  )

  unlink(mft)
  return( fitobj )
}

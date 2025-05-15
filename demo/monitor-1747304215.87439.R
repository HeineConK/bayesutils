# library("stringr")
# library("Cairo")

args <- commandArgs(trailingOnly = TRUE)

sampling.monitor.dir  <-  args[1]
sampling.monitor.csv  <-  args[2]
basename.csv          <-  args[3]
sampling.plots.dir    <-  args[4]
nchains    <-  as.numeric(args[5])


html_monitor <- paste0(sampling.monitor.dir, "/monitor.html")
if(file.exists(html_monitor)) unlink(html_monitor)

# empty dir check
while(length( list.files(sampling.monitor.csv)) == 0){
  cat("No csv sampling log written to monitor yet. Waiting ...", "\n")
  Sys.sleep(3)
}

tmpdir <- sampling.monitor.csv
bn <- basename.csv

fnames <- list.files(path = tmpdir)
fnames_sel <- fnames[ stringr::str_detect(fnames, bn) ]

read_params <- function(fn){
  lns <- readLines(fn)
  parlines <- lns[ stringr::str_detect(lns, "^#") ]


  warmup_included <- any(stringr::str_detect( parlines, "save_warmup = true"))
  ndraws_wu <- stringr::str_match(parlines[ stringr::str_detect(parlines, "num_warmup = ")], "num_warmup = ([0-9]+)")[,2] |> as.numeric()
  ndraws_sa <- stringr::str_match(parlines[ stringr::str_detect(parlines, "num_samples = ")], "num_samples = ([0-9]+)")[,2] |> as.numeric()

  list(save_warmup = warmup_included,
       ndraws_warmup = ndraws_wu,
       ndraws_sa = ndraws_sa)
}

pars <- lapply(paste0(tmpdir, "/", fnames_sel), read_params)

saved_warmup <- pars[[1]]$save_warmup
nswarmup <- pars[[1]]$ndraws_warmup

if( saved_warmup ){
  nstotal <- max( sapply(pars, \(pr) pr$ndraws_warmup + pr$ndraws_sa) )
}else{
  nstotal <- max( sapply(pars, \(pr) pr$ndraws_sa) )
}

read_data <- function(fn){

  csvlines <- readLines(fn)
  is_comment_lines = stringr::str_starts(csvlines, "#")
  no_tbl_avail <- all( is_comment_lines )
  num_rows_avail <- sum( !is_comment_lines ) - 1

  if(no_tbl_avail || num_rows_avail <= 100){
    list(
      has_data = F,
      dd = NA,
      is_dt = NA
    )
  }else{
    drawdat <- read.csv(fn, header = T, comment.char = "#")
    dd = drawdat

    list(
      has_data = T,
      dd = dd,
      is_dt = drawdat$divergent__,
      nc = ncol(drawdat),
      par.name = names(drawdat)
    )
  }
}

k <- 1
sampling_finished <- F
while(!sampling_finished){

  cat("Updating plots", "\n")

  draw_data <- list()
  for(ic in 1:nchains){
    ddfile <- paste0(tmpdir, "/", bn, "-", ic, ".csv")
    if(!file.exists(ddfile)){
      draw_data[[ic]] <- list(
        has_data = F,
        dd = NA,
        is_dt = NA
      )
    }else{
      draw_data[[ic]] <- read_data(ddfile)
    }
  }

  #draw_data <- lapply(, read_data)
  which_has_data <- which( sapply(draw_data, \(dd) dd$has_data) )
  any_data <- length(which_has_data) > 0 # any()

  if(!any_data){
    print("no data written yet")
    Sys.sleep(4)
    next
  }

  npars <- draw_data[[ which_has_data[1] ]]$nc
  parnames <- draw_data[[ which_has_data[1] ]]$par.name

  # write images
  for(ip in 1:npars){
    for(ic in 1:nchains){
      png_outfile <- paste0(sampling.plots.dir, "/", ic, "_", parnames[ip], ".png")
      dd <- draw_data[[ic]]
      if( !dd$has_data ){
        Cairo::CairoPNG(png_outfile, width = nstotal / 2, height = 400)
        plot(1,1, type = "n", xlim = c(1, nstotal), ylim = c(0,1), yaxt = "n")
        if(saved_warmup){
          rect(xleft = 1, xright = nswarmup, ybottom = 0, ytop = 1, border = F, col = "grey80")
        }
        dev.off()
      }
      else{
        x <- dd$dd[,ip]
        yr <- range(sapply(draw_data, function(dd){
          # range(dd$lp)
          if(dd$has_data){
            x <- dd$dd[,ip]
            if(length(x) <= 101) return( c(-3,3) * sd(x) + mean(x ))
            else return( c(-3,3) * sd(x[-(1:100)]) + mean(x[-(1:100)]))
          }else{
            return( c(NA, NA) )
          }
        } ),na.rm = T)

        Cairo::CairoPNG(png_outfile, width = nstotal/2, height = 400)
        plot(1,1, type = "n", xlim = c(1, nstotal), ylim = yr, ylab = paste("chain", ic))
        # TODO: div trans
        # legend(x = "bottomright", legend = paste0(1:nchains, " dt:", diff_trans), col = 1)
        if(saved_warmup){
          rect(xleft = 1, xright = nswarmup, ybottom = yr[1], ytop = yr[2], border = F, col  = "grey80")
        }


        wdiv <- which(dd$is_dt == 1)
        if(length(wdiv > 0)){
          # abline(v = which(ddi$is_dt == 1), col = col.alpha(8))
          abline(v = which(dd$is_dt == 1), col = 8)
          points(wdiv, x[ wdiv ], col = 2, pch = 16)

        }

        lines( 1:length(x), x)
        dev.off()
      }

    }# ic - chain index

  }# ip - params

  # Div trans histogram
  diff_trans <- rep(0, nchains)

  if(saved_warmup){
    diff_trans <- sapply(1:nchains, function(i){
      dd <- draw_data[[i]]
      ifelse(dd$has_data, sum(dd$is_dt[-c(1:nswarmup)] == 1), 0)
    })
  }else{
    diff_trans <- sapply(1:nchains, function(i){
      dd <- draw_data[[i]]

      ifelse(dd$has_data, sum(dd$is_dt == 1), 0)
    })
  }

  Cairo::CairoPNG(paste0(sampling.plots.dir, "/divtrans.hist.png"), width = 120*nchains, height = 400)

  # hist(diff_trans, labels)
  plot(1:nchains, diff_trans, type = "h", ylab = "number of DT", xlab = "", lwd = 4, xaxt = "n", ylim = c(0, ifelse(max(diff_trans) < 10, 10, max(diff_trans) )))
  axis(1, at = 1:nchains, labels = 1:nchains)
  text(1:nchains, diff_trans, labels = diff_trans, pos = 3)
  dev.off()

  # create monitor html, if not yet existent
  if(!file.exists(html_monitor)){
    # aggregate img src lines for html contents
    fl <- character()
    for(ip in 1:npars){
      section_id <- paste0("section", ip)
      fl <- c(fl, paste0('<h2 id="', section_id ,'">', parnames[ip], "</h2>"))

      for(ic in 1:nchains){
        imgsrc_outfile <- paste0("sampling.plots/", ic, "_", parnames[ip], ".png")

        fl <- c(fl, paste0("<img src='", imgsrc_outfile, "' />"))
      }
    }

    section_id <- paste0("section", npars + 1)
    fl <- c(fl, paste0('<h2 id="', section_id ,'">', "Divergent Transition Statistics", "</h2>"))
    fl <- c(fl, paste0("<img src='",  "sampling.plots/divtrans.hist.png", "' >"))
    # write monitor
    writeLines(con = html_monitor,
               text = c(readLines( system.file("cmdstan.monitor/head.html", package = "bayesutils")),
                        fl,
                        readLines( system.file("cmdstan.monitor/foot.html", package = "bayesutils"))
                        ))

    # launch monitor.html
    browseURL(normalizePath(html_monitor))
  }

  # check if sampling is finished
  sampling_finished <- all(sapply(draw_data, \(dd){
    if(!dd$has_data) return(F)
    else{
      return( nrow(dd$dd) == nstotal )
    }
  }))


  Sys.sleep(2)
} # while, !sampling_finished

cat("Sampling done.", Sys.time(), "\n")

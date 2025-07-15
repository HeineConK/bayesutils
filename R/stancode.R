#' @export stancode
#' @export
stancode <- function(cdat = NULL,
                      cpars = NULL,
                      cmodel = NULL,
                      cfuns = NULL,
                      ctransdat = NULL,
                      ctranspars = NULL,
                      cgenquants = NULL,
                      model.name = NULL,
                      model.description = NULL){

  C <- list(
    cdat = cdat,
    cpars = cpars,
    cmodel = cmodel,
    cfuns = cfuns,
    ctransdat = ctransdat,
    ctranspars = ctranspars,
    cgenquants = cgenquants,
    model.name = model.name,
    model.description = model.description
  )

  class(C) <- "stancode"
  return(C)
}

compose.stancode <- function(x, rm.emptylines = T, include.desc = T, include.name = T){
  stopifnot(class(x) == "stancode")

  if(all(sapply(x, \(cc) is.null(cc)))) stop("No code to compose here. All code fields are empty.")

  comp.field <- function(fl, head){
    c( sprintf("%s{", head), fl, "}" )
  }

  cdlines <- character(0)
  if(include.name && !is.null( x$model.name ))   cdlines <- append(cdlines, paste0("// model: ", x$model.name))
  if(include.desc && !is.null( x$model.description )) cdlines <- append(cdlines, paste0("// descr: ", x$model.description))

  if(!is.null(x$cfuns))       cdlines <- append( cdlines, comp.field(x$cfuns, "functions") )
  if(!is.null(x$cdat))        cdlines <- append( cdlines, comp.field(x$cdat, "data") )
  if(!is.null(x$ctransdat))   cdlines <- append( cdlines, comp.field(x$ctransdat, "transformed data") )
  if(!is.null(x$cpars))       cdlines <- append( cdlines, comp.field(x$cpars, "parameters") )
  if(!is.null(x$ctranspars))  cdlines <- append( cdlines, comp.field(x$ctranspars, "transformed parameters") )
  if(!is.null(x$cmodel))      cdlines <- append( cdlines, comp.field(x$cmodel, "model") )
  if(!is.null(x$cgenquants))  cdlines <- append( cdlines, comp.field(x$cgenquants, "generated quantities") )



  if(rm.emptylines){
    cdlines <- unlist( stringr::str_split(cdlines, "\n"))
    cdlines <- cdlines[ -which( cdlines == "" ) ]
  }
  cdlines <- append(cdlines, "")
  return( cdlines )
}

#' @export print.stancode
#' @export
print.stancode <- function(x, print.desc = F, print.name = T){
  sc <- x
  if( !print.desc ) sc$model.description <- NULL
  if( !print.name ) sc$model.name <- NULL
  cdl <- compose.stancode( sc )
  for(l in cdl) cat(l, "\n")

}

sc.to.stan.file <- function(filename, code, check.syntax = TRUE){
  if(class(code) == "stancode") code <- compose.stancode(code, include.desc = T, include.name = T)
  writeLines(text = code, con = filename)

  if( check.syntax ) rstan:::rstudio_stanc( normalizePath(filename) )

}

sc.from.stan.file <- function(stan.filename){
  if(missing(stan.filename)) stop("Error reading stan code. No stan file provided.")
  if(!file.exists(stan.filename)) stop("Error reading stan code. Stan file does not exist.")

  C <- stancode()

  lns <- readLines( stan.filename )

  k <- NULL
  for(ln in lns){
    if(startsWith(ln, "functions{")){
      C$cfuns <- character(0)
      k <- 4
    }
    else if(startsWith(ln, "data{")){
      C$cdat <- character(0)
      k <- 1
    }else if(startsWith(ln, "transformed data{")){
      C$ctransdat <- character(0)
      k <- 5
    }
    else if(startsWith(ln, "parameters{")){
      C$cpars <- character(0)
      k <- 2
    }
    else if(startsWith(ln, "transformed parameters{")){
      C$ctranspars <- character(0)
      k <- 6
    }
    else if(startsWith(ln, "model{")){
      C$cmodel <- character(0)
      k <- 3
    }
    else if(startsWith(ln, "generated quantities{")){
      C$cgenquants <- character(0)
      k <- 7
    }
    else if(startsWith(ln, "// model: ")){
      C$model.name <- stringr::str_split(ln, "// model: ")[[1]][2]
    }
    else if(startsWith(ln, "// descr: ")){
      C$model.description <- stringr::str_split(ln, "// descr: ")[[1]][2]
    }
    else if(!startsWith(ln, "}") && !startsWith(ln, "//")){

      C[[k]] <- append(C[[k]], ln)
    }
  }

  return( C )
}

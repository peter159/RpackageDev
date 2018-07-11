#' Remove non informative variables
#'
#' This is wrap of `nearZeroVar()`,`findCorrelation()` from caret
#'
#' The output of all these steps will be a dataframe as original class
#'
#' @param data dataframe
#' @param keep vector of variables to keep
#' @param freqCut first/second, default 95/5
#' @param uniqueCut nfac/nrow, default 10(\%)
#' @param cutoff default 0.9
#' @param verbose default FALSE
#'
## TODO feature selection use sig test
#'
#' @export
#' @rdname feature_select
rm_near_zero_vars <- function(data,keep = NULL,
                              freqCut = 95 / 5,uniqueCut = 10,
                              verbose = FALSE){
  install_n_require('caret')

  if (!is.null(keep)){
    data.to.rm <- select(data, -one_of(keep))
  }else{
    data.to.rm <- data
  }

  nzv.var <- nearZeroVar(data.to.rm,
                         freqCut = freqCut,
                         uniqueCut = uniqueCut,
                         names = TRUE)

  if (length(nzv.var) > 0){
    if (verbose){
      cat("variables removed: ",paste0(nzv.var,collapse = ','),'\n')
    }
    return(select(data, -one_of(nzv.var)))
  }else{
    return(data)
  }
}

#' @export
#' @rdname feature_select
rm_high_correlated <- function(data,keep = NULL,
                               cutoff = 0.9,verbose = FALSE){
  install_n_require('caret')

  if (!is.null(keep)){
    data.to.rm <- select(data, -one_of(keep))
  }else{
    data.to.rm <- data
  }

  rm.vars <- findCorrelation(data.to.rm,cutoff = cutoff,names = TRUE)

  if (length(rm.vars) > 0){
    if (verbose){
      cat("variables removed: ",paste0(rm.vars,collapse = ','),'\n')
    }
    return(select(data, -one_of(rm.vars)))
  }else{
    return(data)
  }
}

#' @export
#' @rdname feature_select
rm_linear_combos <- function(data,keep = NULL,verbose = FALSE){
  install_n_require('caret')

  if (!is.null(keep)){
    data.to.rm <- select(data, -one_of(keep))
  }else{
    data.to.rm <- data
  }

  rm.vars <- findLinearCombos(data.to.rm)$remove
  rm.vars <- colnames(data)[rm.vars]

  if (length(rm.vars) > 0){
    if (verbose){
      cat("variables removed: ",paste0(rm.vars,collapse = ','),'\n')
    }
    return(select(data, -one_of(rm.vars)))
  }else{
    return(data)
  }
}

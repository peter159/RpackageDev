#' Remove non informative variables
#'
#' This is wrap of `nearZeroVar()`,`findCorrelation()` from caret
#'
#' The output of `rm_near_zero_vars()` will be a dataframe obj
#'
#' @param data dataframe
#' @param keep vector of variables to kept
#' @param freqCut first/second, default 95/5
#' @param uniqueCut nfac/nrow, default 10(\%)
#' @param cutoff default 0.9
#' @param verbose default FALSE
#'
#' @export
#' @rdname rm_noninfo_vars
rm_near_zero_vars <- function(data,keep = NULL,
                              freqCut = 95 / 5,uniqueCut = 10,
                              verbose = FALSE){
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
#' @rdname rm_noninfo_vars
rm_high_correlated <- function(data,keep = NULL,
                               cutoff = 0.9,verbose = FALSE){
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
#' @rdname rm_noninfo_vars
rm_linear_combos <- function(data,keep = NULL,verbose = FALSE){
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


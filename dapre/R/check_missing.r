#' Check and summarize missing variables
#'
#' Steps and functions for checking and summarizing missing variables
#'
#' The output will be a summary of missing variables
#'
#' @param data data.frame obj
#'
#' @export
#' @rdname check_missing
check_missing <- function(data){
  if (!inherits(data,'data.frame')) stop('data obj has to be dataframe!')

  missing_indices <- sapply(data.train,function(x) sum(is.na(x)))
  missing_summary <- data.frame(index = names(data.train),
                                missing_values = missing_indices)
  missing_summary[missing_summary$missing_values > 0,]
}

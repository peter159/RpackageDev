#' Install if not and require package
#'
#' This is a basic interface of installing required package automatically when need
#' The output of `install_if_not()` will be an error of corresponding when installation failed, otherwise quiet
#'
#' @param packagenames vector of package names that need to require for installation
#' @return
#' `install_n_require()` returns an error message when error
#' @examples
#' install_n_require(packagenames = c('ggplot2','ggfortify'))
#'
#' @export
#' @rdname install_n_require
install_n_require<- function(packagenames){
    list.of.packages <- packagenames
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
    if (length(new.packages)) install.packages(new.packages)
    sapply(packagenames,function(x) require(x,character.only = TRUE))
}

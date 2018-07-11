#' Install package if not required
#'
#' This is a basic interface of installing required package automatically when need
#' The output of `install_n_require()` will be a error of correponding error when installation failed, otherwise quiet
#'
#' @param pkglist vector of package names in character that need to required for installation
#' @return
#' `install_n_require()` returns an error message when error
#' @examples
#' install_n_require(pkglist = c('ggplot2','ggfortify'))
#'
#' @export
#' @rdname install_n_require
install_n_require <- function(pkglist){
  if (!all(sapply(pkglist,is.character))){
    stop('pkglist has to be a character vector')
  }

  list.of.packages <- pkglist
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
  if (length(new.packages))install.packages(new.packages)
  invisible(sapply(pkglist,function(x) require(x,character.only = TRUE)))
}

#' Reload package
#'
#' Reload R package
#'
#' @param pkgname package name
#'
#' @export
#' @rdname reload
reload_pkg <- function(pkgname){
  pkg <- deparse(substitute(pkgname))
  pkg_full <- paste0("package:",pkg)
  if (pkg_full %in% search()){
    detach(pkg_full,unload = TRUE,character.only = TRUE)
  }
  suppressMessages(
    require(pkg,character.only = TRUE,quietly = TRUE)
  )
  cat("package:",pkg,"reloaded \n")
}

## this is for on attach action
.onAttach <- function(libname,pkgname)
{
  pkglist <- c("jiebaR",
               ## "base64",
               ## "sqldf",
               "dplyr",
               "data.table",
               "RJSONIO")

  install_n_require(pkglist)

  cmVer <- read.dcf(file = system.file("DESCRIPTION",package = pkgname),
                    fields = "Version")

  packageStartupMessage("  Nielsen Store Matching Tool. Version ",cmVer," Beta")
}

install_n_require <- function(pkglist){
  if (!all(sapply(pkglist,is.character))){
    stop('pkglist has to be a character vector')
  }

  list.of.packages <- pkglist
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
  if (length(new.packages))
  {
    suppressWarnings(install.packages(new.packages))
    cat("\f")
  }
  invisible(sapply(pkglist,
                   function(x) suppressMessages(require(x,character.only = TRUE))))
}

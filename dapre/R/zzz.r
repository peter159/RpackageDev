## this is for on attach action
.onAttach <- function(libname,pkgname){
  pkglist <- c("ggplot2")
  install_n_require(pkglist)

  cmVer <- read.dcf(file = system.file("DESCRIPTION",package = pkgname),
                    fields = "Version")
  packageStartupMessage("Data Preprocessing and EDA. Version ",cmVer)
}

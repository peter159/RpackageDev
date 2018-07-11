## this is on attach action
.onAttach <- function(libname,pkgname){
  Sys.setlocale(category="LC_CTYPE",locale="C")
  cmVer <- read.dcf(file = system.file("DESCRIPTION",package = pkgname),
                    fields = "Version")
  packageStartupMessage(paste(pkgname,cmVer))
}

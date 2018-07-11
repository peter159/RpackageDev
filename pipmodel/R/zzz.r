## this is for on attach action
.onAttach <- function(libname,pkgname){
    cmVer <- read.dcf(file = system.file("DESCRIPTION",package = pkgname),
                    fields = "Version")
    packageStartupMessage("Pipeline Modeling. Version ",cmVer)
}

.onAttach <- function(libname, pkgname) {
    this.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname),
                      fields="Version")
    packageStartupMessage(paste(pkgname, this.version))

    require(lolcat, quietly = T)
    require(SuppDists, quietly = T)
    require(fitdistrplus, quietly = T)
}
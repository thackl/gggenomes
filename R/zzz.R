.onAttach <- function(libname, pkg) {
  pkg_ver <- paste0(pkg, " v", utils::packageDescription(pkg, fields="Version"))
  citfile <- file.path(system.file(package = pkg), "CITATION")
  cit <- format(utils::readCitationFile(citfile), style="text")
  packageStartupMessage(paste0(pkg_ver, "\n\nIf you use '", pkg, "' in published research, please cite:\n\n", cit))
}

.onAttach <- function(libname, pkgname){
  lines = c("--",
            sprintf(paste("Remember to cite, run citation(package = '%s')",
                          "for further info."),pkgname),
            "--")
  msg = paste(lines,collapse="\n")
  packageStartupMessage(msg)
}
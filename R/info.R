# Inform users about the breaking changes of v0.5 compared to previous versions
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(
    "Welcome to tidystats 0.5.2.\nThis is a small bug",
    "fix update."
  ))
}

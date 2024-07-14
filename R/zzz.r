.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "This version of tidystats uses a new and improved way of",
    "structuring statistics. This breaks previous (major) versions of",
    "tidystats."
  )
  packageStartupMessage(msg)
}

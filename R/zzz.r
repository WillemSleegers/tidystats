.onAttach <- function(libname, pkgname) {
  msg <- paste(
    "This version of tidystats uses a new and improved way of",
    "structuring statistics. This breaks previous versions of",
    "tidystats. Updates to other tidystats-related software are coming soon."
  )
  packageStartupMessage(msg)
}

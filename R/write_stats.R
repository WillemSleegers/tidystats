#' Write a tidystats list to a file
#'
#' [write_stats()] writes a tidystats list to a .json file.
#'
#' @param x A tidystats list.
#' @param path A string specifying the path or connection to write to.
#' @param digits The number of decimal places to use. The default is 6.
#'
#' @examples
#' # Conduct a statistical test
#' sleep_wide <- reshape(
#'   sleep,
#'   direction = "wide",
#'   idvar = "ID",
#'   timevar = "group",
#'   sep = "_"
#' )
#' sleep_test <- t.test(sleep_wide$extra_1, sleep_wide$extra_2, paired = TRUE)
#'
#' # Create an empty list
#' statistics <- list()
#'
#' # Add statistics to the list
#' statistics <- add_stats(statistics, sleep_test)
#'
#' # Save the statistics to a file
#' dir <- tempdir()
#' write_stats(statistics, file.path(dir, "statistics.json"))
#'
#' @export
write_stats <- function(x, path, digits = 6) {
  jsonlite::write_json(
    x,
    path = path,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = digits,
    na = "string"
  )
}

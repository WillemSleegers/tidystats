#' Create a tidy stats json object from a statistical output object
#' @export

tidy_stats <- function(model, args = NULL) UseMethod("tidy_stats")

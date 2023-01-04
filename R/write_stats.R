#' Write a tidystats list to a file
#'
#' \code{write_stats} writes a tidystats list to a .json file.
#'
#' @param x A tidystats list.
#' @param path Path or connection to write to.
#' @param digits The number of decimal places to use. The default is 6.
#' 
#' @examples 
#' # Load dplyr for access to the piping operator
#' library(dplyr)
#' 
#' # Conduct statistical tests
#' # t-test:
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#' 
#' # lm:
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_D9 <- lm(weight ~ group)
#' 
#' # ANOVA:
#' npk_aov <- aov(yield ~ block + N*P*K, npk)
#' 
#' #' # Create an empty list
#' results <- list()
#' 
#' # Add output to the results list
#' results <- results %>%
#'   add_stats(sleep_test) %>%
#'   add_stats(lm_D9, type = "primary", preregistered = TRUE) %>%
#'   add_stats(npk_aov, notes = "An ANOVA example")
#'
#' # Save the results
#' dir <- tempdir()
#' write_stats(results, file.path(dir, "results.json"))
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

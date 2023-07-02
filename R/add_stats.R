#' Add statistical output to a tidystats list
#'
#' [add_stats()] is used to add the output of a statistical test to a
#' tidystats list.
#'
#' @param list A tidystats list.
#' @param output Output of a statistical test.
#' @param identifier A string identifying the model. Automatically created if
#' not provided.
#' @param type A string specifying the type of analysis: primary,
#' secondary, or exploratory.
#' @param preregistered A boolean specifying whether the analysis was
#' preregistered or not.
#' @param notes A string specifying additional information.
#' @param class A string to manually specify the class of the object so that
#' tidystats knows how to extract the statistics. See 'Details' for a list of
#' classes that are supported.
#' @param args A list of additional arguments to customize which statistics
#' should be extracted. See 'Details' for a list of supported functions and
#' their arguments.
#'
#' @details
#' Many functions to perform statistical tests (e.g., [t.test()], [lm()]) return
#' an object containing the statistics. These objects can be stored in variables
#' and used with [add_stats()] to extract the statistics and add them to a
#' list.
#'
#' The list can be saved to a file using the [write_stats()] function.
#'
#' The followings functions are supported so that the statistics can be
#' automatically extracted using [add_stats()].
#'
#' | **Package**       | **Functions**                                         |
#' |-------------------|-------------------------------------------------------|
#' | afex              | [aov_ez()], [aov_car()], [aov_4()], [mixed()]         |
#' | BayesFactor       | [generalTestBF()], [lmBF()], [regressionBF()],        |
#' |                   | [ttestBF()], [anovaBF()], [correlationBF()],          |
#' |                   | [contingencyTableBF()], [proportionBF()],             |
#' |                   | [meta.ttestBF()]                                      |
#' | effectsize        | [cohens_d()], [hedges_g()], [glass_delta()]           |
#' | effsize           | [cohen.d()], [VD.A()], [cliff.delta()]                |
#' | emmeans           | [emmeans()], [contrast()], [test()], [mvcontrast()],  |
#' |                   | [eff_size()], [emtrends()], [joint_tests()],          |
#' |                   | [ref_grid()]                                          |
#' | Hmisc             | [rcorr()]                                             |
#' | irr               | [icc()]                                               |
#' | lme4/lmerTest     | [lmer()], [anova()]                                   |
#' | tidystats         | [describe_data()], [count_data()]                     |
#' | stats             | [anova()], [ansari.test()], [aov()],                  |
#' |                   | [bartlett.test()], [binom.test()], [Box.test()],      |
#' |                   | [chisq.test()], [confint()], [cor.test()],            |
#' |                   | [fisher.test()], [fligner.test()], [friedman.test()], |
#' |                   | [glm()], [kruskal.test()], [ks.test()], [lm()],       |
#' |                   | [mantelhaen.test()], [mauchly.test()],                |
#' |                   | [mcnemar.test()], [mood.test()], [oneway.test()],     |
#' |                   | [pairwise.t.test()], [pairwise.prop.test()],          |
#' |                   | [pairwise.wilcox.test()], [poisson.test()],           |
#' |                   | [PP.test()], [prop.test()], [prop.trend.test()],      |
#' |                   | [quade.test()], [shapiro.test()], [t.test()],         |
#' |                   | [var.test()], [wilcox.test()]                         |
#'
#' The following functions are supported when their class is explicitly
#' specified using the `class` argument.
#'
#' | **Package** | **Function**   | **Class name** | **Notes**               |
#' | ------------| ---------------| ---------------|-------------------------|
#' | stats       | [confint()]    | confint        |                         |
#' | emmeans     | [mvcontrast()] | emm_list       | If `show.ests` = `TRUE` |
#'
#' The following functions support customizations regarding which statistics
#' are extracted via the `args` argument.
#'
#' | **Package** | **Function**        | **Arguments**                         |
#' | ------------| --------------------| --------------------------------------|
#' | brms        | [brm()]             | `prob`, `robust`, `mc_se`             |
#' | lavaan      | [lavaan()], [sem()],| `fit.measures`, `standardized`        |
#' |             | [cfa()]             |                                       |
#'
#' See the function's documentation for more information about each argument.
#'
#' @examples
#' # Conduct analyses
#' sleep_test <- t.test(extra ~ group, data = sleep, paired = TRUE)
#'
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_D9 <- lm(weight ~ group)
#' lm_D9_confint <- confint(lm_D9)
#'
#' npk_aov <- aov(yield ~ block + N * P * K, npk)
#'
#' # Create an empty list to store the statistics in
#' statistics <- list()
#'
#' # Add statistics to the list
#' statistics <- statistics |>
#'   add_stats(sleep_test, type = "primary", preregistered = TRUE) |>
#'   add_stats(lm_D9) |>
#'   add_stats(lm_D9_confint, class = "confint") |>
#'   add_stats(npk_aov, notes = "An ANOVA example")
#'
#' @export
add_stats <- function(list, output, identifier = NULL, type = NULL,
                      preregistered = NULL, notes = NULL, args = NULL,
                      class = NULL) {
  if (is.null(identifier)) {
    if (deparse(substitute(output)) == ".") {
      identifier <- paste0(
        "M", formatC(length(list) + 1,
          width = "1", format = "d"
        )
      )
    } else {
      identifier <- deparse(substitute(output))
    }
  } else {
    if (!is.null(names(list))) {
      if (identifier %in% names(list)) {
        stop("Identifier already exists.")
      }
    }
  }

  if (!is.null(class)) {
    class(output) <- append(class(output), class, after = 0)
  }

  analysis <- tidy_stats(output, args = args)

  if (!is.null(type)) {
    if (type == "primary") {
      analysis$type <- "primary"
    } else if (type == "secondary") {
      analysis$type <- "secondary"
    } else if (type == "exploratory") {
      analysis$type <- "exploratory"
    } else {
      warning(paste(
        "Unknown type; type should be either 'primary',",
        "'secondary', or 'exploratory'."
      ))
    }
  }

  if (!is.null(preregistered)) {
    if (preregistered) {
      analysis$preregistered <- "yes"
    } else {
      analysis$preregistered <- "no"
    }
  }

  if (!is.null(notes)) {
    analysis$notes <- notes
  }

  list[[identifier]] <- analysis

  return(list)
}

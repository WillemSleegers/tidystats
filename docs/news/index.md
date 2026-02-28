# Changelog

## tidystats 0.7

### Changes

- Removed the `rlang`, `checkmate`, `stringr`, `readr`, `purrr`,
  `tibble`, `dplyr`, and `tidyr` dependencies.

### Internal changes

- Added internal string helper functions (`remove_string`,
  `detect_string`, `extract_string`) to replace `stringr` functionality.
- Added internal data frame helper functions (`dots_to_names`,
  `group_names`, `stack_rows`) to replace `dplyr`, `tidyr`, and `purrr`
  functionality.

## tidystats 0.6.3

CRAN release: 2025-04-06

### Changes

- Updated to maintain support with the lme4 package, which renamed the
  deviance statistic to -2\*log(L).
- Updated the README.

## tidystats 0.6.2

CRAN release: 2024-07-14

### Changes

- Updated the package to maintain support for all currently supported
  statistical functions.

## tidystats 0.6.1

CRAN release: 2024-04-09

### New

- Added CITATION file. Use `citation("tidystats")` to see how to cite
  tidystats.

### Changes

- Updated the package to maintain support for all currently supported
  statistical functions.

## tidystats 0.6

### Breaking changes

- Redesigned the tidystats structure to be more expandable. This breaks
  compatibility with the Microsoft Word add-in. An update for the
  Microsoft Word add-in is in the works and will be released soon.

### Changes

- Renamed the `results` argument in
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  to `list`
- Removed the multiplication by 100 in
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)’s
  `pct` column to make it easier to present the numbers as percentages
  (e.g., using
  [`scales::label_percent()`](https://scales.r-lib.org/reference/label_percent.html)).
  You can still obtain percentages using the new `pct` argument.
- Removed the `pct` column from the output of
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md).
- Improved names in [`anova()`](https://rdrr.io/r/stats/anova.html)
  tests.

### New

- Added two new arguments to
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md):
  `args` and `class`. The `args` argument can be used to supply
  additional arguments to customize which statistics are extracted from
  a particular analysis. For a list of supported functions, see the
  “Details” section in the help document of
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md).
  The `class` argument can be used to explicitly indicate the class of
  the analysis you want to add. Sometimes the output of a particular
  analysis returns an object with insufficient information for tidystats
  to know how to extract the statistics. By using the `class` argument,
  you can tell tidystats what kind of object it is so that it can
  nevertheless extract the statistics. For a list of supported classes,
  see the Details section of the help document of
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md).
- Added support for the
  [`confint()`](https://rdrr.io/r/stats/confint.html) function from the
  stats package using the new `class` argument in \`add_stats().
- Added support for the `alpha()`, `corr.test()`, `mardia()`, and
  `ICC()` functions from the psych package.
- Added support for the `icc()` function from the irr package.
- Added support for the `cohen.d()` function from the effsize package.
- Added support for the `emmeans()` function from the emmeans package.
- Added support for the `test()` function on the result of `contrast()`
  on the result of `emmeans()` from the emmeans package.
- Added support for `lme()`, `nlme()`, `gls()`, and
  [`anova()`](https://rdrr.io/r/stats/anova.html) from the nlme package.
- Added support for many more functions from the stats package.
- Added support for the effectsize package.
- Added support for the effsize package.
- Added support for the afex package.
- Added a `pct` argument to
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  to return proportions instead of percentages.

## tidystats 0.5.2

CRAN release: 2023-04-14

- A minor bug fix update to keep the package on CRAN.

### Miscellaneous

- Removed info message that is displayed when the package is loaded.

## tidystats 0.5.1

CRAN release: 2022-01-04

### Changes

- Renamed the `variable` column in the output of
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  to `var`.
- Improved ordering of the columns in the output of
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md).
- Removed automatically setting the method to ‘Generic test’ when adding
  a custom test. This means you can set the method yourself now.
- Renames the `variable` column to `var` in
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md).
- [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  now (again) has support for multiple variables. You can provide more
  than 1 column name (separated by commas) to calculate descriptives for
  each variable.

### Bug fixes

- Using
  [`tidy_stats()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats.md)
  on ungrouped count data produced with
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  is now properly tidied.
- Fixed order of columns in
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  when the data is grouped.
- Fixed a bug when trying to add the results of a multilevel model
  (“Error in summary$`ngrps[[names(summary`$ngrps) == group\$name\]\] :
  attempt to select more than one element in vectorIndex”)
- Fixed GitHub issue
  [\#8](https://github.com/WillemSleegers/tidystats/issues/8).

## tidystats 0.5

CRAN release: 2020-09-21

### Breaking changes

- Changed the way certain model results are parsed. The estimate is now
  parsed as a list containing the name of the estimate and the value of
  the estimate. Models are now parsed to extract the following types of
  lists: statistics, terms, pairs, groups, and effects. This new parsing
  unites t-tests, ANOVA, and regression, including multilevel
  regression.

### New

- Added support for generic tests. If tidystats does not support a
  particular analysis, you can create your own generic test by providing
  a list of statistics.
- Improved support for [`anova()`](https://rdrr.io/r/stats/anova.html).
- Added support for more BayesFactor functions.
- Added a pkgdown website for the package.
- Added several vignettes, including an introduction to tidystats, how
  to use the
  [`tidy_stats_to_data_frame()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats_to_data_frame.md)
  function, and a description of the tidystats taxonomy.

### Miscellaneous

- Removed the cox dataset.

## tidystats 0.4.1

CRAN release: 2020-06-15

### Changes

- [`read_stats()`](https://willemsleegers.github.io/tidystats/reference/read_stats.md)
  now converts Inf character strings to numeric.
- [`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)
  now has a digits argument that determines the number of decimals for
  saved numbers (default: 6).

### New

- Added support for [`anova()`](https://rdrr.io/r/stats/anova.html).
- Added
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  again.

### Bug fixes

- Fixed a bug in
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  caused by the dplyr 1.0.0 update.

### Miscellaneous

- Added tests to minimize bugs
- Added two vignettes

## tidystats 0.4

CRAN release: 2019-09-12

### Breaking changes

- tidystats has been completely redesigned in terms of how statistics
  are combined together. While previously the output of statistical
  models was converted to a tidy data frame, the output is now converted
  to a list, with an entirely different structure. The reason for this
  change is that lists are more machine-readable, enabling more
  interesting features down the line. It is still possible to convert
  the list of statistics to a single data frame with a new function
  called
  [`tidy_stats_to_data_frame()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats_to_data_frame.md).
- The significant changes made to tidystats has resulted in the loss of
  some previously supported statistical functions. For a list of
  currently supported statistical functions, see the help document of
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  or the README.
- All `report()` functions have been removed for now. These may return
  (if I get the impression these are liked) but for now I am focusing my
  development time on creating a Word add-in that will enable
  researchers to use a tidystats-produced file for reporting statistics
  in Microsoft Word.
- [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  no longer accepts multiple column names. The goal of the function is
  now to calculate the descriptives of a single column (which can still
  be grouped to calculate the descriptives for each group level).
- [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  has been removed.

### Changes

- [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  now has a `type` argument to specify whether an analysis was a primary
  analysis, secondary analysis, or exploratory analysis.
- [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  now has a `preregistered` argument to specify whether an analysis was
  preregistered or not.

### New

- Added an example dataset called ‘quote_source’ containing data of a
  replication of Lorge & Curtiss (1936) that was part of the Many Labs
  project (Klein et al., 2014)

## tidystats 0.3

CRAN release: 2019-01-03

### Changes

- Changed the argument order in the family of
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  functions. Previously, the model output or tidy data frame was the
  first argument. This allowed you to directly pipe the model output
  into
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  (using **magrittr**’s %\>%). However, an alternative approach is to
  have the tidystats list to be the first argument. This allows you
  create a long sequence of pipes. You start with the results list, add
  a model via
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md),
  pipe the result into the next
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md),
  and so on. Since you often store your model output in variable names
  anyway, this is probably more convenient. Additionally, this probably
  also keeps your script more tidy (you can do this at the end of your
  data analysis script).
- Certain statistical models are now tidied differently due to the
  addition of a ‘group’ column. Several models like multilevel models,
  meta-analytic models, and arguably also regression models have more
  than just terms (e.g., model fit), so to distinguish between
  coefficients and other parts of the output, a ‘group’ column has been
  added. This also means usage of the `report()` is affected, as now the
  group should be specified when necessary. Affected models are
  regression, within-subjects ANOVA, multilevel models, and
  meta-analysis models.
- Removed the `identifier` column from each list element when using
  [`read_stats()`](https://willemsleegers.github.io/tidystats/reference/read_stats.md).
- Reordered the columns of
  [`tidy_stats.lm()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats.md)
  and
  [`tidy_stats.glm()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats.md)
  to be consistent with the other
  [`tidy_stats()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats.md)
  functions.
- Added check for an existing identifier in `add_stats_to_model()`.
- Added warnings in case unsupported output is added (e.g., a pre-tided
  data frame).
- [`read_stats()`](https://willemsleegers.github.io/tidystats/reference/read_stats.md)
  now removes empty columns from each list element.

### New

- Added a new function called `inspect()`. This function accepts a
  tidystats results list or the output of a statistical model and will
  display all results in RStudio’s Viewer pane. This allows the user to
  visually inspect the results and, importantly, copy results in APA
  style to their clipboard. This function is aimed at users who prefer
  not to use R Markdown or when you want to quickly run a model and get
  the results in APA-style. This new function works well with Microsoft
  Word, but does not work with Apple Pages (some of the styling is lost
  when copying the results).
- Added a `class` argument to
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  and `add_stats_to_model()`. Some statistical tests return a normal
  data.frame or matrix, which does not specify which test produced the
  results. This makes it difficult for tidystats to figure out how to
  tidy the result. Previously, we solved this by
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  accepting pre-tidied data frames. Now we added a the `class` argument
  to specify the name of the function that produced the results, so that
  we can then tidy it for you. Run
  [`?add_stats`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  to see a list of supported classes and see the help document of
  [`tidy_stats.confint()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats.md)
  for an example.
- Under the hood: Added a generic report function for single values
  called `report_statistic()`. Consequently, all report functions have
  been updated to use this new generic function.
- Added support for [`glm()`](https://rdrr.io/r/stats/glm.html).
- Added support for lme4’s `lmer()` and lmerTest’s `lmer()`.
- Added support for psych’s `alpha()`.
- Added support for psych’s `ICC()`.
- Added support for stats’
  [`confint()`](https://rdrr.io/r/stats/confint.html) via the new
  `class` argument in
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  and `add_stats_to_model()`.

### Bug fixes

- Fixed a bug that would incorrectly classify ANOVAs as One-way ANOVAs
  when character variables were used rather than factors.
- Prepared for dplyr 0.8.

### Miscellaneous

- Added tests to the R package to minimize bugs.
- Made the code and documentation more consistent.
- Added an under-the-hood helper function to rename statistics columns.

## tidystats 0.2

CRAN release: 2018-05-06

### Changes

- Renamed `describe()` to
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  so that it no longer conflicts with **psych**’s `describe()`.
- Changed
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
  to no longer accept non-numeric variables, but added the feature that
  descriptives can be calculated for more than 1 variable at a time. It
  is recommended to use the
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  function for non-numeric variables.
- Renamed `tidy_descriptives()` to `tidy_describe_data()` and improved
  the function. A notable change is that var information is now returned
  to identify which descriptives belong to which variable. Also changed
  the group delimiter to ’ - ’.
- [`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)
  now prettifies the numbers using
  [`prettyNum()`](https://rdrr.io/r/base/formatc.html) when saving them
  to disk.

### New

- Improved `report()` function. The method now supports the option to
  retrieve a single statistic from any tidy stats data frame. This will
  allow you to report all statistics, even when reporting functions for
  a specific method are not yet supported.
- Added quick report functions for means and standard deviations.
  Instead of using `report()` you can use `M()` and `SD()` to quickly
  report the mean or standard deviation, without having to specify that
  particular statistic. Less typing!
- Added an option called ‘tidystats_list’ in
  [`options()`](https://rdrr.io/r/base/options.html) to set a default
  list. By setting the tidystats list in
  [`options()`](https://rdrr.io/r/base/options.html), you do not need to
  specify the list in the **results** argument of `report()`. Less
  typing!
- Reporting regression results will now include a check for whether
  confidence intervals are included, and report them.
- Added skewness and kurtosis to
  [`describe_data()`](https://willemsleegers.github.io/tidystats/reference/describe_data.md)
- Added new
  [`count_data()`](https://willemsleegers.github.io/tidystats/reference/count_data.md)
  function to calculate count descriptives of categorical data. Also
  added a `tidy_count_data()` function to tidy the output of this new
  function.
- Added support for
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) and
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html).
- Added a better default `identifier` to
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md).
  If you supply a variable to be added to the tidystats list, and no
  identifier is provided, it will take the variable name as the
  identifier. If you pipe the results into
  [`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
  then the old default identifier will be used (e.g., “M1”).
- Added identifier check to `report()`. The function will now throw an
  error when the identifier does not exist.
- Added statistic check to all report functions. The function will now
  throw an error when the statistic does not exist.
- Improved `report_p_value()` to support multiple p-values.
- Updated documentation to be more consistent and to take into account
  the changes made in the current update.

### Bug fixes

- Fixed bug that it was assumed that confidence intervals in `htest`
  objects were always 95% confidence intervals.
- Fixed bug in report functions that would occur when no statistic
  argument was provided.
- Removed spaces from terms in
  [`aov()`](https://rdrr.io/r/stats/aov.html) output.
- Removed a leading space from the method information of a Two Sample
  t-test.
- Improved `add_stats_to_model()`. The method previously required a term
  and did not automatically complete information (e.g., method
  information).

## tidystats 0.1

CRAN release: 2017-10-29

- Initial release

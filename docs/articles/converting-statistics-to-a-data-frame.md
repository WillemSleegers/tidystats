# Converting statistics to a data frame

Besides using tidystats in combination with a text editor add-in to
report statistics, you can also use tidystats to read and use the
statistics for other purposes. For example, researchers can extract
specific statistics and perform analyses such as meta-analyses or a
p-curve analysis on the extracted statistics.

One particular useful function that was created for this purpose is
[`tidy_stats_to_data_frame()`](https://willemsleegers.github.io/tidystats/reference/tidy_stats_to_data_frame.md).
This function converts a tidystats list of statistics to a standard data
frame. That means you can use standard subsetting functions such as
[`subset()`](https://rdrr.io/r/base/subset.html) on the data to retrieve
the statistics of interest.

## An example

Below is an example of how to convert a list of statistics to a data
frame and perform several simple operations.

In the example below we read the tidystats list and select all the
p-values.

``` r
library(tidystats)

# Read the .json file containing the statistics and immediately convert it to
# a data frame
stats_file <- system.file("extdata", "statistics.json", package = "tidystats")
if (!nzchar(stats_file)) stats_file <- "../inst/extdata/statistics.json"

statistics <- read_stats(stats_file) |>
  tidy_stats_to_data_frame()

# Extract all the p-values
p_values <- subset(statistics, statistic_name == "p")

p_values
```

|  | identifier | analysis_name | group_name_1 | group_name_2 | statistic_name | symbol | subscript | lower | value | upper | interval | level |
|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|:---|---:|
| sleep_test.5 | sleep_test | sleep$`extra[sleep`$group == 1\] and sleep$`extra[sleep`$group == 2\] | \- | \- | p | \- | \- | \- | 0.002833 | \- | \- | \- |
| lm_D9.6 | lm_D9 | weight ~ group | Model | \- | p | \- | \- | \- | 0.249023 | \- | \- | \- |
| lm_D9.12 | lm_D9 | weight ~ group | Coefficients | (Intercept) | p | \- | \- | \- | 0.000000 | \- | \- | \- |
| lm_D9.17 | lm_D9 | weight ~ group | Coefficients | groupTrt | p | \- | \- | \- | 0.249023 | \- | \- | \- |
| npk_aov.6 | npk_aov | yield ~ block + N \* P \* K | Terms | block | p | \- | \- | \- | 0.015939 | \- | \- | \- |
| npk_aov.12 | npk_aov | yield ~ block + N \* P \* K | Terms | N | p | \- | \- | \- | 0.004372 | \- | \- | \- |
| npk_aov.18 | npk_aov | yield ~ block + N \* P \* K | Terms | P | p | \- | \- | \- | 0.474904 | \- | \- | \- |
| npk_aov.24 | npk_aov | yield ~ block + N \* P \* K | Terms | K | p | \- | \- | \- | 0.028795 | \- | \- | \- |
| npk_aov.30 | npk_aov | yield ~ block + N \* P \* K | Terms | N:P | p | \- | \- | \- | 0.263165 | \- | \- | \- |
| npk_aov.36 | npk_aov | yield ~ block + N \* P \* K | Terms | N:K | p | \- | \- | \- | 0.168648 | \- | \- | \- |
| npk_aov.42 | npk_aov | yield ~ block + N \* P \* K | Terms | P:K | p | \- | \- | \- | 0.862752 | \- | \- | \- |

Alternatively, we can can also easily select all significant p-values.

``` r
sig_p_values <- subset(statistics, statistic_name == "p" & value < .05)
```

|  | identifier | analysis_name | group_name_1 | group_name_2 | statistic_name | symbol | subscript | lower | value | upper | interval | level |
|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|:---|---:|
| sleep_test.5 | sleep_test | sleep$`extra[sleep`$group == 1\] and sleep$`extra[sleep`$group == 2\] | \- | \- | p | \- | \- | \- | 0.002833 | \- | \- | \- |
| lm_D9.12 | lm_D9 | weight ~ group | Coefficients | (Intercept) | p | \- | \- | \- | 0.000000 | \- | \- | \- |
| npk_aov.6 | npk_aov | yield ~ block + N \* P \* K | Terms | block | p | \- | \- | \- | 0.015939 | \- | \- | \- |
| npk_aov.12 | npk_aov | yield ~ block + N \* P \* K | Terms | N | p | \- | \- | \- | 0.004372 | \- | \- | \- |
| npk_aov.24 | npk_aov | yield ~ block + N \* P \* K | Terms | K | p | \- | \- | \- | 0.028795 | \- | \- | \- |

This could be useful if you want to conduct a [p-curve
analysis](https://doi.org/10.1177/1745691614553988). Although do note
that you should not blindly select all *p*-values. You should select
only the *p*-values that are relevant to a particular hypothesis. If
researchers provide the correct meta-information for each test (e.g., by
indicating whether it is a primary analysis), this could help
meta-researchers make correct decisions about which statistics to
include in their analyses.

## Summary

By importing a tidystats-produced file of statistics, you can convert
the statistics to a data frame using the `tidy_stats_to_data_frame`
function and apply common data transformation functions to extract
specific statistics. These statistics can then be used in analyses such
as meta-analyses, p-curve analyses, or other analyses.

## References

Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014). p-curve and
effect size: Correcting for publication bias using only significant
results. *Perspectives on Psychological Science*, *9*(6), 666-681.
<https://doi.org/10.1177/1745691614553988>

# Introduction to tidystats

## Why use tidystats?

The tidystats package is designed to address two problems common in
scientific research: incomplete and incorrect statistics reporting. The
problem of incomplete statistics reporting likely stems from a
fundamental trade-off between comprehensively reporting all statistics
on the one hand and providing a clear, easy-to-read, text on the other.
Word limits don’t help either. The problem of incorrect statistics
reporting is likely caused by manually copy-pasting statistical output
from the output window into a text editor. tidystats addresses these two
problems by enabling researchers to combine the statistics from their
statistical analyses into a single file, which can be shared with others
and used to report statistics using a text editor add-in.

## How to use tidystats?

tidystats is designed to easily fit in your data analysis workflow. In
fact, tidystats can simply be tacked on at the end of a data analysis
session, assuming you have stored the results from each statistical
function into a variable. For example, when you create a regression
model using the [`lm()`](https://rdrr.io/r/stats/lm.html) function, you
often store the result of that analysis in a variable:

``` r
model <- lm(extra ~ group, data = sleep)
```

By storing the output of statistical functions in a variable, you can
use the
[`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
function from tidystats to extract the statistics and add them to a
list. Once all the statistics are gathered together, you save them to a
.json file using the
[`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)
function. This .json file can then be read by a text editor add-in to
report the statistics, or shared with others and read into R to extract
statistics (e.g., for meta-analyses).

## An example

Below follows an example of several analyses conducted on the
`quote_source` data contained within the tidystats package. The data is
from a large-scaled replication of Lorge & Curtiss (1936). More details
can be found in the paper of the replication effort (Klein et al.,
2014). In short, participants saw the following quote:

> “I have sworn to only live free, even if I find bitter the taste of
> death.”

The quote was attributed to either George Washington, a liked
individual, or Osama Bin Laden, a disliked individual. Participants were
asked to what extent they agree with the quote, on a 9-point Likert
scale ranging from 1 (disagreement) to 9 (agreement).

We start with a bit of setup.

``` r
library(tidystats)

data <- quote_source
```

The main hypothesis is that people will like the quote more when it is
attributed to George Washington compared to Osama Bin Laden. We test
this hypothesis by conducting a *t*-test.

``` r
t_test <- t.test(response ~ source, data = data)
t_test
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  response by source
    ## t = -12.801, df = 6322.7, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group Bin Laden and group Washington is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.8017441 -0.5888010
    ## sample estimates:
    ##  mean in group Bin Laden mean in group Washington 
    ##                 5.232241                 5.927514

Participants appear to rate the quote a bit more positively when it is
attributed to George Washington.

We can also perform some additional tests. For instance, does it matter
if the participant is from the US? And does age matter? To answer these
questions, we can perform interaction tests using
[`lm()`](https://rdrr.io/r/stats/lm.html).

Let’s start with the interaction between the source and whether the
participant is from the U.S. or not.

``` r
lm_us_or_not <- lm(response ~ source * us_or_international, data = data)
summary(lm_us_or_not)
```

    ## 
    ## Call:
    ## lm(formula = response ~ source * us_or_international, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.0047 -1.2278 -0.2278  1.7722  3.7722 
    ## 
    ## Coefficients:
    ##                                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                             5.24884    0.08489  61.832  < 2e-16 ***
    ## sourceWashington                        0.40522    0.11720   3.457 0.000549 ***
    ## us_or_internationalUS                  -0.02101    0.09550  -0.220 0.825891    
    ## sourceWashington:us_or_internationalUS  0.37169    0.13227   2.810 0.004966 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.159 on 6321 degrees of freedom
    ##   (18 observations deleted due to missingness)
    ## Multiple R-squared:  0.02748,    Adjusted R-squared:  0.02701 
    ## F-statistic: 59.53 on 3 and 6321 DF,  p-value: < 2.2e-16

The interaction is significant, so it appears to matter whether the
participant is from the U.S. or not. In fact, participants from the U.S.
show a stronger effect than those from outside the U.S.

What about the interaction between source and the participant’s age?

``` r
lm_age <- lm(response ~ source * age, data = data)
summary(lm_age)
```

    ## 
    ## Call:
    ## lm(formula = response ~ source * age, data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.7433 -1.2018 -0.1516  1.7928  3.8828 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.933702   0.095891  51.451  < 2e-16 ***
    ## sourceWashington     0.557365   0.135579   4.111 3.99e-05 ***
    ## age                  0.011470   0.003361   3.412 0.000648 ***
    ## sourceWashington:age 0.005453   0.004783   1.140 0.254328    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.156 on 6308 degrees of freedom
    ##   (31 observations deleted due to missingness)
    ## Multiple R-squared:  0.03085,    Adjusted R-squared:  0.03038 
    ## F-statistic: 66.92 on 3 and 6308 DF,  p-value: < 2.2e-16

No significant interaction effect, so we do not have evidence for age
changing the size of the effect.

Let’s say these are the analyses we want to save the output of and
report later. This is where tidystats comes in. The steps to perform are
to first create an empty list and then to use the
[`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
function to add statistics to the list. This is why we stored each
analysis into a variable. The
[`add_stats()`](https://willemsleegers.github.io/tidystats/reference/add_stats.md)
function takes an analysis, extracts the statistics, and adds the result
to the list. Optionally, you can add additional information about each
analysis, such as whether it was preregistered, whether it was a
primary, secondary, or exploratory analysis, or simply add some notes.

``` r
# Create an empty list to store the analyses in
statistics <- list()

# Add the analyses
statistics <- statistics |>
  add_stats(
    t_test,
    preregistered = TRUE, type = "primary",
    notes = "A t-test comparing the effect of source on the quote rating."
  ) |>
  add_stats(
    lm_us_or_not,
    preregistered = FALSE, type = "exploratory",
    notes = "Interaction effect with being from the U.S. or not."
  ) |>
  add_stats(lm_age)
```

You can see that I added quite some information to the first and second
analysis. I did this to add some documentation about why each analysis
was conducted. This might be particularly helpful when others will
import the file and use it for meta-research (e.g., performing a
meta-analysis or p-curve analysis).

To save these analyses to a file, use the
[`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)
function.

``` r
write_stats(statistics, "lorge-curtiss-1936-replication.json")
```

Note the file extension: .json. These types of files are simply text
files, but in a format that is machine-readable (unfortunately, not very
human-readable). This file can be used to share with others so that they
can read it back into R and extract statistics (e.g., for meta-analyses)
or by you to report the statistics in a text editor.

## References

Lorge, I., & Curtiss, C. C. (1936). Prestige, suggestion, and attitudes.
*The Journal of Social Psychology*, *7*, 386-402.
<https://doi.org/10.1080/00224545.1936.9919891>

Klein, R.A. et al. (2014) Investigating Variation in Replicability: A
“Many Labs” Replication Project. *Social Psychology*, *45*(3), 142-152.
<https://dx.doi.org/10.1027/1864-9335/a000178>

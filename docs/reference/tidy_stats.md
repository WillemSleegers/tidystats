# Tidy the output of a statistics object

`tidy_stats` is used to convert the output of a statistical object to a
list of organized statistics. The `tidy_stats` function is automatically
run when `add_stats` is used, so there is generally no need to use this
function explicitly. It can be used, however, to peek at how the output
of a specific analysis will be organized.

## Usage

``` r
# S3 method for class 'BFBayesFactor'
tidy_stats(x, args = NULL)

# S3 method for class 'rcorr'
tidy_stats(x, args = NULL)

tidy_stats(x, args = NULL)

# S3 method for class 'htest'
tidy_stats(x, args = NULL)

# S3 method for class 'pairwise.htest'
tidy_stats(x, args = NULL)

# S3 method for class 'lm'
tidy_stats(x, args = NULL)

# S3 method for class 'glm'
tidy_stats(x, args = NULL)

# S3 method for class 'anova'
tidy_stats(x, args = NULL)

# S3 method for class 'aov'
tidy_stats(x, args = NULL)

# S3 method for class 'aovlist'
tidy_stats(x, args = NULL)

# S3 method for class 'confint'
tidy_stats(x, args = NULL)

# S3 method for class 'afex_aov'
tidy_stats(x, args = NULL)

# S3 method for class 'mixed'
tidy_stats(x, args = NULL)

# S3 method for class 'brmsfit'
tidy_stats(x, args = NULL)

# S3 method for class 'effectsize_difference'
tidy_stats(x, args = NULL)

# S3 method for class 'effsize'
tidy_stats(x, args = NULL)

# S3 method for class 'emmGrid'
tidy_stats(x, args = NULL)

# S3 method for class 'summary_emm'
tidy_stats(x, args = NULL)

# S3 method for class 'emm_list'
tidy_stats(x, args = NULL)

# S3 method for class 'icclist'
tidy_stats(x, args = NULL)

# S3 method for class 'lavaan'
tidy_stats(x, args = NULL)

# S3 method for class 'lmerMod'
tidy_stats(x, args = NULL)

# S3 method for class 'lmerModLmerTest'
tidy_stats(x, args = NULL)

# S3 method for class 'lme'
tidy_stats(x, args = NULL)

# S3 method for class 'nlme'
tidy_stats(x, args = NULL)

# S3 method for class 'anova.lme'
tidy_stats(x, args = NULL)

# S3 method for class 'gls'
tidy_stats(x, args = NULL)

# S3 method for class 'psych'
tidy_stats(x, args = NULL)

# S3 method for class 'tidystats'
tidy_stats(x, args = NULL)

# S3 method for class 'tidystats_descriptives'
tidy_stats(x, args = NULL)

# S3 method for class 'tidystats_counts'
tidy_stats(x, args = NULL)
```

## Arguments

- x:

  The output of a statistical test.

## Methods (by class)

- `tidy_stats(BFBayesFactor)`: tidy_stats method for class 'BayesFactor'

- `tidy_stats(rcorr)`: tidy_stats method for class 'rcorr'

- `tidy_stats(htest)`: tidy_stats method for class 'htest'

- `tidy_stats(pairwise.htest)`: tidy_stats method for class
  'pairwise.htest'

- `tidy_stats(lm)`: tidy_stats method for class 'lm'

- `tidy_stats(glm)`: tidy_stats method for class 'glm'

- `tidy_stats(anova)`: tidy_stats method for class 'anova'

- `tidy_stats(aov)`: tidy_stats method for class 'aov'

- `tidy_stats(aovlist)`: tidy_stats method for class 'aovlist'

- `tidy_stats(confint)`: tidy_stats method for class 'confint'

- `tidy_stats(afex_aov)`: tidy_stats method for class 'afex_aov'

- `tidy_stats(mixed)`: tidy_stats method for class 'afex_aov'

- `tidy_stats(brmsfit)`: tidy_stats method for class 'brmsfit'

- `tidy_stats(effectsize_difference)`: tidy_stats method for class
  'effectsize_difference'

- `tidy_stats(effsize)`: tidy_stats method for class 'effsize'

- `tidy_stats(emmGrid)`: tidy_stats method for class 'emmGrid'

- `tidy_stats(summary_emm)`: tidy_stats method for class 'summary_emm'

- `tidy_stats(emm_list)`: tidy_stats method for class 'emm_list'

- `tidy_stats(icclist)`: tidy_stats method for class 'icclist'

- `tidy_stats(lavaan)`: tidy_stats method for class 'lavaan'

- `tidy_stats(lmerMod)`: tidy_stats method for class 'lmerMod'

- `tidy_stats(lmerModLmerTest)`: tidy_stats method for class
  'lmerModLmerTest'

- `tidy_stats(lme)`: tidy_stats method for class 'lme'

- `tidy_stats(nlme)`: tidy_stats method for class 'nlme'

- `tidy_stats(anova.lme)`: tidy_stats method for class 'anova.lme'

- `tidy_stats(gls)`: tidy_stats method for class 'gls'

- `tidy_stats(psych)`: tidy_stats method for class 'psych'

- `tidy_stats(tidystats)`: tidy_stats method for class 'tidystats'

- `tidy_stats(tidystats_descriptives)`: tidy_stats method for class
  'tidystats_descriptives'

- `tidy_stats(tidystats_counts)`: tidy_stats method for class
  'tidystats_counts'

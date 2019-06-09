<!-- README.md is generated from README.Rmd. Please edit that file -->

<p align="center">
  <img src="https://github.com/WillemSleegers/tidystats/blob/master/docs/img/hex.png" width = 150 align = center alt="tidystats logo"/>
</p>

tidystats
---------------

**Authors:** [Willem Sleegers](http://willemsleegers.com/)
**License:** [MIT](https://opensource.org/licenses/MIT)

`tidystats` is an R package to easily create a text file containing the output of 
statistical models. The goal of this package is to help researchers accompany 
their manuscript with an organized data file of statistical results in order to 
greatly improve the reliability of meta-research and to reduce statistical 
reporting errors.

Please see below for instructions on how to install and use this package. 
**Do note that the package is currently in development. This means the package 
may contain bugs and is subject to significant changes.** If you find any bugs 
or if you have any feedback, please let me know by creating an issue here on 
Github (it's really easy to do!).

### Installation

`tidystats` can be installed from CRAN and the latest version can be installed 
from Github using [devtools](https://github.com/hadley/devtools). 


```r
library(devtools)
install_github("willemsleegers/tidystats")
```

### Setup

Load the package and start by creating an empty list to store the results of 
statistical models in.


```r
library(tidystats)

results <- list()
```

### Usage

The main function is `add_stats()`. The function has 2 necessary arguments:

- `results`: The list you want to add the statistical output to.
- `output`: The output of a statistical test you want to add to the list (e.g., 
the output of `t.test()` or `lm()`)

Optionally you can also add an identifier and additional notes using the `identifier` and `notes` arguments.

The `identifier` is used to identify the model 
(e.g., 'weight_height_correlation'). If you do not provide one, one is 
automatically created for you.

The `notes` argument is used to add additional information which you may find 
fruitful. Some statistical tests have default `notes` output (e.g., t-tests), 
which will be overwritten when a `notes` argument is supplied to the 
`add_stats()` function.

### Supported statistical functions

**Package:** stats

- `t.test()`
- `cor.test()`
- `lm()`
- `glm()`
- `aov()`
- `chisq.test()`
- `wilcox.test()`
- `fisher.test()`

# Helper functions in tidystats

Functions used under the hood in the tidystats package.

## Usage

``` r
tidy_matrix(m, symmetric = TRUE)

add_statistic(
  list,
  name,
  value,
  symbol = NULL,
  subscript = NULL,
  interval = NULL,
  level = NULL,
  lower = NULL,
  upper = NULL
)

symbol(
  x = c("alpha", "chi_squared", "delta", "guttmans_lambda", "K_squared", "lambda",
    "p_hat", "R_squared", "sigma", "t_squared", "tau")
)
```

## Arguments

- m:

  A matrix.

## Functions

- `tidy_matrix()`: Function to convert matrix objects to a tidy data
  frame.

- `add_statistic()`: Function to add a statistic to list. It helps
  create the list and ignores NULL values.

- `symbol()`: Function to return symbols in ASCII.

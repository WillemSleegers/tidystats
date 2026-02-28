# Read a .json file that was produced with [`write_stats()`](https://willemsleegers.github.io/tidystats/reference/write_stats.md)

`read_stats()` can read a .json file containing statistics that was
produced using tidystats. It returns a list containing the statistics,
with the identifier as the name for each list element.

## Usage

``` r
read_stats(file)
```

## Arguments

- file:

  A string specifying the path to the tidystats data file.

## Examples

``` r
# A simple example, assuming there is a file called 'statistics.json'
if (FALSE) { # \dontrun{
statistics <- read_stats("statistics.json")
} # }

# A working example
statistics <- read_stats(
  file = system.file("statistics.json", package = "tidystats")
)
```

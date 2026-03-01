## Resubmission

The package was previously archived due to failing checks. This submission
addresses those issues:

* Removed all tidyverse dependencies from Imports (rlang, checkmate, stringr,
  readr, purrr, tibble, dplyr, tidyr). The package now only imports jsonlite
  and methods.
* Rewrote all tests to use direct value comparisons, removing the dependency
  on stored reference JSON files that caused brittle failures.

## R CMD check results

0 errors | 0 warnings | 1 note

* The note about "New submission / Package was archived" is expected for a
  resubmission after archival.

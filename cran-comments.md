## Submission

This patch release fixes the test failures reported on the CRAN check page.

* The `irr::icc()` tests previously compared against hard-coded numeric
  constants for the denominator degrees of freedom and the p-value. The `irr`
  package computes these values differently across versions, so the tests
  failed on check machines running a different `irr` version. The tests now
  compare `tidy_stats()` output against the fitted `icc()` object's own values,
  making them version-independent.
* As a precaution, the tests for the other suggested statistics packages were
  hardened in the same way, so they compare against each object's own values
  rather than hard-coded constants.

## R CMD check results

0 errors | 0 warnings | 0 notes

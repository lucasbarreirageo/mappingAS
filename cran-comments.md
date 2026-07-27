## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Test environments

* local: Ubuntu 24.04, R 4.3.3 (R CMD check --as-cran)
* win-builder: R-devel and R-release (Status: 1 NOTE — new submission)
* GitHub Actions: macOS (release); Windows (release); Ubuntu (R-devel, release, oldrel-1)

## Internet access

Several functions read public MapBiomas GeoTIFFs and the WDPA FeatureServer over the
network. All access is wrapped in tryCatch() and fails gracefully with an informative
message. No examples, tests or vignettes access the internet when run on CRAN:
network examples use \dontrun, tests use bundled local fixtures, and the vignette is
not evaluated.

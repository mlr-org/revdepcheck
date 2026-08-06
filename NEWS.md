# 1.0.0.9002

* `problems.md` no longer claims "Wow, no problems at all. :)" when revdeps
  failed to check. It now reports how many packages could not be checked, why,
  and points at `failures.md`.

* `cran.md` no longer silently omits packages that failed before `R CMD check`
  could run (e.g. a `PREPERROR` from a dependency that would not install), and
  no longer prints `NA` instead of the reason a package failed to check.

* `failures.md` now includes the error message for packages that failed before
  installation, instead of an empty code block.

* Reports no longer attribute the checked package's own GitHub URL and
  maintainer email to revdeps that failed before they could be checked, which
  happened because `desc::desc(text = NULL)` falls back to reading the
  DESCRIPTION in the working directory.

* Add `cran` parameter to the `get_repos()` internal and propagate it to the
  upstream functions including `revdep_check()`. It allow user to decide
  whether htey want to always append CRAN mirror to repos or not (@maksymis)
  
* `cloud_check(r_version = "4.4.0")` is the updated default.

# revdepcheck (development version)

* `cloud_check(r_version = "4.3.1")` is the updated default (#361).

* `cloud_check()` gains the ability to check Bioconductor packages via a new
  `bioc` argument, with a default of `FALSE` due to a relatively high likelihood
  of failed checks since Bioconductor system dependencies are currently not
  installed in the cloud check service (#362, #369).

* updated pkgdown template and url to https://revdepcheck.r-lib.org.

* `cloud_check()` gains the ability to add additional packages as the source
  of reverse dependencies.

* `cran_revdeps()` now accepts multiple packge names.

* `cloud_results()` gains a progress bar so you can see what's happening
  for large revdep runs (#273)

* `cloud_report()` can opt-out of saving failures, and saves the CRAN report
  the same way as `revdep_report()` (#271).

* `revdep_report()` now saves the results of `revdep_report_cran()` to
  `revdep/cran.md` (#204)

* Exported `revdep_report()` to write the current results to README.md
  and problems.md.


# revdepcheck 1.0.0

First public release.

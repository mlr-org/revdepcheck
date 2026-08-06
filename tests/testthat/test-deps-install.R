context("dependency installs")

test_that("lazy install deps survive the replaced library path", {
  skip_on_cran()
  skip_if(getRversion() < "4.2.0", "needs `.libPaths(include.site =)`")

  ## crancache calls `callr::rcmd()` from `install_packages()`, i.e. while the
  ## library path is replaced by the library we install into. Check that
  ## preloading `lazy_install_deps()` keeps that lookup working in a worker
  ## that only has the install library and the base library on its path.
  install_lib <- file.path(tempfile(), "library")
  dir.create(install_lib, recursive = TRUE)
  on.exit(unlink(install_lib, recursive = TRUE), add = TRUE)

  use_callr <- function(lib, preload) {
    for (pkg in preload) {
      tryCatch(loadNamespace(pkg), error = function(err) NULL)
    }
    .libPaths(lib, include.site = FALSE)
    tryCatch(
      {
        callr::rcmd("config", "CC")
        TRUE
      },
      error = function(err) conditionMessage(err)
    )
  }

  expect_true(callr::r(
    use_callr,
    args = list(lib = install_lib, preload = lazy_install_deps())
  ))

  ## Without the preloading, this is the failure we are guarding against
  expect_match(
    callr::r(use_callr, args = list(lib = install_lib, preload = character())),
    "there is no package called"
  )
})

context("report")

fake_comparison <- function(package, status = "+") {
  structure(
    list(
      package = package,
      versions = c("1.0", "1.0"),
      status = status,
      old = list(structure(list(cran = TRUE), class = "rcmdcheck")),
      new = structure(
        list(cran = TRUE, version = "1.0"),
        class = "rcmdcheck"
      ),
      cmp = data.frame(
        type = "note",
        change = 0,
        output = "a note",
        stringsAsFactors = FALSE
      )
    ),
    class = "rcmdcheck_comparison"
  )
}

fake_preperror <- function(package, errormsg = "dependency not available") {
  result <- list(
    stdout = character(),
    stderr = character(),
    errormsg = errormsg
  )
  rcmdcheck_error(package, old = result, new = result)
}

test_that("no_problems_message() is only cheerful if everything was checked", {
  expect_equal(
    no_problems_message(list(fake_comparison("good"))),
    "*Wow, no problems at all. :)*"
  )

  msg <- no_problems_message(list(
    fake_comparison("good"),
    fake_preperror("bad")
  ))
  expect_false(grepl("Wow", msg))
  expect_match(msg, "1 of 2 package\\(s\\) could not be checked")
  expect_match(msg, "failed to install dependencies")
  expect_match(msg, "failures.md")
})

test_that("failure_reason() covers every failure status", {
  expect_equal(failure_reason(fake_comparison("x", "i-")), "failed to install")
  expect_equal(failure_reason(fake_comparison("x", "i+")), "failed to install")
  expect_equal(failure_reason(fake_comparison("x", "t-")), "check timed out")
  expect_equal(failure_reason(fake_comparison("x", "t+")), "check timed out")
  expect_equal(
    failure_reason(fake_preperror("x")),
    "failed to install dependencies"
  )
  expect_equal(
    failure_reason(rcmdcheck_error("x", old = NULL, new = NULL)),
    "check failed to run"
  )
})

test_that("on_cran() does not drop packages that failed before checking", {
  expect_true(on_cran(fake_comparison("good")))
  # We know nothing about these, and hiding them from cran.md is worse than
  # occasionally mentioning a Bioconductor package
  expect_true(on_cran(fake_preperror("bad")))
  expect_true(on_cran(rcmdcheck_error("bad", old = NULL, new = NULL)))

  not_cran <- fake_comparison("bioc")
  not_cran$new$cran <- FALSE
  expect_false(on_cran(not_cran))
})

test_that("failures report includes the pre-install error message", {
  out <- capture.output(
    revdep_report_failures(results = list(fake_preperror("bad", "boom!")))
  )
  expect_true(any(grepl("boom!", out, fixed = TRUE)))
})

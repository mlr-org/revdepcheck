context("dependencies")

test_that("parse_deps", {
  deps <- c(
    "foobar",
    "foobar (>= 1.0.0)",
    "foo, bar",
    "foo,bar",
    "foo(>= 1.0.0), bar",
    "foo,\n    bar",
    ""
  )
  expect_equal(
    parse_deps(deps),
    list(
      "foobar",
      "foobar",
      c("foo", "bar"),
      c("foo", "bar"),
      c("foo", "bar"),
      c("foo", "bar"),
      character()
    )
  )
})

test_that("deps_match finds the package at both ends of the string", {
  deps <- c(
    "mlr3 (>= 1.0.1), R (>= 3.1.0),paradox,",
    "R (>= 3.1.0),mlr3,paradox",
    ",,,mlr3",
    "mlr3misc,mlr3verse",
    ""
  )
  expect_equal(deps_match(deps, "mlr3"), c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("parse_deps extreme cases", {
  deps <- c(NA, "", "  ")
  expect_equal(
    parse_deps(deps),
    list(character(), character(), character())
  )

  expect_equal(parse_deps(character()), list())
})

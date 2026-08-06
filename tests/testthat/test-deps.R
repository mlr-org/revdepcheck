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

test_that("cran_deps can ignore suggested packages", {
  db <- cbind(
    Package = c("pkg", "hard", "soft", "harddep", "softdep"),
    Depends = c(NA, NA, NA, NA, NA),
    Imports = c("hard", "harddep", "softdep", NA, NA),
    LinkingTo = c(NA, NA, NA, NA, NA),
    Suggests = c("soft", NA, NA, NA, NA)
  )
  local_mocked_bindings(available_packages = function(...) db)

  expect_equal(
    cran_deps("pkg", repos = character()),
    c("hard", "harddep", "soft", "softdep")
  )
  expect_equal(
    cran_deps(
      "pkg",
      repos = character(),
      direct = c("Depends", "Imports", "LinkingTo")
    ),
    c("hard", "harddep")
  )
})

test_that("parse_deps extreme cases", {
  deps <- c(NA, "", "  ")
  expect_equal(
    parse_deps(deps),
    list(character(), character(), character())
  )

  expect_equal(parse_deps(character()), list())
})

revdep_report_checklist <- function(pkg, results, file = "") {
  worse <- map_lgl(results, \(x) any(x$cmp$change == 1))
  problems <- results[worse]

  for (problem in problems) {
    url <- pkg_links(problem)[[1]]
    cat_glue("* [ ] [{problem$package}]({url}) \u2014 ", file = file)
  }
}

# Create a vector of links in order of desirability
pkg_links <- function(result) {
  links <- list()

  ## Packages that failed before they were checked have no description, and
  ## `desc(text = NULL)` would silently fall back to reading the DESCRIPTION of
  ## the package being checked - i.e. attribute our own repo and maintainer
  ## email to somebody else's revdep.
  description <- result$new$description
  desc <- if (!any(nzchar(description))) {
    NULL
  } else {
    tryCatch(desc::desc(text = description), error = function(x) NULL)
  }
  if (!is.null(desc)) {
    links[["GitHub"]] <- pkg_github(desc)

    maintainer <- desc$get_maintainer()
    email <- rematch2::re_match(maintainer, "<(.+)>")[[1]]
    if (!is.na(email)) {
      links[["Email"]] <- paste0("mailto:", email)
    }
  }

  if (isTRUE(result$new$cran)) {
    links[["GitHub mirror"]] <- paste0(
      "https://github.com/cran/",
      result$package
    )
  }

  if (length(links) == 0) {
    # We know nothing beyond the name, which happens when a package fails
    # before it is ever checked
    links[["CRAN"]] <- paste0(
      "https://cran.r-project.org/package=",
      result$package
    )
  }

  unlist(links)
}

#' @importFrom processx process
#' @importFrom callr r r_process r_process_options
#' @importFrom crancache available_packages
#' @importFrom withr with_envvar

deps_install_opts <- function(
  pkgdir,
  pkgname,
  quiet = FALSE,
  env = character(),
  skip_suggests_failures = FALSE
) {
  func <- function(libdir, packages, required, quiet, repos) {
    ip <- crancache::install_packages
    withr::with_libpaths(
      libdir,
      {
        ip(
          packages,
          dependencies = FALSE,
          lib = libdir[1],
          quiet = quiet,
          repos = repos
        )
        installed <- rownames(installed.packages(libdir[1]))

        missing <- setdiff(required, installed)
        if (length(missing)) {
          stop(
            "Failed to install dependencies: ",
            paste(missing, collapse = ", ")
          )
        }

        ## Non-empty only if some packages are optional, i.e. if
        ## `skip_suggests_failures` is TRUE
        skipped <- setdiff(packages, installed)
        if (length(skipped)) {
          message(
            "Ignoring failed installation of optional packages: ",
            paste(skipped, collapse = ", ")
          )
        }
      }
    )
  }

  args <- c(
    ## We don't want to install the revdep checked package again,
    ## that's in a separate library, hence the `exclude` argument
    deps_opts(
      pkgname,
      exclude = pkg_name(pkgdir),
      skip_suggests_failures = skip_suggests_failures
    ),

    list(
      libdir = dir_find(pkgdir, "pkg", pkgname),
      quiet = quiet
    )
  )

  ## CRANCACHE_REPOS makes sure that we only use cached CRAN packages,
  ## but not packages that were installed from elsewhere
  r_process_options(
    func = func,
    args = args,
    system_profile = FALSE,
    user_profile = FALSE,
    env = c(
      CRANCACHE_REPOS = "cran,bioc",
      CRANCACHE_QUIET = if (quiet) "yes" else "no",
      env
    )
  )
}

deps_opts <- function(
  pkgname,
  exclude = character(),
  skip_suggests_failures = FALSE
) {
  ## We set repos, so that dependencies from Bioconductor are installed
  ## automatically
  repos <- get_repos(bioc = TRUE, cran = TRUE)

  ## We have to do this "manually", because some of the dependencies
  ## might be also dependencies of crancache, so they will be already
  ## installed in another library directory, and also loaded.
  ## But we want to install everything into the package's specific library,
  ## because this is the only library used for the check.
  '!DEBUG Querying dependencies of `paste(pkgname, collapse = ", ")`'
  packages <- cran_deps(pkgname, repos)

  packages <- setdiff(packages, exclude)

  ## We do this, because if a package is not available,
  ## utils::install.packages does not install anything, just gives a
  ## warning
  "!DEBUG dropping unavailable dependencies"
  available <- with_envvar(
    c(CRANCACHE_REPOS = "cran,bioc", CRANCACHE_QUIET = "yes"),
    rownames(available_packages(repos = repos))
  )
  packages <- intersect(packages, available)

  ## Packages that must be installed for the check to be meaningful. Failures
  ## for the remaining (suggested) packages can be tolerated, because a
  ## package is supposed to guard their use, e.g. with `requireNamespace()`,
  ## and `R CMD check` is run with `_R_CHECK_FORCE_SUGGESTS_=false`.
  required <- if (skip_suggests_failures) {
    hard <- cran_deps(
      pkgname,
      repos,
      direct = c("Depends", "Imports", "LinkingTo")
    )
    intersect(packages, hard)
  } else {
    packages
  }

  list(
    packages = packages,
    required = required,
    repos = repos
  )
}

deps_install_task <- function(state, task) {
  pkgdir <- state$options$pkgdir
  pkgname <- task$args[[1]]

  dir_setup_package(pkgdir, pkgname)

  "!DEBUG Install dependencies for package `pkgname`"
  px_opts <- deps_install_opts(
    pkgdir,
    pkgname,
    quiet = state$options$quiet,
    env = state$options$env,
    skip_suggests_failures = state$options$skip_suggests_failures
  )
  px <- r_process$new(px_opts)

  ## Update state
  worker <- list(
    process = px,
    package = pkgname,
    stdout = character(),
    stderr = character(),
    task = task
  )
  state$workers <- c(state$workers, list(worker))

  wpkg <- match(worker$package, state$packages$package)
  state$packages$state[wpkg] <- "deps_installing"

  state
}

deps_install_done <- function(state, worker) {
  starttime <- worker$process$get_start_time()
  duration <- as.numeric(Sys.time() - starttime)
  wpkg <- match(worker$package, state$packages$package)

  worker$process$wait(timeout = 1000)
  worker$process$kill()
  if (worker$process$get_exit_status()) {
    ## failed, we just stop the whole package
    cleanup_library(state, worker)
    state$packages$state[wpkg] <- "done"

    rresult <- if (isTRUE(worker$killed)) {
      "Process was killed while installing dependencies"
      status <- "TIMEOUT"
    } else {
      status <- "PREPERROR"
      tryCatch(
        worker$process$get_result(),
        error = function(e) conditionMessage(e)
      )
    }

    result <- list(
      stdout = worker$stdout,
      stderr = worker$stderr,
      errormsg = rresult
    )

    for (which in c("old", "new")) {
      db_insert(
        state$options$pkgdir,
        worker$package,
        version = NULL,
        status = status,
        which = which,
        duration = duration,
        starttime = starttime,
        result = unclass(toJSON(result)),
        summary = NULL
      )
    }
  } else {
    ## succeeded
    state$packages$state[wpkg] <- "deps_installed"
  }

  state
}

# Not used by other methods, but simplifies debugging
deps_install <- function(
  pkgdir,
  pkgname,
  quiet = FALSE,
  new_session = FALSE,
  skip_suggests_failures = FALSE
) {
  px_opts <- deps_install_opts(
    pkgdir,
    pkgname,
    quiet = FALSE,
    skip_suggests_failures = skip_suggests_failures
  )
  execute_r(px_opts, new_session = new_session)
}

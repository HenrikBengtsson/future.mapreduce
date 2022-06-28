source("incl/start.R")
options(future.debug = FALSE)

assert_results <- function(res) {
  stopifnot(is.list(res), length(res) == 3L)
  stopifnot(all(c("globals", "packages", "scanForGlobals") %in% names(res)))
  
  globals <- res[["globals"]]
  stopifnot(inherits(globals, "Globals"), inherits(globals, "FutureGlobals"))

  packages <- res[["packages"]]
  if (!is.null(packages)) stopifnot(is.character(packages), !anyNA(packages))

  scanForGlobals <- res$scanForGlobals
  stopifnot(is.logical(scanForGlobals), length(scanForGlobals) == 1L,
            !is.na(scanForGlobals))

  invisible(res)
} # assert_results()


message("*** get_globals_and_packages_xapply() ...")

envir <- new.env()

for (globals in list(TRUE, FALSE, character(), list())) {
  FUN <- function(...) NULL
  res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir, globals = globals)
  assert_results(res)
  stopifnot(
    length(res$globals) == 2L,
    all(c("...future.FUN", "MoreArgs") %in% names(res$globals))
  )
}

## FIXME: Why doesn't 'b' show up as a global here? /HB 2021-11-25
FUN <- function(a) b
res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 0L
)


FUN <- function(pkg) utils::packageVersion(pkg)
res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 0L
)


library(utils)
FUN <- function(pkg) packageVersion(pkg)
res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 1L,
  all(c("utils") %in% res$packages)
)

FUN <- function(...) NULL
res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir, packages = "utils")
assert_results(res)

FUN <- function(...) NULL
res <- get_globals_and_packages_xapply(FUN = FUN, envir = envir, args = list(a = 42))
assert_results(res)


message(" - exceptions ...")

FUN <- function(...) NULL
envir <- new.env()

res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, globals = list(42))
}, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, globals = 42)
}, error = identity)
stopifnot(inherits(res, "error"))

...future.FUN <- 42
res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, globals = "...future.FUN")
}, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, packages = 42)
}, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, packages = NA_character_)
}, error = identity)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  get_globals_and_packages_xapply(FUN = FUN, envir = envir, packages = "")
}, error = identity)
stopifnot(inherits(res, "error"))


message("*** get_globals_and_packages_xapply() ... DONE")

#source("incl/end.R")

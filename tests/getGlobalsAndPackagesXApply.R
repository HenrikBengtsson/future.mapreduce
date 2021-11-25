source("incl/start.R")
options(future.debug = FALSE)

assert_results <- function(res) {
  stopifnot(is.list(res), length(res) == 3L)
  stopifnot(all(c("globals", "packages", "scanForGlobals") %in% names(res)))
  
  globals <- res[["globals"]]
  stopifnot(inherits(globals, "Globals"), inherits(globals, "FutureGlobals"))

  packages <- res[["packages"]]
  stopifnot(is.character(packages), !anyNA(packages))

  scanForGlobals <- res$scanForGlobals
  stopifnot(is.logical(scanForGlobals), length(scanForGlobals) == 1L,
            !is.na(scanForGlobals))

  invisible(res)
} # assert_results()


message("*** getGlobalsAndPackagesXApply() ...")

envir <- new.env()

FUN <- NULL
res <- getGlobalsAndPackagesXApply(FUN = FUN, envir = envir)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 0L
)

## FIXME: Why doesn't 'b' show up as a global here? /HB 2021-11-25
FUN <- function(a) b
res <- getGlobalsAndPackagesXApply(FUN = FUN, envir = envir, debug = TRUE)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 0L
)


FUN <- function(pkg) utils::packageVersion(pkg)
res <- getGlobalsAndPackagesXApply(FUN = FUN, envir = envir, debug = TRUE)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 0L
)


library(utils)
FUN <- function(pkg) packageVersion(pkg)
res <- getGlobalsAndPackagesXApply(FUN = FUN, envir = envir, debug = TRUE)
assert_results(res)
stopifnot(
  length(res$globals) == 2L,
  all(c("...future.FUN", "MoreArgs") %in% names(res$globals)),
  length(res$packages) == 1L,
  all(c("utils") %in% res$packages)
)

message(" - exceptions ...")

message("*** getGlobalsAndPackagesXApply() ... DONE")

#source("incl/end.R")

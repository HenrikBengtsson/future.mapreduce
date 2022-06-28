#' Identify Globals and Packages of a Map-Reduce Function Call
#'
#' @inheritParams future::getGlobalsAndPackages
#'
#' @param FUN A \link[base:function]{function} that takes one or more
#' arguments.
#'
#' @param args,MoreArgs (optional) A list of arguments passed to `FUN`.
#' Only one of `args` and `MoreArgs` may be specified at the same time.
#'
#' @param envir The \link[base:environment]{environment} from where
#' globals should be searched.
#'
#' @param packages (optional) a character vector specifying packages
#' to be attached in the \R environment evaluating the future.
#'
#' @return A names list with elements `globals`, `packages`, and
#' `scanForGlobals`.
#'
#' @importFrom globals globalsByName
#' @importFrom future as.FutureGlobals getGlobalsAndPackages resolve
#' @export
get_globals_and_packages_xapply <- function(FUN, args = NULL, MoreArgs = NULL, envir, globals = TRUE, packages = NULL) {
  use_args <- !is.null(args)

  debug <- getOption("future.debug", FALSE)

  pkgs <- NULL
  scanForGlobals <- FALSE
  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")
      scanForGlobals <- TRUE
      expr <- do.call(call, args = c(list("FUN"),
                                     if (use_args) args else MoreArgs))
    } else {
      expr <- NULL
      attr(globals, "add") <- c(attr(globals, "add"),
                                c("FUN", if (use_args) "..." else "MoreArgs"))
    }
    gp <- getGlobalsAndPackages(expr, envir = envir, globals = globals)
    globals <- gp$globals
    pkgs <- gp$packages
    gp <- NULL
      
    if (debug) {
      mdebugf(" - globals found/used: [%d] %s", length(globals), hpaste(sQuote(names(globals))))
      mdebugf(" - needed namespaces: [%d] %s", length(pkgs), hpaste(sQuote(pkgs)))
      mdebug("Finding globals ... DONE")
    }
  } else if (is.character(globals)) {
    globals <- unique(c(globals, "FUN", if (use_args) "..." else "MoreArgs"))
    globals <- globalsByName(globals, envir = envir, mustExist = FALSE)
  } else if (is.list(globals)) {
    names <- names(globals)
    if (length(globals) > 0 && is.null(names)) {
      stop("Invalid argument 'globals'. All globals must be named.")
    }
  } else {
    stopf("Invalid argument 'globals': %s", mode(globals))
  }
  globals <- as.FutureGlobals(globals)
  stop_if_not(inherits(globals, "FutureGlobals"))
  
  names <- names(globals)
  if (!is.element("FUN", names)) {
    globals <- c(globals, FUN = FUN)
  }
  
  if (use_args) {
    if (!is.element("...", names)) {
      if (debug) mdebug("Getting '...' globals ...")
      dotdotdot <- globalsByName("...", envir = envir, mustExist = TRUE)
      dotdotdot <- as.FutureGlobals(dotdotdot)
      dotdotdot <- resolve(dotdotdot)
      ## Recalculate the total size?
      maxSize <- getOption("future.globals.maxSize")
      if (is.null(maxSize) || is.finite(maxSize)) {
        objectSize <- import_future("objectSize")
        attr(dotdotdot, "total_size") <- objectSize(dotdotdot)
      }
      if (debug) mdebug("Getting '...' globals ... DONE")
      globals <- c(globals, dotdotdot)
    }
  } else if (!is.element("MoreArgs", names)) {
    globals <- c(globals, list(MoreArgs = MoreArgs))
  }

  ## Assert there are no reserved variables names among globals
  reserved <- intersect(c("...future.FUN", "...future.elements_ii",
                        "...future.seeds_ii"), names)
  if (length(reserved) > 0) {
    stopf("Detected globals using reserved variables names: %s",
         paste(sQuote(reserved), collapse = ", "))
  }
 
  ## Avoid FUN() clash with mapply(..., FUN) below.
  names <- names(globals)
  names[names == "FUN"] <- "...future.FUN"
  names(globals) <- names
  
  if (debug) {
    mdebug("Globals to be used in all futures:")
    mstr(globals)
  }

  if (!is.null(packages)) {
    stop_if_not(is.character(packages))
    packages <- unique(packages)
    stop_if_not(!anyNA(packages), all(nzchar(packages)))
    pkgs <- unique(c(pkgs, packages))
  }
  
  if (debug) {
    mdebug("Packages to be attached in all futures:")
    mstr(pkgs)
  }

  list(globals = globals, packages = pkgs, scanForGlobals = scanForGlobals)
}

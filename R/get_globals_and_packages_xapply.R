#' Identify Globals and Packages of a Map-Reduce Function Call
#'
#' @inheritParams future::getGlobalsAndPackages
#'
#' @param FUN A \link[base:function]{function} that takes one or more
#' arguments.
#'
#' @param args (optional) A list of arguments passed to `FUN`, either via
#' a named argument (`args_name`), or via \dots.
#'
#' @param args_name If `"..."`, then the arguments in `args` are passed
#' to `FUN()` as individual arguments.  If a string, then `args` as
#' passed to `FUN()` via the argument of this name.
#'
#' @param packages (optional) a character vector specifying packages
#' to be attached in the \R environment evaluating the future.
#'
#' @param envir The \link[base:environment]{environment} from where
#' globals should be searched.
#'
#' @return
#' A names list with elements:
#'
#' * `globals` - a \link[future:FutureGlobals]{FutureGlobals} object
#' * `packages` - a character vector of package names
#'
#' @importFrom globals globalsByName
#' @importFrom future as.FutureGlobals getGlobalsAndPackages resolve
#' @export
get_globals_and_packages_xapply <- function(FUN, args = NULL, args_name = "...", globals = TRUE, packages = NULL, envir = parent.frame()) {
  stop_if_not(
    length(args_name) == 1L,
    is.character(args_name),
    !is.na(args_name),
    nzchar(args_name)
  )

  debug <- getOption("future.debug", FALSE)

  pkgs <- NULL
  if (is.logical(globals)) {
    ## Gather all globals?
    if (globals) {
      if (debug) mdebug("Finding globals ...")
      expr <- do.call(call, args = c(list("FUN"), args))
    } else {
      expr <- NULL
      attr(globals, "add") <- c(attr(globals, "add"), "FUN", args_name)
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
    globals <- unique(c(globals, "FUN", args_name))
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
  
  if (args_name == "...") {
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
  } else if (!is.element(args_name, names)) {
    args <- list(args)
    names(args) <- args_name
    globals <- c(globals, args)
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

  list(
     globals = globals,
    packages = pkgs
  )
}

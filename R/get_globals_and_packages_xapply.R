#' Identify Globals and Packages of a Map-Reduce Function Call
#'
#' @inheritParams future::getGlobalsAndPackages
#'
#' @param fun A \link[base:function]{function} that takes one or more
#' arguments.
#'
#' @param fun_name The name of the argument that `fun` should be passed
#' as.
#'
#' @param args (optional) A list of arguments passed to `fun`, either via
#' a named argument (`args_name`), or via \dots.
#'
#' @param args_name If `"..."`, then the arguments in `args` are passed
#' to `fun()` as individual arguments.  If a string, then `args` as
#' passed to `fun()` via the argument of this name.
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
get_globals_and_packages_xapply <- function(fun, fun_name = "FUN", args = NULL, args_name = "...", globals = TRUE, packages = NULL, envir = parent.frame()) {
  stop_if_not(
    length(fun_name) == 1L,
    is.character(fun_name),
    !is.na(fun_name),
    nzchar(fun_name)
  )
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
      expr <- do.call(call, args = c(list(fun_name), args))
    } else {
      expr <- NULL
      attr(globals, "add") <- c(attr(globals, "add"), fun_name, args_name)
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
    globals <- unique(c(globals, fun_name, args_name))
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
  if (!is.element(fun_name, names)) {
    if (packageVersion("globals") >= "0.15.1-9005"  ) {
      globals <- globals
      globals[[fun_name]] <- fun
    } else {
      fun <- list(fun)
      names(fun) <- fun_name
      globals <- c(globals, fun)
    }
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
  reserved <- intersect(
    c(
      sprintf("...future.%s", fun_name),
      "...future.elements_ii",
      "...future.seeds_ii"
    ),
    names
  )
  if (length(reserved) > 0) {
    stopf("Detected globals using reserved variables names: %s",
         paste(sQuote(reserved), collapse = ", "))
  }
 
  ## Avoid fun() clash with mapply(..., fun) below.
  names <- names(globals)
  names[names == fun_name] <- sprintf("...future.%s", fun_name)
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

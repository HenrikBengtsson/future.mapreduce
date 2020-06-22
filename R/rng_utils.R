get_random_seed <- function() {
  env <- globalenv()
  env$.Random.seed
}

set_random_seed <- function(seed) {
  env <- globalenv()
  if (is.null(seed)) {
    rm(list = ".Random.seed", envir = env, inherits = FALSE)
  } else {
    env$.Random.seed <- seed
  }
}

next_random_seed <- function(seed = get_random_seed()) {
  sample.int(n = 1L, size = 1L, replace = FALSE)
  seed_next <- get_random_seed()
  stop_if_not(!any(seed_next != seed))
  invisible(seed_next)
}

is_valid_random_seed <- function(seed) {
  oseed <- get_random_seed()
  on.exit(set_random_seed(oseed))
  env <- globalenv()
  env$.Random.seed <- seed
  res <- tryCatch({
    sample.int(n = 1L, size = 1L, replace = FALSE)
  }, simpleWarning = function(w) w)
  !inherits(res, "simpleWarning")
}

## For RNGkind("L'Ecuyer-CMRG") we should have (see help('RNGkind')):
##   .Random.seed <- c(rng.kind, n) where length(n) == 6L.
## From R source code: check for rng.kind %% 10000L == 407L
is_lecyer_cmrg_seed <- function(seed) {
  is.numeric(seed) &&
    length(seed) == 7L &&
    all(is.finite(seed)) &&
    (seed[1] %% 10000L == 407L)
}

# @importFrom utils capture.output
as_lecyer_cmrg_seed <- function(seed) {
  ## Generate a L'Ecuyer-CMRG seed (existing or random)?
  if (is.logical(seed)) {
    stop_if_not(length(seed) == 1L)
    if (!is.na(seed) && !seed) {
      stop("Argument 'seed' must be TRUE if logical: ", seed)
    }

    oseed <- get_random_seed()
    
    ## Already a L'Ecuyer-CMRG seed?  Then use that as is.
    if (!is.na(seed) && seed) {
      if (is_lecyer_cmrg_seed(oseed)) return(oseed)
    }
    
    ## Otherwise, generate a random one.
    on.exit(set_random_seed(oseed), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    return(get_random_seed())
  }

  stop_if_not(is.numeric(seed), all(is.finite(seed)))
  seed <- as.integer(seed)

  ## Already a L'Ecuyer-CMRG seed?
  if (is_lecyer_cmrg_seed(seed)) {
    return(seed)
  }

  ## Generate a new L'Ecuyer-CMRG seed?
  if (length(seed) == 1L) {
    oseed <- get_random_seed()
    on.exit(set_random_seed(oseed), add = TRUE)
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    return(get_random_seed())
  }
  
  stop("Argument 'seed' must be L'Ecuyer-CMRG RNG seed as returned by parallel::nextRNGStream() or an single integer: ", capture.output(str(seed)))
}

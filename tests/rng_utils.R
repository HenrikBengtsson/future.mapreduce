source("incl/start,load-only.R")

as_lecyer_cmrg_seed <- future:::as_lecyer_cmrg_seed
is_lecyer_cmrg_seed <- future:::is_lecyer_cmrg_seed

message("*** RNG ...")

message(" - make_rng_seeds ...")

seeds <- make_rng_seeds(2L, seed = NULL)
stopifnot(is.null(seeds))

seeds <- make_rng_seeds(2L, seed = FALSE)
stopifnot(is.null(seeds))

seeds <- make_rng_seeds(0L, seed = 42L)
stopifnot(length(seeds) == 0L, identical(seeds, list()))

seeds <- make_rng_seeds(2L, seed = TRUE)
stopifnot(length(seeds) == 2L, all(sapply(seeds, FUN = is_lecyer_cmrg_seed)))

seeds <- make_rng_seeds(3L, seed = 42L)
stopifnot(length(seeds) == 3L, all(sapply(seeds, FUN = is_lecyer_cmrg_seed)))

seeds <- make_rng_seeds(1L, seed = 42L)
stopifnot(length(seeds) == 1L, all(sapply(seeds, FUN = is_lecyer_cmrg_seed)))

seeds0 <- lapply(1:3, FUN = as_lecyer_cmrg_seed)
seeds <- make_rng_seeds(length(seeds0), seed = seeds0)
stopifnot(length(seeds) == length(seeds0),
          all(sapply(seeds, FUN = is_lecyer_cmrg_seed)))


message(" - exceptions ...")

seed <- as_lecyer_cmrg_seed(seed = 42L)

## Invalid L'Ecuyer seed
seed_invalid <- seed + 1L
res <- tryCatch({
  seed <- as_lecyer_cmrg_seed(seed = seed_invalid)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## Invalid seed
res <- tryCatch({
  seed <- as_lecyer_cmrg_seed(seed = 1:2)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## Invalid length
seeds0 <- lapply(1:2, FUN = as_lecyer_cmrg_seed)
res <- tryCatch({
  seeds <- make_rng_seeds(1L, seed = seeds0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## Seeds of different kinds
seeds0 <- lapply(1:2, FUN = as_lecyer_cmrg_seed)
seeds0[[1]] <- seeds0[[1]][-1]
res <- tryCatch({
  seeds <- make_rng_seeds(2L, seed = seeds0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## List of scalar seeds?
res <- tryCatch({
  seeds <- make_rng_seeds(1L, seed = list(42L))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## Not seeds at all?
seeds0 <- lapply(1:2, FUN = as_lecyer_cmrg_seed)
seeds0[[1]] <- letters[1:7]
res <- tryCatch({
  seeds <- make_rng_seeds(2L, seed = seeds0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## Invalid seeds?
seeds0 <- lapply(1:2, FUN = as_lecyer_cmrg_seed)
seeds0 <- lapply(seeds0, FUN = rev)
res <- tryCatch({
  seeds <- make_rng_seeds(2L, seed = seeds0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

message("*** RNG ... DONE")

source("incl/end.R")

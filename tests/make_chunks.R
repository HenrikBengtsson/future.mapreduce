source("incl/start.R")

assert_all_indices <- function(chunks, nelements = max(unlist(chunks))) {
  stopifnot(is.list(chunks))
  idxs <- unlist(chunks, use.names = FALSE)
  stopifnot(is.numeric(idxs), !anyNA(idxs), all(idxs >= 1), all(is.finite(idxs)), all(idxs <= nelements))
  idxs <- sort(idxs)
  stopifnot(!any(duplicated(idxs)))
  stopifnot(all(diff(idxs) == 1L))

  ordering <- attr(chunks, "ordering")
  if (!is.null(ordering)) {
    stopifnot(
      is.numeric(ordering), length(ordering) == nelements,
      !anyNA(ordering), all(ordering >= 1L), all(ordering <= n),
      !any(duplicated(ordering))
    )
  }

  invisible(idxs)
}

message("*** make_chunks() ...")

n <- 0L
chunks <- make_chunks(nelements = n, nworkers = 1L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(integer(0L))))

n <- 1L
chunks <- make_chunks(nelements = n, nworkers = 1L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1L)))

n <- 2L
chunks <- make_chunks(nelements = n, nworkers = 1L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:2)))

n <- 1L
chunks <- make_chunks(nelements = n, nworkers = 2L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1L)))

n <- 2L
chunks <- make_chunks(nelements = n, nworkers = 2L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1L, 2L)))

n <- 3L
chunks <- make_chunks(nelements = n, nworkers = 2L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1L, 2:3)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = +Inf)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, as.list(1:8)))


message(" - make_chunks(..., chunk_size) ...")

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 1L, chunk_size = 2L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, chunk_size = 2L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, chunk_size = 10L)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 1L, chunk_size = Inf)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:8)))

## FIXME: Add 'ordering' attribute tests

n <- 8L
ordering <- "random"
chunks <- make_chunks(nelements = n, nworkers = 3L, chunk_size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nelements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by make_chunks()
stopifnot(all.equal(chunks, list(1:2, 3:4, 5:6, 7:8), check.attributes = FALSE))

n <- 8L
ordering <- 8:1
chunks <- make_chunks(nelements = n, nworkers = 3L, chunk_size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nelements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by make_chunks()
stopifnot(identical(chunks, structure(list(1:2, 3:4, 5:6, 7:8), ordering = ordering)))

n <- 8L
ordering <- function(nelements) {
  rev(seq_len(nelements))
}
chunks <- make_chunks(nelements = n, nworkers = 3L, chunk_size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nelements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by make_chunks()
stopifnot(all.equal(chunks, list(1:2, 3:4, 5:6, 7:8), check.attributes = FALSE))


message(" - make_chunks(..., scheduling) ...")

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = FALSE)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, as.list(1:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = TRUE)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 2L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = TRUE)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1L, 2L)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = 0.0)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = 1.0)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = 1.2)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = 2.0)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, list(1:2, 3L, 4L, 5L, 6L, 7:8)))

n <- 8L
chunks <- make_chunks(nelements = n, nworkers = 3L, scheduling = +Inf)
assert_all_indices(chunks, nelements = n)
stopifnot(identical(chunks, as.list(1:8)))

## FIXME: Add 'ordering' attribute tests


message(" - exceptions ...")

res <- tryCatch({
  chunks <- make_chunks()
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nworkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = -1L, nworkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: Give a more informative error message
res <- tryCatch({
  chunks <- make_chunks(nelements = Inf, nworkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = "a", nworkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: Give an error message
res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = "a")
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, chunk_size = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, chunk_size = NA_integer_)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, chunk_size = 1:2)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = -1.0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = NA_real_)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = 1:2)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = c(TRUE, FALSE))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: This skips the validation of 'ordering'
res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = structure(TRUE, ordering = "unknown"))
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 2L, nworkers = 1L, scheduling = structure(TRUE, ordering = "unknown"))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: This skips the validation of 'ordering'
res <- tryCatch({
  chunks <- make_chunks(nelements = 0L, nworkers = 1L, scheduling = structure(TRUE, ordering = FALSE))
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- make_chunks(nelements = 2L, nworkers = 1L, scheduling = structure(TRUE, ordering = FALSE))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

message("*** make_chunks() ... DONE")

source("incl/end.R")

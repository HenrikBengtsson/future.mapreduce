source("incl/start.R")

assert_all_indices <- function(chunks, nbrOfElements = max(unlist(chunks))) {
  stopifnot(is.list(chunks))
  idxs <- unlist(chunks, use.names = FALSE)
  stopifnot(is.numeric(idxs), !anyNA(idxs), all(idxs >= 1), all(is.finite(idxs)), all(idxs <= nbrOfElements))
  idxs <- sort(idxs)
  stopifnot(!any(duplicated(idxs)))
  stopifnot(all(diff(idxs) == 1L))

  ordering <- attr(chunks, "ordering")
  if (!is.null(ordering)) {
    stopifnot(
      is.numeric(ordering), length(ordering) == nbrOfElements,
      !anyNA(ordering), all(ordering >= 1L), all(ordering <= n),
      !any(duplicated(ordering))
    )
  }

  invisible(idxs)
}

message("*** makeChunks() ...")

n <- 0L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 1L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(integer(0L))))

n <- 1L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 1L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1L)))

n <- 2L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 1L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:2)))

n <- 1L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 2L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1L)))

n <- 2L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 2L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1L, 2L)))

n <- 3L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 2L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1L, 2:3)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = +Inf)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, as.list(1:8)))


message(" - makeChunks(..., future.chunk.size) ...")

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 1L, future.chunk.size = 2L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.chunk.size = 2L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.chunk.size = 10L)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 1L, future.chunk.size = Inf)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:8)))

## FIXME: Add 'ordering' attribute tests

n <- 8L
ordering <- "random"
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.chunk.size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nbrOfElements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by makeChunks()
stopifnot(all.equal(chunks, list(1:2, 3:4, 5:6, 7:8), check.attributes = FALSE))

n <- 8L
ordering <- 8:1
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.chunk.size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nbrOfElements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by makeChunks()
stopifnot(identical(chunks, structure(list(1:2, 3:4, 5:6, 7:8), ordering = ordering)))

n <- 8L
ordering <- function(nbrOfElements) {
  rev(seq_len(nbrOfElements))
}
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.chunk.size = structure(2L, ordering = ordering))
assert_all_indices(chunks, nbrOfElements = n)
ns <- vapply(chunks, FUN.VALUE = NA_integer_, FUN = length)
stopifnot(all(ns == 2L))
## Note, indices in the chunks are _not_ reordered by makeChunks()
stopifnot(all.equal(chunks, list(1:2, 3:4, 5:6, 7:8), check.attributes = FALSE))


message(" - makeChunks(..., future.scheduling) ...")

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = FALSE)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, as.list(1:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = TRUE)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 2L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = TRUE)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1L, 2L)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = 0.0)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = 1.0)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = 1.2)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = 2.0)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, list(1:2, 3L, 4L, 5L, 6L, 7:8)))

n <- 8L
chunks <- makeChunks(nbrOfElements = n, nbrOfWorkers = 3L, future.scheduling = +Inf)
assert_all_indices(chunks, nbrOfElements = n)
stopifnot(identical(chunks, as.list(1:8)))

## FIXME: Add 'ordering' attribute tests


message(" - exceptions ...")

res <- tryCatch({
  chunks <- makeChunks()
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfWorkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = -1L, nbrOfWorkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: Give a more informative error message
res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = Inf, nbrOfWorkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = "a", nbrOfWorkers = 1L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: Give an error message
res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = "a")
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.chunk.size = 0L)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.chunk.size = NA_integer_)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.chunk.size = 1:2)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = -1.0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = NA_real_)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = 1:2)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = c(TRUE, FALSE))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: This skips the validation of 'ordering'
res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = structure(TRUE, ordering = "unknown"))
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 2L, nbrOfWorkers = 1L, future.scheduling = structure(TRUE, ordering = "unknown"))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

## FIXME: This skips the validation of 'ordering'
res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = structure(TRUE, ordering = FALSE))
}, error = identity)
print(res)
#stopifnot(inherits(res, "error"))

res <- tryCatch({
  chunks <- makeChunks(nbrOfElements = 2L, nbrOfWorkers = 1L, future.scheduling = structure(TRUE, ordering = FALSE))
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))

message("*** makeChunks() ... DONE")

source("incl/end.R")

source("incl/start.R")

message("*** makeChunks() ...")

chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L)
stopifnot(identical(chunks, list(integer(0L))))

chunks <- makeChunks(nbrOfElements = 1L, nbrOfWorkers = 1L)
stopifnot(identical(chunks, list(1L)))

chunks <- makeChunks(nbrOfElements = 2L, nbrOfWorkers = 1L)
stopifnot(identical(chunks, list(1:2)))

chunks <- makeChunks(nbrOfElements = 1L, nbrOfWorkers = 2L)
stopifnot(identical(chunks, list(1L)))

chunks <- makeChunks(nbrOfElements = 2L, nbrOfWorkers = 2L)
stopifnot(identical(chunks, list(1L, 2L)))

chunks <- makeChunks(nbrOfElements = 3L, nbrOfWorkers = 2L)
stopifnot(identical(chunks, list(1L, 2:3)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = +Inf)
stopifnot(identical(chunks, as.list(1:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 1L, future.chunk.size = 2L)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.chunk.size = 2L)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.chunk.size = 10L)
stopifnot(identical(chunks, list(1:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 1L, future.chunk.size = Inf)
stopifnot(identical(chunks, list(1:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.scheduling = 0.0)
stopifnot(identical(chunks, list(1:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.scheduling = 1.0)
stopifnot(identical(chunks, list(1:3, 4:5, 6:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.scheduling = 1.2)
stopifnot(identical(chunks, list(1:2, 3:4, 5:6, 7:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.scheduling = 2.0)
stopifnot(identical(chunks, list(1:2, 3L, 4L, 5L, 6L, 7:8)))

chunks <- makeChunks(nbrOfElements = 8L, nbrOfWorkers = 3L, future.scheduling = +Inf)
stopifnot(identical(chunks, as.list(1:8)))


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
  chunks <- makeChunks(nbrOfElements = 0L, nbrOfWorkers = 1L, future.scheduling = -1.0)
}, error = identity)
print(res)
stopifnot(inherits(res, "error"))


message("*** makeChunks() ... DONE")

source("incl/end.R")

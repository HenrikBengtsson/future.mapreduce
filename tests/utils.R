source("incl/start,load-only.R")

message("*** utils ...")

message("*** hpaste() ...")

# Some vectors
x <- 1:6
y <- 10:1
z <- LETTERS[x]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Abbreviation of output vector
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
printf("x = %s.\n", hpaste(x))
## x = 1, 2, 3, ..., 6.

printf("x = %s.\n", hpaste(x, maxHead = 2))
## x = 1, 2, ..., 6.

printf("x = %s.\n", hpaste(x), maxHead = 3) # Default
## x = 1, 2, 3, ..., 6.

# It will never output 1, 2, 3, 4, ..., 6
printf("x = %s.\n", hpaste(x, maxHead = 4))
## x = 1, 2, 3, 4, 5 and 6.

# Showing the tail
printf("x = %s.\n", hpaste(x, maxHead = 1, maxTail = 2))
## x = 1, ..., 5, 6.

# Turning off abbreviation
printf("y = %s.\n", hpaste(y, maxHead = Inf))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

## ...or simply
printf("y = %s.\n", paste(y, collapse = ", "))
## y = 10, 9, 8, 7, 6, 5, 4, 3, 2, 1

# Change last separator
printf("x = %s.\n", hpaste(x, lastCollapse = " and "))
## x = 1, 2, 3, 4, 5 and 6.

# No collapse
stopifnot(all(hpaste(x, collapse = NULL) == x))

# Empty input
stopifnot(identical(hpaste(character(0)), character(0)))

message("*** hpaste() ...")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# debug()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** mdebug() ...")

mdebug("Hello #", 1)
mdebugf("Hello #%d", 1)
options(future.debug = TRUE)

mdebug("Hello #", 2)
mdebugf("Hello #%d", 2)
options(future.debug = FALSE)

mdebug("Hello #", 3)
mdebugf("Hello #%d", 3)

message("*** mdebug() ... DONE")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# import_from()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** import_from() ...")

obj <- import_from("sample.int", package = "base")
stopifnot(identical(obj, base::sample.int))

obj <- import_future("future")
stopifnot(identical(obj, future::future))

obj <- import_from("non-existing-fcn", default = NA, package = "future")
stopifnot(identical(obj, NA))

res <- tryCatch({
  obj <- import_from("non-existing-fcn", package = "future")
}, error = identity)
print(res)
stopifnot(inherits(res, "simpleError"))

message("*** import_from() ... DONE")


message("*** stop_if_not() ...")

stop_if_not(TRUE)
stop_if_not(TRUE, TRUE)
res <- tryCatch({
  stop_if_not(FALSE)
}, error = identity)
stopifnot(inherits(res, "simpleError"))
res <- tryCatch({
  stop_if_not(list(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
}, error = identity)
stopifnot(inherits(res, "simpleError"))

message("*** stop_if_not() ... DONE")


message("*** assert_values2() ...")

assert_values2(nX = 2L, values2 = as.list(1:2))
res <- tryCatch({
  assert_values2(nX = 1L, values = as.list(1:2), values2 = as.list(1:2), fcn_name = "tests", debug = TRUE)
}, error = identity)
print(res)
stopifnot(inherits(res, "FutureError"))

message("*** assert_values2() ... DONE")


message("*** stealth_sample.int() ...")

if (exists(".Random.seed", envir = globalenv())) {
  rm(list = ".Random.seed", envir = globalenv())
}

seed <- globalenv()[[".Random.seed"]]
stopifnot(is.null(seed))
y <- stealth_sample.int(n = 10L, size = 1L)
stopifnot(identical(globalenv()[[".Random.seed"]], seed))
stopifnot(is.integer(y), !anyNA(y), length(y) == 1L, all(y >= 1L), all(y <= 10L))

dummy <- sample.int(1L)
seed <- globalenv()[[".Random.seed"]]
stopifnot(!is.null(seed))
y <- stealth_sample.int(n = 10L, size = 1L)
stopifnot(identical(globalenv()[[".Random.seed"]], seed))
stopifnot(is.integer(y), !anyNA(y), length(y) == 1L, all(y >= 1L), all(y <= 10L))

message("*** stealth_sample.int() ... DONE")

message("*** stopf(), warnf(), msgf() ...")

for (fcn in list(stopf, warnf, msgf)) {
  res <- tryCatch(fcn("boom"), condition = identity)
  print(res)
  stopifnot(inherits(res, "condition"))
}

message("*** stopf(), warnf(), msgf() ... DONE")

message("*** mprint(), mstr() ...")

mprint(1, debug = TRUE)
mstr(1, debug = TRUE)

message("*** mprint(), mstr() ... DONE")


v <- future.mapreduce:::future_version()
message(sprintf("future version: %s", sQuote(v)))

for (name in c("digits", "unknown")) {
  v <- future.mapreduce:::getOption(name)
  message(sprintf("option %s: %s", sQuote(name), sQuote(v)))
  v <- future.mapreduce:::getOption(name, default = NA)
  message(sprintf("option %s: %s", sQuote(name), sQuote(v)))
}

message("*** utils ... DONE")

source("incl/end.R")

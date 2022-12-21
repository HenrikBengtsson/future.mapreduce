#' Create Chunks of Index Vectors
#'
#' _This is an internal function._
#' 
#' @param nelements (integer) Total number of elements to iterate over.
#' 
#' @param nworkers (integer) Number of workers available.
#' 
#' @param scheduling (numeric) A strictly positive scalar.
#' Only used if argument `chunk_size` is `NULL`.
#'
#' @param chunk_size (numeric) The maximum number of elements per
#' chunk, or `NULL`.  If `NULL`, then the chunk sizes are given by the
#' `scheduling` argument.
#'
#' @return A list of chunks, where each chunk is an integer vector of
#' unique indices in `[1, nelements]`.  The union of all chunks
#' holds `nelements` elements and equals `1:nelements`.
#' If `nelements == 0`, then an empty list is returned.
#'
#' @section Control processing order of elements:
#' Attribute `ordering` of `chunk_size` or `scheduling` can
#' be used to control the ordering the elements are iterated over, which
#' only affects the processing order _not_ the order values are returned.
#' This attribute can take the following values:
#' * index vector - an numeric vector of length `nelements` specifying
#'                  how elements are remapped
#' * function     - an function taking one argument which is called as
#'                  `ordering(nelements)` and which must return an
#'                  index vector of length `nelements`, e.g.
#'                  `function(n) rev(seq_len(n))` for reverse ordering.
#' * `"random"`   - this will randomize the ordering via random index
#'                  vector `sample.int(nelements)`.
#' 
#' @importFrom parallel splitIndices
#' @export
make_chunks <- function(nelements, nworkers,
                        scheduling = 1.0, chunk_size = NULL) {
  stop_if_not(nelements >= 0L, nworkers >= 1L)

  ## 'chunk_size != NULL' takes precedence over 'scheduling'
  if (!is.null(chunk_size)) {
    stop_if_not(length(chunk_size) == 1L, !is.na(chunk_size),
                chunk_size > 0)
    ## Same definition as parallel:::staticNChunks() in R (>= 3.5.0)
    nchunks <- max(1, ceiling(nelements / chunk_size))

    ## Customized ordering?
    ordering <- attr(chunk_size, "ordering", exact = TRUE)
  } else {
    if (is.logical(scheduling)) {
      stop_if_not(length(scheduling) == 1L, !is.na(scheduling))
      if (scheduling) {
        nchunks <- nworkers
        if (nchunks > nelements) nchunks <- nelements
      } else {
        nchunks <- nelements
      }
    } else {
      ## Treat 'scheduling' as the number of chunks per worker, i.e.
      ## the number of chunks each worker should process on average.
      stop_if_not(length(scheduling) == 1L, !is.na(scheduling),
                  scheduling >= 0)
      if (nworkers > nelements) nworkers <- nelements
      nchunks <- scheduling * nworkers
      if (nchunks < 1L) {
        nchunks <- 1L
      } else if (nchunks > nelements) {
        nchunks <- nelements
      }
    }
    
    ## Customized ordering?
    ordering <- attr(scheduling, "ordering", exact = TRUE)
  }

  chunks <- splitIndices(nelements, ncl = nchunks)
  
  ## Customized ordering?
  if (nelements > 1L && !is.null(ordering)) {
    if (is.character(ordering) && ordering == "random") {
      map <- stealth_sample.int(nelements)
    } else if (is.numeric(ordering)) {
      map <- ordering
    } else if (is.function(ordering)) {
      map <- ordering(nelements)
    } else {
      stopf("Unknown value of attribute %s for argument %s: %s", "ordering", if (!is.null(chunk_size)) "chunk_size" else "scheduling", mode(ordering))
    }

    if (!is.null(map)) {
      ## Simple validity check of "ordering".  Looking for NAs, range,
      ## uniqueness is too expensive so skipped.
      stop_if_not(length(map) == nelements)
      attr(chunks, "ordering") <- map
    }
  }
  
  chunks
}

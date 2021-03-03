#' Generate typos using multiple methods
#'
#' This is a simple wrapper to let users generate typos using multiple methods
#' in a single function call. Results returned by this function should be
#' equivalent to the union of all functions supplied to `methods`.
#'
#' @param vec A vector of strings to attempt to generate typos for.
#' @param methods A vector of strings matching the methods (listed in
#' [typo_addition]) to use for typo generation. Set to `"*"` or `"all"` to
#' generate the maximum number of typos using default character sets.
#' @param chars Characters to use in generating typos. If set to `NA`, will use
#' the default character sets for each method; otherwise, `chars[[i]]` should be
#' a character set usable by `methods[[i]]`.
#'
#' @return A named list of length `length(vec)` containing all unique
#' permutations from the typo-generating methods.
#'
#' @examples
#' generate_typos(c("Michael", "David"), c("typo_addition", "typo_prefix"))
#' @md
#' @export
generate_typos <- function(vec, methods, chars = NA) {
  if (length(methods) == 1 && (methods == "*" | methods == "all")) {
    methods <- c(
      "typo_addition",
      "typo_prefix",
      "typo_bitsquat",
      "typo_insertion",
      "typo_omission",
      "typo_transposition"
    )
    chars <- NA
  }

  stats::setNames(
    lapply(
      vec,
      function(v) {
        unique(
          stats::setNames(
            unlist(
              mapply(
                function(m, c) {
                  if (is.na(c)) do.call(m, list(v)) else do.call(m, list(v, c))
                },
                m = methods,
                c = chars
              )
            ),
            NULL
          )
        )
      }
    ),
    vec
  )
}

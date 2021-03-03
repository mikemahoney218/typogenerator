#' Methods for typo generation
#'
#' These functions implement various strategies to generate variants of strings.
#' This is a partial implementation of Normand (2020); some variants are not
#' implemented due to being out of scope (such as homoglyph typos, which this
#' package does not intend to exhaustively cover). All typo permutations are
#' returned in lowercase, and case differences are not considered typos.
#'
#' @param vec A vector of strings to attempt to generate typos for.
#' @param chars Characters to use in generating typos. Takes one of three
#' formats:
#'
#'  * For `typo_doublehit` and `typo_replace`, a named list (such as the one
#'    returned by [qwerty_neighbors]) where the names of the list represent
#'    individual characters and the elements of the list are characters to
#'    append or replace.
#'  * For `typo_addition`, `typo_prefix`, `typo_bitsquat`, `typo_insertion`,
#'    and `typo_vowelswap`, a vector of acceptable characters to add, swap, or
#'    insert.
#'  * For `typo_repetition`, `typo_omission`, and `typo_transposition`,
#'    this argument is ignored.
#'
#'
#' @section Methods Implemented:
#'
#' At the moment 12 methods are implemented.
#'
#' * `typo_addition` adds characters to the end of a string.
#' * `typo_prefix` adds characters to the start of a string.
#' * `typo_bitsquat` changes a single character at a time, moving through the
#'   length of the string (so that each permutation is one bit different from
#'   the original input).
#' * `typo_repetition` repeats a single character to simulate pressing a key
#'   twice.
#' * `typo_insertion` inserts a single character into each position in the
#'   string. `typo_hyphenation` is equivalent to
#'   `typo_insertion(vec, chars = "-")` and `typo_subdomain` is equivalent to
#'   `typo_insertion(vec, chars = ".")`.
#' * `typo_omission` removes a single character from the string.
#' * `typo_vowelswap` replaces the vowels in the string with the other vowels.
#'   Values provided to `chars` are used to define the full set of vowels;
#'   by default, `y` is not considered to be a vowel.
#' * `typo_transposition` swaps adjacent characters throughout the string.
#' * `typo_replace` replaces a single character with a set of characters (such
#'   as those close to the key on a keyboard).
#' * `typo_doublehit` is similar to `typo_replace` but inserts the wrong
#'   character rather than replacing with it (to simulate hitting two keys at
#'   once).
#'
#' @return A list of length `length(vec)` using `vec` as names, with each
#' element of the list containing a character vector of potential typos.
#'
#' @examples
#' typo_bitsquat("Michael")
#'
#' typo_replace(c("Michael", "David"))
#' @references
#' Thibault Normand (2020). Typogenerator: a typosquatting generator in Golang.
#' Golang package version 0.2.0. \url{https://github.com/zntrio/typogenerator}
#'
#' @rdname typo_generation
#' @md
#' @export
typo_addition <- function(vec, chars = gh_allowed()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) paste0(x, chars)
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_prefix <- function(vec, chars = gh_allowed()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) paste0(chars, x)
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_bitsquat <- function(vec, chars = gh_allowed()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) {
        string <- strsplit(x, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen)) {
          res[[i]] <- vapply(
            chars,
            function(y) {
              paste0(
                c(
                  string[0:(i - 1)],
                  y,
                  if (i == strlen) NULL else string[(i + 1):strlen]
                ),
                collapse = ""
              )
            },
            character(1)
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_repetition <- function(vec, chars = NULL) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(string) {
        string <- strsplit(string, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen)) {
          res[[i]] <- paste0(
            c(
              string[0:i],
              string[[i]],
              if (i == strlen) NULL else string[(i + 1):strlen]
            ),
            collapse = ""
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_insertion <- function(vec, chars = gh_allowed()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(string) {
        string <- strsplit(string, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen + 1)) {
          res[[i]] <- vapply(
            chars,
            function(x) {
              paste0(
                c(
                  string[0:(i - 1)],
                  x,
                  if (i > strlen) NULL else string[(i):strlen]
                ),
                collapse = ""
              )
            },
            character(1)
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_omission <- function(vec, chars = NULL) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) {
        string <- strsplit(x, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen)) {
          res[[i]] <- paste0(
            c(
              string[0:(i - 1)],
              if (i == strlen) NULL else string[(i + 1):strlen]
            ),
            collapse = ""
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_vowelswap <- function(vec, chars = c("a", "e", "i", "o", "u")) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) {
        string <- strsplit(x, "")[[1]]
        strlen <- length(string)
        strvowels <- which(string %in% chars)
        res <- vector("list", length(strvowels))
        for (i in seq_len(length(strvowels))) {
          res[[i]] <- vapply(
            chars,
            function(y) {
              paste0(
                c(
                  string[0:(strvowels[[i]] - 1)],
                  y,
                  if (strvowels[[i]] == strlen) NULL else string[(strvowels[[i]] + 1):strlen]
                ),
                collapse = ""
              )
            },
            character(1)
          )
        }
        res <- unique(unlist(res))
        res[-which(res == x)]
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_transposition <- function(vec, chars = NULL) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(string) {
        string <- strsplit(string, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen - 1)) {
          res[[i]] <- paste0(
            c(
              string[0:(i - 1)],
              string[[i + 1]],
              string[[i]],
              if (i >= strlen - 1) NULL else string[(i + 2):strlen]
            ),
            collapse = ""
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_replace <- function(vec, chars = qwerty_neighbors()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) {
        string <- strsplit(x, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen)) {
          res[[i]] <- vapply(
            chars[[which(names(chars) == tolower(string[[i]]))]],
            function(y) {
              paste0(
                c(
                  string[0:(i - 1)],
                  y,
                  if (i == strlen) NULL else string[(i + 1):strlen]
                ),
                collapse = ""
              )
            },
            character(1)
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_doublehit <- function(vec, chars = qwerty_neighbors()) {
  stats::setNames(
    lapply(
      tolower(vec),
      function(x) {
        string <- strsplit(x, "")[[1]]
        strlen <- length(string)
        res <- vector("list", strlen)
        for (i in seq_len(strlen)) {
          res[[i]] <- c(
            vapply(
              chars[[which(names(chars) == tolower(string[[i]]))]],
              function(y) {
                paste0(
                  c(
                    string[0:(i)],
                    y,
                    if (i == strlen) NULL else string[(i + 1):strlen]
                  ),
                  collapse = ""
                )
              },
              character(1)
            ),
            vapply(
              chars[[which(names(chars) == tolower(string[[i]]))]],
              function(y) {
                paste0(
                  c(
                    string[0:(i - 1)],
                    y,
                    string[(i):strlen]
                  ),
                  collapse = ""
                )
              },
              character(1)
            )
          )
        }
        unique(unlist(res))
      }
    ),
    vec
  )
}

#' @rdname typo_generation
#' @export
typo_hyphenation <- function(vec, chars = "-") typo_insertion(vec, chars)

#' @rdname typo_generation
#' @export
typo_subdomain <- function(vec, chars = ".") typo_insertion(vec, chars)

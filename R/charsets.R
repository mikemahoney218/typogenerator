#' Create a vector of characters allowed in GitHub repository names
#'
#' @return A character vector of length 39 containing characters allowed in
#' GitHub repository names.
#'
#' @export
gh_allowed <- function() {
  vapply(c(45:46, 48:57, 95, 97:122),
         intToUtf8,
         character(1))
}

#' Create a list of characters which neighbor one another on QWERTY keyboards
#'
#' @return A list of length 37. Each element of the list is named with a key
#' on the QWERTY keyboard and contains a character vector of neighboring keys.
#'
#' @export
qwerty_neighbors <- function() {
  list(
    'q' = c('1', '2', 'w', 'a'),
    'w' = c('2', '3', 'e', 's', 'a', 'q'),
    'e' = c('3', '4', 'r', 'd', 's', 'w'),
    'r' = c('4', '5', 't', 'f', 'd', 'e'),
    't' = c('5', '6', 'y', 'g', 'f', 'r'),
    'y' = c('6', '7', 'u', 'h', 'g', 't'),
    'u' = c('7', '8', 'i', 'j', 'h', 'y'),
    'i' = c('8', '9', 'o', 'k', 'j', 'u'),
    'o' = c('9', '0', 'p', 'l', 'k', 'i'),
    'p' = c('0', '-', 'l', 'o'),
    'a' = c('q', 'w', 's', 'z'),
    's' = c('w', 'e', 'd', 'x', 'z', 'a'),
    'd' = c('e', 'r', 'f', 'c', 'x', 's'),
    'f' = c('r', 't', 'g', 'v', 'c', 'd'),
    'g' = c('t', 'y', 'h', 'b', 'v', 'f'),
    'h' = c('y', 'u', 'j', 'n', 'b', 'g'),
    'j' = c('u', 'i', 'k', 'm', 'n', 'h'),
    'k' = c('i', 'o', 'l', 'm', 'j'),
    'l' = c('o', 'p', 'k'),
    'z' = c('a', 's', 'x'),
    'x' = c('s', 'd', 'c', 'z'),
    'c' = c('d', 'f', 'v', 'x'),
    'v' = c('f', 'g', 'b', 'c'),
    'b' = c('g', 'h', 'n', 'v'),
    'n' = c('h', 'j', 'm', 'b'),
    'm' = c('j', 'k', 'n'),
    '1' = c('2', 'q'),
    '2' = c('1', '3', 'w', 'q'),
    '3' = c('2', '4', 'e', 'w'),
    '4' = c('3', '5', 'r', 'e'),
    '5' = c('4', '6', 't', 'r'),
    '6' = c('5', '7', 'y', 't'),
    '7' = c('6', '8', 'u', 'y'),
    '8' = c('7', '9', 'i', 'u'),
    '9' = c('8', '0', 'o', 'i'),
    '0' = c('9', 'p', 'o', '-'),
    '-' = c('0', 'p')
  )
}

# TODO:
# - Switch purrr calls to base R?
# - write tests
# - add function for constructing full call for passing to system2
# - ERROR CHECK that arg & user-friendly name aren't both used
#   - Warn user, default behavior to use raw flag over user-friendly name
# - handle BOOL flags correctly (UNTESTED)
# - handle vector arguments

#' return function dots as named list
#'
#' @param ... dots from function call
#'
#' @return named list of kwargs from ...
#' @export
#'
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  myFunction(example = "hello", example2 = "world", boolFlag = TRUE)
getDots <- function(...){
  dots <- eval(substitute(list(...)))
  return(dots)
}

#' convert list from getDots to vector of formatted shell flags
#'
#' @param dots output from getDots
#' @param flag_lookup optional named vector to convert names from dots to a
#'   simplified shell flag, useful for providing aliases to single-letter flags
#' @param prefix flag prefix, usually - or --
#'
#' @return named vector of shell flags with their values
#' @export
#'
#' @examples
#' theFunction <- function(...) { getDots(...) }
#' theDots <-  myFunction(example = "hello", example2 = "world", boolFlag = TRUE)
#' theArgs <-  dotsToArgs(theDots)
dotsToArgs <- function(dots, flag_lookup = NULL, prefix = "-"){

  testthat::expect_named(dots)

  if (!is.null(flag_lookup)) {
    names(dots)[names(dots) %in% names(flag_lookup)] <- flag_lookup[names(dots)[names(dots) %in% names(flag_lookup)]]
  }

  # collapse logicals, T = include, F = exclude
  dots <- purrr::map(dots, ~{
    if (!is.logical(.x)) return(.x)

    if (.x == T) return("")

    return(.x)
  })

  # only FALSE logicals remain, so they are dropped
  dots <- dots[!purrr::map_lgl(dots, is.logical)]

  # collapse to named vector
  named_dots <- purrr::map_chr(dots, as.character)

  # concatenate to vector of flag calls
  args <- purrr::map2_chr(names(named_dots), named_dots, ~{paste0(prefix, .x, " ", .y)})

  return(args)
}



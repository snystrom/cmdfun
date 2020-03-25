# TODO:
# - Switch purrr calls to base R?
# - write tests
# - add function for constructing full call for passing to system2
# - ERROR CHECK that arg & user-friendly name aren't both used
#   - Warn user, default behavior to use raw flag over user-friendly name
# - handle BOOL flags correctly (UNTESTED)
# - handle vector arguments

getDots <- function(...){
  dots <- eval(substitute(list(...)))
  return(dots)
}

dotsToArgs <- function(dots, flag_lookup = NULL, prefix = "-"){
  # prefix = flag prefix, usually - or --
  # dots = named list parsed from getDots
  # flag_lookup = named vector where names are custom name and value is actual flag,
       # useful for parsing single-letter flags with no expanded names
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



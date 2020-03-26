
#' Removes logical vectors from list objects
#'
#' @param list any list object
#'
#' @return the same list without any entries with logical vectors
#'
#' @examples
drop_list_logicals <- function(list){
  list[!purrr::map_lgl(list, is.logical)]
}


#' Replaces list entries of bool of specified value with empty strings
#'
#' @param list 
#' @param bool whether to convert T or F to empty string (default T)
#'
#' @return same list with entries replaced 
#' 
#'
#' @examples
convert_logical_to_empty <- function(list, bool = T){
  list <- purrr::map(list, ~{
    if (!is.logical(.x)) return(.x)

    if (.x == bool) return("")

    return(.x)
  })
  
}

#' Replace list entries with boolean values of TRUE with empty string
#'
#' @param list 
#'
#' @return list where entries with TRUE are replaced for ""
#'
#' @examples
true_to_empty <- function(list){
  list <- convert_logical_to_empty(list, T)
  return(list)
}


#' converts names of object (obj) to values of named vector (dict)
#'
#' @param obj list or vector
#' @param dict named vector where names are converted to values
#'
#' @return
#'
#' @examples
convert_names <- function(obj, dict){
  testthat::expect_named(obj)
  testthat::expect_named(dict)
  
  names(obj)[names(obj) %in% names(dict)] <- dict[names(obj)[names(obj) %in% names(dict)]]
  return(obj)
}

#' count number of times dots contains match to long or short version of flag
#'
#' @param value value from argsDict entry
#' @param name name of argsDict entry
#' @param dots list of dots from getDots()
#'
#' @return count of matches to value or name (should be 1 or greater if set, 0 if not)
#'
#' @examples
count_matched_args <- function(value, name, dots){
  names(dots) %in% c(value, name) %>% sum
}

#' returns list of named args with multiple definitions
#'
#' @param vec named vector output of count_matched_args
#'
#' @return names of args with matches > 1
#'
#' @examples
find_multimatched_args <- function(vec){
  testthat::expect_named(vec)
  names(vec[vec > 1])
}

#' Prints message warning user of each argument that is defined multiple times in function call
#'
#' @param name name of arg
#'
#' @return
#'
#' @examples
warn_multimatched_arg <- function(name){
  message(paste0(name, " is set multiple times in function call, ensure this is correct behavior."))
}

#' Combines key/value pairs from named vector by separator
#'
#' @param dict argsDict
#' @param sep separator
#'
#' @return
#'
#' @examples
concatenate_args <- function(dict, sep = "/"){
  paste(names(dict), dict, sep = sep)
}




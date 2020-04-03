#' Keep items by name from list
#'
#' @param list an R list
#' @param names vector of names to keep
#'
#' @return
#' 
#' @export
#'
#' @examples
#' keep_list_by_name(list("a" = 1, "b" = 2), "a")
keep_list_by_name <- function(list, names){
  list[(names(list) %in% names)]
}

#' Drop items by name from list
#'
#' @param list an R list
#' @param names vector of names to drop
#'
#' @return
#'
#' @export
#' 
#' @examples
#' drop_list_by_name(list("a" = 1, "b" = 2), "a")
drop_list_by_name <- function(list, names){
  list[!(names(list) %in% names)]
}

#' Drops list members by function
#'
#' @param list any list object
#' @param fun a function that evaluates to boolean value, used to filter members of list
#'
#' @return the same list without any entries with logical vectors
#'
#' @examples
#' myList <- list(a = 1, b = TRUE, c = FALSE)
#' \dontrun{
#' # This will drop logicals from list
#' drop_list_fun(myList, fun = is.logical)
#' }
drop_list_fun <- function(list, fun){
  testthat::expect_equal(class(fun), "function")
  list[!purrr::map_lgl(list, fun)]
}

#' Removes logical vectors from list objects
#'
#' @param list any list object
#'
#' @return the same list without any entries with logical vectors
#'
#' @examples
#' myList <- list(a = 1, b = TRUE, c = FALSE)
#' \dontrun{
#' drop_list_logicals(myList)
#' }
drop_list_logicals <- function(list){
  drop_list_fun(list, is.logical)
}

#' Removes logical vectors from list objects
#'
#' @param list any list object
#'
#' @return the same list without any entries with logical vectors
#'
#' @examples
#' myList <- list(a = 1, b = TRUE, c = FALSE, d = NULL)
#' \dontrun{
#' drop_list_NULL(myList)
#' }
drop_list_NULL <- function(list){
  drop_list_fun(list, is.null)
}


#' Replaces list entries of bool of specified value with empty strings
#'
#' @param list a list
#' @param bool whether to convert T or F to empty string (default T)
#'
#' @return same list with entries replaced 
#' 
#'
#' @examples
#' myList <- list(a = TRUE, b = FALSE)
#' \dontrun{
#' convert_logical_to_empty(myList)
#' }
convert_logical_to_empty <- function(list, bool = T){
  list <- purrr::map(list, ~{
    if (!is.logical(.x)) return(.x)

    if (.x == bool) return("")

    return(.x)
  })
  
}

#' Replace list entries with boolean values of TRUE with empty string
#'
#' @param list a list
#'
#' @return list where entries with TRUE are replaced for ""
#'
#' @examples
#' myList <- list(a = TRUE)
#' \dontrun{
#' true_to_empty(myList)
#' }
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
#' dict <- c("long" = "l")
#' dots <- list("long" = 1, "a" = 1)
#' \dontrun{
#' convert_names(dots, dict)
#' }
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
#' @importFrom magrittr %>%
#' 
#' @examples
#' dict <- c("name" = "value")
#' dots <- list("name" = 1, "value" = 1)
#' \dontrun{
#' count_matched_args(dict[1], names(dict)[1], dots)
#' }
#'
#' dict <- c("name" = "value")
#' dots <- list("name" = 1)
#' \dontrun{
#' count_matched_args(dict[1], names(dict)[1], dots)
#' }
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
#' vec <- c("setArg" = 1, "unsetArg" = 0, "multiSetArg" = 3)
#' \dontrun{
#' find_multimatched_args(vec)
#' }
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
#' \dontrun{
#' warn_multimatched_arg("arg")
#' }
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
#' dict <- c("name" = "val")
#' \dontrun{
#' concatenate_args(dict)
#' }
concatenate_args <- function(dict, sep = "/"){
  paste(names(dict), dict, sep = sep)
}


#' Searches for illegal values in string
#'
#' @param flag string
#' @param illegal_chars vector of flags forbidden in string
#'     default values are: "&", "|", ";", "(", ")", "{", "}", "$", "\@", "/"
#' @return Boolean value for each flag
#'
#' @examples
#' \dontrun{
#' flag_is_illegal("&&echo")
#' }
flag_is_illegal <- function(flag, 
                                  illegal_chars = c("&", "\\|", ";", "\\(", "\\)", "\\{", "\\}", "\\$", "\\@", "\\/", " ")){
  any(purrr::map_lgl(illegal_chars, grepl, flag))
}

#' Prints illegal flag warning for name
#'
#' @param name name containing illegal flag
#'
#' @return
#'
#' @examples
#' \dontrun{
#' error_illegal_flag("&&echo")
#' }
error_illegal_flag <- function(name){
  stop(paste0(name, " is not a valid flag name. Contains illegal character."))
}

#' Checks dots for illegal flag names
#'
#' @param dots list output of getDots
#'
#' @return prints warning for each illegal flag
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' theFunction <- function(...) { getDotArgs() }
#' theDots <-  theFunction(validFlag = TRUE, `&illegalFlag` = "will error")
#' \dontrun{
#' theArgs <-  dotsToArgs(theDots)
#' }
check_args_contain_illegal_flags <- function(dots){
  purrr::map_lgl(names(dots), flag_is_illegal) %>% 
    purrr::set_names(names(dots)) %>% 
    {.[.==T]} %>% 
    {purrr::walk(names(.), error_illegal_flag)}
}

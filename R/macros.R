#' Macro for constructing target path validators
#' 
#' A common pattern in designing shell interfaces is to ask the user to give an
#' absolute path to the target shell utility. It is common to pass this
#' information from the user to R by using either R environment variables
#' defined in .Renviron, using options (set with option(), and got with
#' getOption()), having the user explicitly pass the path in the function call,
#' or failing this, using a default install path.
#' 
#' Another common use-case involves software packages with many tools packaged
#' in a single directory, and the user may want to call one or many utilities
#' within this common structure.
#' 
#' For example, the software "coolpackage" is installed in "~/coolpackage", and
#' has two programs: "tool1", and "tool2" found in "~/coolpackage/tool1" and 
#' ~/coolpackage/tool2", respectively. 
#' 
#' To design an interface to coolpackage, this function can automate checking
#' and validation for not only the package, but for each desired utility in the
#' package.
#' 
#' The heirarchy of path usage is: user-defined > option_name > environment_var > default_path
#' 
#' 
#'
#' @param environment_var name of R environment variable defining target path. Can be set in .Renviron.
#' @param option_name name of user-configurable option (called by getOption) which will hold path to target
#' @param default_path default install path of target. Can contain shell
#'   specials like "~" which will be expanded at runtime (as opposed to build time of the handler).
#' @param utils optional character vector containing names of valid utils inside
#'   target path, used to populate error checking for valid install. 
#' 
#' @return function that returns a valid path to tool or optional utility.
#' 
#' The returned path_handler function takes as input a path or util. where path
#' is a user override path for the supported tool. If the user-defined path is
#' invalid, this will always throw an error and not search the defined defaults.
#' 
#' util must be found within the target path, but does not have to be present in
#' the original "utils" call. The user will be warned if this is the case. If
#' `util` is set to `TRUE` will return all paths to utilities without checking
#' the install. This can be used for writing user-facing install checkers.
#' 
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' cool_checker <- build_path_handler(default_path = "~/coolpackage", utils = c("tool1", "tool2"))
#' # returns path to coolpackage
#' cool_checker()
#' # returns path to coolpackage/tool1
#' cool_checker(util = "tool1")
#' 
#' }
build_path_handler <- function(environment_var = NULL, option_name = NULL, default_path = NULL, utils = NULL){
  
  if (is.null(environment_var) & is.null(option_name) & is.null(default_path)){
    warning("at least one of: environment_var, option_name, default_path is not assigned, user must manually set path")
  }
    
  requiredArgs <- getAllArgs() %>% 
    drop_list_by_name("utils")
  
  purrr::map(requiredArgs, length) %>% 
    drop_list_fun(fun = function(x) x <= 1) %>% 
    names %>% 
    purrr::walk(~{
      stop(paste0(.x, " must contain only 1 value"))
    })
  
  # passing NULL to getOption returns NULL & throws error,
  # safely_ allows catching the NULL in .$result without throwing error
  safe_getOption <- purrr::safely(R.utils::getOption)
  
  return_valid_path <- function(path = NULL, util = NULL){
    # Try to use correct path if user doesn't set in call
    pathList <- list()
    
    if (!is.null(path)) {
      pathList$user <- check_valid_command_path(path) 
      
    } 
    
    if (!is.null(safe_getOption(option_name)$result)) {
      pathList$option <- valid_path_or_null(R.utils::getOption(option_name))
      
    } 
    
    if (!identical(Sys.getenv(environment_var), Sys.getenv()) &
        !identical(Sys.getenv(environment_var), "") &
        length(Sys.getenv(environment_var)) == 1) {
      
      pathList$environment <- valid_path_or_null(Sys.getenv(environment_var))
    } 
    
    if (!is.null(default_path)) {
        pathList$default <- default_path
    }
    
    if (length(pathList) == 0) stop("No path defined or detected")
    
    # use this vector to sort list of valid paths
    heirarchy <- c("user", "option", "environment", "default")
    
    validPathHeirarchy <- pathList %>% 
      unlist %>% 
      sort_vector(heirarchy) %>% 
      as.list()
    
    # This check is to mostly to evaluate default_path at runtime 
    # if all other options fail. This is so that default_path values 
    # like "~/path/to/file" won't expand at compile-time.
    fullPath <- check_valid_command_path(validPathHeirarchy[[1]])
    
    if (!is.null(util)) {
      if (is.null(utils)){
        stop("this function has no defined utils")
      }
      if (util == TRUE){
        return(file.path(fullPath, utils))
      }
      utilPath <- check_valid_util(util, utils, fullPath)
      return(utilPath)
    } else {
      return(fullPath)
    }
    
  }
  
  return(return_valid_path)
}

#' Checks for valid members of subdirectory
#' 
#' Not meant to be called directly
#'
#' @param util name of target located in path
#' @param utils name of supported targets in path
#' @param path path to directory
#'
#' @return safe path to util, or error if util does not exist
#' @export
#'
#' @examples
#' \dontrun{
#' # this will return /full/path/to/meme/bin/dreme
#' # or return an error for all values of util that are not "dreme" and "ame"
#' # or error if "dreme" does not exist in "~/meme/bin/"
#' check_valid_util("dreme", utils = c("dreme", "ame"), "~/meme/bin")
#' 
#' # This will throw error
#' check_valid_util("badUtil", utils = c("dreme", "ame"), "~/meme/bin")
#' }
check_valid_util <- function(util, utils = NULL, path = NULL){
  testthat::expect_length(util, 1)
  # check util is valid
  needs_util_warning = !util %in% utils
  
  # check util exists
  util_path <- file.path(sanitize_path(path), util)
  
  util_exists <- file.exists(util_path)
  
  if (!(needs_util_warning) & util_exists){
    return(util_path)
  }
  
  if (!(needs_util_warning) & !(util_exists)){
    stop(paste0(util_path, " is an invalid path to supported util: ", util, ". Check that ", util, " is installed."))
  }
  
  if ((needs_util_warning) & !(util_exists)){
    stop(paste0(util_path, " is an invalid path to an unsupported util: ", util))
  }
  
  if (needs_util_warning & util_exists){
      warning("the util: ", path, ", exists but is not supported by package maintainer or in `utils` definition")
      return(util_path)
  }
  
}

#' Checks path is valid
#'
#' Not meant to be called directly
#' 
#' @param path path to file or directory
#'
#' @return expanded system path
#' @export
#'
#' @examples
#' \dontrun{
#' # will return /full/path/to/meme/bin, or error if path doesn't exist
#' check_valid_command_path("~/meme/bin")
#' }
check_valid_command_path <- function(path){
  path <- sanitize_path(path)
  command_exists <- file.exists(path)
  
  if (!command_exists){
    stop(paste0("Command: ", path, ", does not exist."))
  }
  
  return(path)
  
}

#' Check if path exists
#'
#' @param path file path
#'
#' @return boolean
#'
#' @noRd
is_valid_path <- function(path){
  path <- sanitize_path(path)
  is_valid <- file.exists(path)
  return(is_valid)
}

#' Checks if path exists
#'
#' @param path a path
#'
#' @return sanitized file path if it exists, otherwise return NULL
#'
#' @noRd
valid_path_or_null <- function(path){
  if (is_valid_path(path)){
    return(sanitize_path(path))
  } else {
    return(NULL)
  }
}

#' Sort a named vector using custom name order
#'
#' @param vector named vector
#' @param names order to arrange names in vector
#'
#' @return sorted vector in order of names
#'
#' @noRd
sort_vector <- function(vector, names){
  vector[order(factor(names(vector), levels = names))]
}
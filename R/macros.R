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
#' The hierarchy of path usage is: user-defined > option_name > environment_var > default_path
#' 
#' 
#'
#' @param environment_var name of R environment variable defining target path. Can be set in .Renviron.
#' @param option_name name of user-configurable option (called by getOption) which will hold path to target
#' @param default_path default install path of target. Can contain shell
#'   specials like "~" which will be expanded at runtime (as opposed to build time of the search function).
#' @param utils optional character vector containing names of valid utils inside
#'   target path, used to populate error checking for valid install. 
#' 
#' @return function that returns a valid path to tool or optional utility.
#' 
#' The returned path_search function takes as input a path or util. where path
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
#' if (.Platform$OS.type == "unix") {
#' bin_checker <- cmd_path_search(default_path = "/bin", utils = c("ls", "pwd"))
#' # returns path to bin
#' bin_checker()
#' # returns path to bin/ls
#' bin_checker(util = "ls")
#' }
cmd_path_search <- function(environment_var = NULL, option_name = NULL, default_path = NULL, utils = NULL){
  
  if (is.null(environment_var) & is.null(option_name) & is.null(default_path)){
    warning("at least one of: environment_var, option_name, default_path is not assigned, user must manually set path")
  }
  
  # The following strategy fails because of lazy evalutation of function args
  # everything will eval to length 1 even if length > 1 because they are assigned to:
  #
  # function (x, value, pos = -1, envir = as.environment(pos), inherits = FALSE,  immediate = TRUE) 
  # .Internal(assign(x, value, envir, inherits))
  # 
  # Instead of being directly evaluated when building the macro,
  # this results (for a reason I haven't figured out yet) in only returning the
  # **first** object if something is given multiple assignment (ie a vector of
  # length > 1)
  # This is why the check "succeeds" in certain situations, because it silently avoids the length check
  # SO DON'T USE cmd_args_* WHEN BUILDING CHECKS (OR MACROS??)
  
  #requiredArgs <- cmd_args_all() %>% 
  #  cmd_list_drop_named("utils")
  
  requiredArgs <- list("environment_var" = environment_var,
                       "option_name" = option_name,
                       "default_path" = default_path)
  
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
      pathList$user <- .check_valid_command_path(path) 
      
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
    hierarchy <- c("user", "option", "environment", "default")
    
    validPathHierarchy <- pathList %>% 
      unlist %>% 
      sort_vector(hierarchy) %>% 
      as.list()
    
    # This check is to mostly to evaluate default_path at runtime 
    # if all other options fail. This is so that default_path values 
    # like "~/path/to/file" won't expand at compile-time.
    fullPath <- .check_valid_command_path(validPathHierarchy[[1]])
    
    if (!is.null(util)) {
      
      if (length(util) > 1){
        stop("util must be NULL or length 1")
      }
      
      if (is.null(utils)){
        stop("this function has no defined utils")
      }
      if (util == TRUE){
        return(file.path(fullPath, utils))
      }
      utilPath <- .check_valid_util(util, utils, fullPath)
      return(utilPath)
    } else {
      return(fullPath)
    }
    
  }
  
  return(return_valid_path)
}

#' Macro for constructing boolean check for valid path
#' 
#' @param path_search function output of `cmd_path_search()` **NOTE:** When
#'   passing the function, do not pass as: `fun()`, but `fun` to avoid evaluation.
#' @param util value to pass to `util` argument of `path_search`, allows
#'   building individual functions for each util (if passing one of each),
#'   or for simultaneously checking all utils if setting `util = TRUE`. Will
#'   cause error if `util = TRUE` but no utils are defined. **NOTE:** There is
#'   no error checking for whether `util` is set correctly during the build
#'   process, so ensure correct spelling, etc. to avoid cryptic failures.
#'
#' @return a function returning TRUE or FALSE if a valid install is detected.
#'   With arguments: `path` (a path to install location), `util` an optional `character(1)` to 
#' 
#' @export
#'
#' @examples
#' if (.Platform$OS.type == "unix") {
#' search <- cmd_path_search(option_name = "bin_path", default_path = "/bin/")
#' valid_install <- cmd_install_is_valid(search)
#' # Returns TRUE if "/bin/" exists
#' valid_install()
#' # Returns FALSE if "bad/path/" doesn't exist
#' valid_install("bad/path/")
#' 
#' # Also works with options
#' search_option_only <- cmd_path_search(option_name = "bin_path")
#' valid_install2 <- cmd_install_is_valid(search_option_only)
#' options(bin_path = "/bin/")
#' valid_install2()
#'
#' # Setting util = TRUE will check that all utils are also installed
#' search_with_utils <- cmd_path_search(default_path = "/bin", utils = c("ls", "pwd"))
#' valid_install_all <- cmd_install_is_valid(search_with_utils, util = TRUE)
#' valid_install_all()
#' }
cmd_install_is_valid <- function(path_search, util = NULL){
  
  util_check <- !is.null(util)
  util_true <- FALSE
  
  # This looks like bad code, but it's necessary since util can be `NULL`,
  # `logical(1)`, or a `character(1)`, so can't just use the value of util here.
  if (util_check){
    if (util == TRUE){
      util_true <- TRUE
    }
  }
  
  is_valid <- function(path = NULL){
    x <- tryCatch(path_search(path = path, util = util),
             error = function(e) return(FALSE))
    
    if (is.character(x) & !util_true){
      return(TRUE)
    } 
    
    if (util_true){
      # util = TRUE will list all utils without checking
      # so have to check here
      return(all(file.exists(x)))
    }
    
    if (!x){
      return(FALSE)
    }
  }
  
  return(is_valid)
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
#' if (.Platform$OS.type == "unix") {
#' # this will return /full/path/to/bin
#' # or return an error for all values of util that are not "ls" and "pwd"
#' # or error if "ls" does not exist in "/bin"
#' .check_valid_util("ls", utils = c("ls", "pwd"), "/bin")
#' 
#' \dontrun{
#' # This will throw error
#' .check_valid_util("badUtil", utils = c("ls", "pwd"), "/bin")
#' }
#' }
.check_valid_util <- function(util, utils = NULL, path = NULL){
  testthat::expect_length(util, 1)
  # check util is valid
  needs_util_warning <- !util %in% utils
  
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
#' if (.Platform$OS.type == "unix" & file.exists("~/bin")) {
#' # will return /full/path/to/home/bin, or error if path doesn't exist
#' .check_valid_command_path("~/bin")
#' }
.check_valid_command_path <- function(path){
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

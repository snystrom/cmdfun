#' Keep items by name from list
#'
#' @param list an R list
#' @param names vector of names to keep
#'
#' @return list keeping only items defined by names
#' 
#' @noRd
#'
#' @examples
#' cmd_list_keep_named(list("a" = 1, "b" = 2), "a")
cmd_list_keep_named <- function(list, names){
  list[(names(list) %in% names)]
}

#' Drop items by name from list
#'
#' @param list an R list
#' @param names vector of names to drop
#'
#' @return list removing items defined by names
#'
#' @noRd
#' 
#' @examples
#' cmd_list_drop_named(list("a" = 1, "b" = 2), "a")
cmd_list_drop_named <- function(list, names){
  list[!(names(list) %in% names)]
}

#' keep entries from list of flags by name or name/value pair.
#'
#' @param flags named list output of cmd_args_to_flags
#' @param keep vector of flag entries to keep. Pass an unnamed vector
#'   to keep flags by name. Pass a named vector to keep flags by name/value
#'   pairs. Pass a numeric vector to keep by position.
#'
#' @return flags list with values not in keep removed
#' @export
#'
#' @examples
#' exFlags <- list("flag1" = 2, "flag2" = "someText")
#' cmd_list_keep(exFlags, "flag1")
#' # will keep flag2 because its name and value match 'keep' vector
#' cmd_list_keep(exFlags, c("flag2" = "someText"))
#' # Will keep "flag1" by position index
#' cmd_list_keep(exFlags, 1)
#'
#' # won't keep flag2 because its value isn't 'someText'
#' exFlags2 <- list("flag1" = 2, "flag2" = "otherText")
#' cmd_list_keep(exFlags, c("flag2" = "someText"))
cmd_list_keep <- function(flags, keep){
  
  testthat::expect_named(flags)
  
  if (length(keep) == 0){
    return(list())
  }
  
  if (is.numeric(keep)){
    return(list_keep_index(flags, keep))
  }
  
  if (is.null(names(keep))){
    keeps <- list_index_names(flags, keep)
    return(cmd_list_keep(flags, keeps))
  }

  if (!is.null(names(keep))) {
    keeps <- list_index_named_values(flags, keep)
    return(list_keep_index(flags, keeps))
  }
  
}

#' Drop entries from list of flags by name or name/value pair.
#'
#' @param flags named list output of cmd_args_to_flags
#' @param drop vector of flag entries to drop. Pass an unnamed vector
#'   to drop flags by name. Pass a named vector to drop flags by name/value
#'   pairs. Pass a numeric vector to drop by position.
#'
#' @return flags list with values in drop removed
#' @export
#'
#' @examples
#' exFlags <- list("flag1" = 2, "flag2" = "someText")
#' cmd_list_drop(exFlags, "flag1")
#' # will drop flag2 because its name and value match 'drop' vector
#' cmd_list_drop(exFlags, c("flag2" = "someText"))
#' # Will drop "flag1" by position index
#' cmd_list_keep(exFlags, 1)
#'
#' # won't drop flag2 because its value isn't 'someText'
#' exFlags2 <- list("flag1" = 2, "flag2" = "otherText")
#' cmd_list_drop(exFlags, c("flag2" = "someText"))
cmd_list_drop <- function(flags, drop){
  
  testthat::expect_named(flags)
  
  if (length(drop) == 0){
    return(flags)
  }
  
  if (is.numeric(drop)){
    return(list_drop_index(flags, drop))
  }
  
  if (is.null(names(drop))){
    drops <- list_index_names(flags, drop)
    return(cmd_list_drop(flags, drops))
  }

  if (!is.null(names(drop))) {
    drops <- list_index_named_values(flags, drop)
    return(list_drop_index(flags, drops))
  }
  
}

#' keeps list entry by positional index
#'
#' @param flags list of flags
#' @param index position in list to keep
#'
#' @return flags kept at indices
#'
#' @examples
#' 
#' @noRd
list_keep_index <- function(flags, index){
  
  if (length(index) == 0 | all(is.na(index))){ return(list()) }
  
  index <- index[!is.na(index)]
  
  flags[index]
}

#' drops list entry by positional index
#'
#' @param list a `list`
#' @param index position in list to drop
#'
#' @return list w/ values dropped at indices
#'
#' @examples
#' 
#' @noRd
list_drop_index <- function(list, index){
  
  if (length(index) == 0 | all(is.na(index))){ return(list) }
  
  index <- index[!is.na(index)]
  
  list[-index]
}

#' Return index of named list values
#'
#' @param list a named list
#' @param names names of list entries
#'
#' @return vector of indices corresponding to named values matching names
#'
#' @examples
#' 
#' @noRd
list_index_names <- function(list, names){
  i <- which(names(list) %in% names)
  names(i) <- NULL
  
  if (length(i) == 0) { return(NULL) }
  
  return(i)
}

#' Return index for list entry matching name & value
#' 
#' 
#'
#' @param list a named list
#' @param named_values named character vector of list objects + their values to be matched.
#'
#' @return index in flags list with entries matching name/value pairs in named_values
#'
#' @examples
#' myList <- list(a = 1, b = 2, b = 5)
#' list_index_named_values(myList, c("b" = 5))
#' list_index_named_values(myList, c("b" = 2, "a" = 1))
#' 
#' @noRd
list_index_named_values <- function(list, named_values){
  
  testthat::expect_named(named_values)
  
  if (any(names(named_values) == "")){
    warnVals <- named_values[names(named_values) == ""]
    warning(paste0("The following values have no names associated with them and will be ignored: ", warnVals))
  }
  
  indices <- purrr::imap_int(named_values, ~{
    i <- which(names(list) == .y & list == .x)
    names(i) <- NULL
    
    # NA will mean "no match"
    if (length(i) == 0){return(NA)}
    # ignore names of "" to avoid unexpected behavior
    if (.y == ""){return(NA)}
    
    return(i)
  }) %>% 
    purrr::set_names(NULL)
  return(indices)
}


#' Check that file exits, error if not
#'
#' @param files list or vector of paths to check
#'
#' @return nothing or error message
#' @export
#'
#' @importFrom magrittr %>%
#' 
#' @examples
#' cmd_files_exist(tempdir())
#' \dontrun{
#' # Throws error if file doesn't exist
#' cmd_files_exist(file.path(tempdir(), "notreal"))
#' }
cmd_files_exist <- function(files){
  files %>%
    purrr::map(purrr::discard, file.exists) %>%
    purrr::compact() %>%
    purrr::walk(error_file_not_exist)
}

#' Throw error that file doesn't exist
#'
#' @param file path to file
#'
#' @return file doesn't exist error
#'
#' @examples
#' 
#' @noRd
error_file_not_exist <- function(file){
  stop(paste0(file, " was not found."))
}

#' Generates list of expected output files
#'
#' See documentation of cmd_output_check() for more details about how this works
#'
#' @param ext file extension (no ".", ie "txt", "html")
#' @param prefix file name to be given each ext. If a character vector, must be equal length of ext or shorter
#' @param outdir optional directory where files should exist
#'
#' @return list of file paths by each ext or prefix (whichever is longer)
#' @export
#'
#' @examples
#' # Makes list for many file types of same prefix
#' # ie myFile.txt, myFile.html, myFile.xml
#' cmd_output_expect(c("txt", "html", "xml"), "myFile")
#' 
#' # Makes list for many files of same type
#' # ie myFile1.txt, myFile2.txt, myFile3.txt
#' cmd_output_expect("txt", c("myFile1", "myFile2", "myFile3"))
#'
cmd_output_expect <- function(ext, prefix, outdir = "."){
  files <- purrr::map2(ext, prefix, ~{
    file.path(outdir, paste0(.y, ".", .x)) %>% 
      sanitize_path()
  })

  if (length(ext) < length(prefix)){
    files %>%
      purrr::set_names(prefix) %>%
      return()
  } else {
    files %>%
      purrr::set_names(ext) %>%
      return()
  }

}

#' Creates list of paths by file extension & checks they exist
#'
#' Ext or prefix can be a vector or single character. The shorter value will be
#' propagated across all values of the other. See Examples for details.
#'
#' @param ext vector of file extensions
#' @param prefix name of file prefix for each extension.
#' @param outdir directory the files will be inside
#'
#' @return vector of valid file paths
#' @export
#'
#' @importFrom magrittr %T>%
#'
#' @examples
#' \dontrun{
#' # Checks many file types of same prefix
#' # ie myFile.txt, myFile.html, myFile.xml
#' cmd_output_check(c("txt", "html", "xml"), "myFile")
#' # Checks many files of same type
#' # ie myFile1.txt, myFile2.txt, myFile3.txt
#' cmd_output_check("txt", c("myFile1", "myFile2", "myFile3"))
#' }
#'
#' 
cmd_output_check <- function(ext, prefix, outdir = "."){
  cmd_output_expect(ext, prefix, outdir) %T>%
    cmd_files_exist()
}


#' Checks if file exists, returns pretty status message
#'
#' @param file path to file
#'
#' @return ui_done or ui_oops printed to terminal.
#' @export
#'
#' @examples
#' cmd_ui_file_exists("/path/to/file.txt")
cmd_ui_file_exists <- function(file){
  if (file.exists(file)) {
    usethis::ui_done(file)
  } else {
    usethis::ui_oops(file)
  }
  return(invisible(NULL))
}


#' Wrapper function for checking an install
#' 
#' This function can be lightly wrapped by package builders to build a user-friendly install checking function.
#'
#' @param path_handler `function` output of `cmd_path_handle()`
#' @param path user-override path to check (identical to `path` argument of `cmd_path_handle()` output)
#'
#' @return pretty printed message indicating whether files exits or not. Green check = Yes, red X = No.
#' @export
#'
#' @examples
#' \dontrun{
#' path_handler <- cmd_path_handle(default = "/bin", utils = "ls")
#' cmd_install_check(path_handler)
#' }
cmd_install_check <- function(path_handler, path = NULL){
  if (!is.function(path_handler)) {
    stop("path_handler must be a function")
  }
  message("checking main install")
  
  x <- try(path_handler(path = path) %>% cmd_ui_file_exists(), silent = TRUE)
  
  
  if (class(x) == "try-error") {
    cmd_ui_file_exists(path)
    return(invisible(NULL))
    }
  
  
  message("checking util installs")
  path_handler(path = path, util = TRUE) %>% 
    purrr::walk(cmd_ui_file_exists)
  
  return(invisible(NULL))
}

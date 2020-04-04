build_valid_path <- function(path = NULL, utils = NULL){
  basePath <- file.path(tempdir(), path)
  dir.create(basePath)
  
  if (!is.null(utils)){
    utilPaths <- file.path(basePath, utils)
    file.create(utilPaths)
    
    names(utilPaths) <- utils
    
  }
  
  allPaths <- c("base" = basePath, utilPaths)
  return(allPaths)
  
}

cleanup_valid_path <- function(allPaths){
  stopifnot("base" %in% names(allPaths))
  unlink(allPaths["base"], recursive = T)
}

myUtils <- c("tool1", "tool2")
check_paths <- build_valid_path("tests", myUtils)
base_path <- check_paths["base"]
names(base_path) <- NULL
double_assign <- c("one", "two")

test_option <- "dotargs_test_option"
test_env_var <- "DOTARGS_TEST"

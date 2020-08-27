
build_valid_path <- function(path = NULL, utils = NULL){
  basePath <- sanitize_path(file.path(tempdir(), path))
  dir.create(basePath, showWarnings = F)
  
  utilPaths <- utils
  
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


allUtils <- c("tool1", "tool2", "tool3")
supportedUtils <- c("tool1", "tool2")
unsupportedUtils <- "tool3"
myUtils <- supportedUtils

check_paths <- build_valid_path("tests", allUtils)
base_path <- check_paths["base"]
names(base_path) <- NULL

double_assign <- c("one", "two")
test_option <- "dotargs_test_option"
test_env_var <- "DOTARGS_TEST"

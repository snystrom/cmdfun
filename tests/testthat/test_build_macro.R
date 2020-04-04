library(dotargs)
meme_path <- "~/meme/bin"
myUtils <- c("dreme", "ame", "tomtom")

check_meme <- build_util_checker(environment_var = NULL, 
                                 option_name = NULL, 
                                 default_path = meme_path)

test_that("Default path only, noUtils works",{
# TODO: do i need to call internal function in test or are those globally scoped for testthat?
  expect_equal(check_meme(meme_path), dotargs:::sanitize_path(meme_path))
  expect_error(check_meme(meme_path, util = "dreme"), "no defined utils")
})


check_meme <- build_util_checker(environment_var = NULL, 
                                 option_name = NULL, 
                                 default_path = meme_path,
                                 utils = myUtils)

test_that("Defining & checking utils works", {
  expect_equal(check_meme(), dotargs:::sanitize_path(meme_path))
  expect_equal(check_meme(meme_path, util = "dreme"), dotargs:::sanitize_path(file.path(meme_path, "dreme")))
})

test_that("Options definition works",{
  check_meme <- build_util_checker(environment_var = NULL, 
                             option_name = "mp", 
                             default_path = "bad/path",
                             utils = myUtils)
  
  expect_equal(check_meme(meme_path), dotargs:::sanitize_path(meme_path))
  expect_equal(check_meme(meme_path, util = "dreme"), dotargs:::sanitize_path(file.path(meme_path, "dreme")))
  
  options("mp" = meme_path)
  
  expect_equal(check_meme(meme_path), dotargs:::sanitize_path(meme_path))
  expect_equal(check_meme(meme_path, util = "dreme"), dotargs:::sanitize_path(file.path(meme_path, "dreme")))
})
  check_meme <- build_util_checker(environment_var = NULL, 
                             option_name = "mp", 
                             default_path = "bad/path",
                             utils = myUtils)
  
  check_meme()
  expect_equal(check_meme(meme_path), dotargs:::sanitize_path(meme_path))
  expect_equal(check_meme(meme_path, util = "dreme"), dotargs:::sanitize_path(file.path(meme_path, "dreme")))
  
  options("mp" = meme_path)
  options("mpp" = meme_path)
  
  expect_equal(check_meme(meme_path), dotargs:::sanitize_path(meme_path))
  expect_equal(check_meme(meme_path, util = "dreme"), dotargs:::sanitize_path(file.path(meme_path, "dreme")))
  
check_meme(meme_path)
check_meme(meme_path, util = "dreme")

Sys.setenv("MEME_PATH" = meme_path)
check_meme_utils <- build_util_checker(environment_var = "MEME_PATH", 
                                 option_name = "badoption", 
                                 default_path = "bad/path",
                                 utils = myUtils)
check_meme_utils(meme_path)
check_meme_utils(meme_path, util = "dreme")

check_meme_utils <- build_util_checker(environment_var = "MEME_PATH", 
                                 option_name = "mp", 
                                 default_path = "bad/path",
                                 utils = myUtils)
check_meme_utils(meme_path)
check_meme_utils(meme_path, util = "dreme")



full_meme_path <- check_valid_command_path(meme_path)
check_valid_util("dreme", myUtils, full_meme_path)

# util check accepts only 1 util
expect_error(check_valid_util(c("dreme", "ame"), myUtils, full_meme_path))
# generated function check errors same as above
expect_error(check_meme_utils(meme_path, util = c("dreme", "ame")))
                 
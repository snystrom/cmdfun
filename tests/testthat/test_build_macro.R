
test_that("warn at least 1 var not assigned", {
  expect_warning(build_util_checker(), "at least one")
  expect_warning(build_util_checker(environment_var = NULL), "at least one")
  expect_warning(build_util_checker(option_name = NULL), "at least one")
  expect_warning(build_util_checker(default_path = NULL), "at least one")
})

test_that("At least 1 path is defined at calltime", {
  expect_warning(build_check <- build_util_checker())
  
  expect_error(build_check(), "No path defined or detected")
})

test_that("Catches double assignment",{
  
  expect_error(
    build_util_checker(environment_var = double_assign),
    "environment_var must contain"
  )
  
  expect_error(
    build_util_checker(option_name = double_assign),
    "option_name must contain"
  )
  
  expect_error(
    build_util_checker(default_path = double_assign),
    "default_path must contain"
  )
  
  # utils can have many values
  expect_success(
    build_util_checker(default_path = base_path, utils = double_assign)
  )
})

suppressWarnings(
check_build <- build_util_checker(environment_var = NULL, 
                                 option_name = NULL, 
                                 default_path = NULL)
)

test_that("Default path only, noUtils works",{
# TODO: will this error R CMD CHECK?
  expect_equal(check_build(base_path), dotargs:::sanitize_path(base_path))
  expect_error(check_build(base_path, util = myUtils[1]), "no defined utils")
})


check_build <- build_util_checker(environment_var = NULL, 
                                 option_name = NULL, 
                                 default_path = base_path,
                                 utils = myUtils)

test_that("Defining & checking utils works", {
  expect_equal(check_build(), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  # Expect errror with many utils
  expect_error(check_build(base_path, util = myUtils), class = "expectation_failure")
})

test_that("Options definition works",{
  check_build <- build_util_checker(environment_var = NULL, 
                             option_name = test_option, 
                             default_path = "bad/path",
                             utils = myUtils)
  
  expect_error(check_build(), "bad/path, does not exist")
  
  R.utils::setOption(test_option, base_path)
  
  expect_equal(check_build(), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  
  R.utils::setOption(test_option, NULL)
})

test_that("Environment definition works", {
  check_build <- build_util_checker(environment_var = test_env_var, 
                             utils = myUtils)
  expect_error(check_build(), "No path defined or detected")
  
  Sys.setenv(test_env_var = base_path)
  check_build <- build_util_checker(environment_var = test_env_var, 
                                   utils = myUtils)
  
  expect_equal(check_build(base_path), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  
  Sys.setenv(test_env_var = "")
})

# Cleanup temp dir & files
cleanup_valid_path(check_paths)

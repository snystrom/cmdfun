skip_if(T, message = "Needs to be Manually Run until I fix environment issues")

test_that("warn at least 1 var not assigned", {
  expect_warning(build_path_handler(), "at least one")
  expect_warning(build_path_handler(environment_var = NULL), "at least one")
  expect_warning(build_path_handler(option_name = NULL), "at least one")
  expect_warning(build_path_handler(default_path = NULL), "at least one")
})

test_that("At least 1 path is defined at calltime", {
  expect_warning(build_check <- build_path_handler())
  
  expect_error(build_check(), "No path defined or detected")
})

test_that("Catches double assignment",{
  
  expect_error(
    build_path_handler(environment_var = c("one", "two")),
    "environment_var must contain"
  )
  
  expect_error(
    build_path_handler(option_name = double_assign),
    "option_name must contain"
  )
  
  expect_error(
    build_path_handler(default_path = double_assign),
    "default_path must contain"
  )
  
  # utils can have many values
  expect_success(
    build_path_handler(default_path = tempdir(), utils = double_assign)
  )
})


test_that("Default path only, noUtils works",{
  expect_warning(
    check_build <- build_path_handler(environment_var = NULL, 
                                     option_name = NULL, 
                                     default_path = NULL)
  )
# TODO: will this error R CMD CHECK?
  expect_equal(check_build(base_path), dotargs:::sanitize_path(base_path))
  expect_error(check_build(base_path, util = myUtils[1]), "no defined utils")
})

test_that("Passing invalid user path throws error", {
  check_build <- build_path_handler(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  expect_equal(check_build(), dotargs:::sanitize_path(base_path))
  expect_error(check_build("bad/path"), "does not exist")
  
})

test_that("Defining & checking utils works", {
  check_build <- build_path_handler(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  expect_equal(check_build(), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  # Expect errror with many utils
  expect_error(check_build(base_path, util = myUtils), class = "expectation_failure")
})

test_that("Options definition works",{
  check_build <- build_path_handler(environment_var = NULL, 
                             option_name = test_option, 
                             default_path = "bad/path",
                             utils = myUtils)
  
  expect_error(check_build(), "bad/path, does not exist")
  
  R.utils::setOption(test_option, base_path)
  
  expect_equal(check_build(), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  
  # Test inheritance of default w/ bad option
  R.utils::setOption(test_option, "wrong_path")
  expect_error(check_build(), "bad/path, does not exist")
  
  R.utils::setOption(test_option, NULL)
})

test_that("Environment definition works", {
  check_build <- build_path_handler(environment_var = test_env_var, 
                             utils = myUtils)
  expect_error(check_build(), "No path defined or detected")
  
  Sys.setenv(test_env_var = base_path)
  check_build <- build_path_handler(environment_var = test_env_var, 
                                   utils = myUtils)
  
  expect_equal(check_build(base_path), dotargs:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  
  # Test inheritance of default w/ bad environment var
  Sys.setenv(test_env_var = "wrong_path")
  check_build <- build_path_handler(environment_var = test_env_var, 
                                    default_path = "bad/path")
  expect_error(check_build(), "bad/path, does not exist")
  
  Sys.setenv(test_env_var = "")
})

test_that("Util warnings work", {
  expect_equal(check_valid_util(util = myUtils[1], utils = myUtils, path = base_path), 
               dotargs:::sanitize_path(file.path(base_path, myUtils[1])))
  expect_error(check_valid_util(util = "badUtil", utils = myUtils, path = base_path), "invalid path to an unsupported")
  expect_warning(check_valid_util(util = allUtils[3], utils = myUtils, path = base_path), "exists but is not supported")
  # error when tool supported but not exist
  expect_error(check_valid_util(util = "tool4", utils = c(myUtils, "tool4"), path = base_path), "invalid path to supported")
})

teardown({
  # Cleanup temp dir & files
  cleanup_valid_path(check_paths)
})

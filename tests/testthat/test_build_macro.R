test_that("warn at least 1 var not assigned", {
  expect_warning(cmd_path_search(), "at least one")
  expect_warning(cmd_path_search(environment_var = NULL), "at least one")
  expect_warning(cmd_path_search(option_name = NULL), "at least one")
  expect_warning(cmd_path_search(default_path = NULL), "at least one")
})

test_that("At least 1 path is defined at calltime", {
  expect_warning(build_check <- cmd_path_search())
  
  expect_error(build_check(), "No path defined or detected")
})

test_that("Catches double assignment",{
  
  expect_error(
    cmd_path_search(environment_var = double_assign),
    "environment_var must contain"
  )
  
  expect_error(
    cmd_path_search(option_name = double_assign),
    "option_name must contain"
  )
  
  expect_error(
    cmd_path_search(default_path = double_assign),
    "default_path must contain"
  )
  
  # utils can have many values
  expect_success(
    cmd_path_search(default_path = tempdir(), utils = double_assign)
  )
})


test_that("Default path only, noUtils works",{
  expect_warning(
    check_build <- cmd_path_search(environment_var = NULL, 
                                     option_name = NULL, 
                                     default_path = NULL)
  )

  expect_equal(check_build(base_path), cmdfun:::sanitize_path(base_path))
  expect_error(check_build(base_path, util = myUtils[1]), "no defined utils")
})

test_that("Passing invalid user path throws error", {
  check_build <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  expect_equal(check_build(), cmdfun:::sanitize_path(base_path))
  expect_error(check_build("bad/path"), "does not exist")
  
})

test_that("Defining & checking utils works", {
  check_build <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  expect_equal(check_build(), cmdfun:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), cmdfun:::sanitize_path(file.path(base_path, myUtils[1])))
  # Expect errror with many utils
  expect_error(check_build(base_path, util = myUtils), "util must be NULL or length 1")
})

test_that("Options definition works",{
  check_build <- cmd_path_search(environment_var = NULL, 
                             option_name = test_option, 
                             default_path = "bad/path",
                             utils = myUtils)
  
  expect_error(check_build(), "bad/path, does not exist")
  
  R.utils::setOption(test_option, base_path)
  
  expect_equal(check_build(), cmdfun:::sanitize_path(base_path))
  expect_equal(check_build(util = myUtils[1]), cmdfun:::sanitize_path(file.path(base_path, myUtils[1])))
  
  # Test inheritance of default w/ bad option
  R.utils::setOption(test_option, "wrong_path")
  expect_error(check_build(), "bad/path, does not exist")
  
  R.utils::setOption(test_option, NULL)
})

test_that("Environment definition works", {
  check_build <- cmd_path_search(environment_var = test_env_var, 
                             utils = myUtils)
  expect_error(check_build(), "No path defined or detected")
  
  Sys.setenv(test_env_var = base_path)
  check_build <- cmd_path_search(environment_var = test_env_var, 
                                   utils = myUtils)
  
  expect_equal(check_build(base_path), cmdfun:::sanitize_path(base_path))
  expect_equal(check_build(base_path, util = myUtils[1]), cmdfun:::sanitize_path(file.path(base_path, myUtils[1])))
  
  # Test inheritance of default w/ bad environment var
  Sys.setenv(test_env_var = "wrong_path")
  check_build <- cmd_path_search(environment_var = test_env_var, 
                                    default_path = "bad/path")
  expect_error(check_build(), "bad/path, does not exist")
  
  Sys.setenv(test_env_var = "")
})

test_that("Util warnings work", {
  expect_equal(.check_valid_util(util = myUtils[1], utils = myUtils, path = base_path), 
               cmdfun:::sanitize_path(file.path(base_path, myUtils[1])))
  expect_error(.check_valid_util(util = "badUtil", utils = myUtils, path = base_path), "invalid path to an unsupported")
  expect_warning(.check_valid_util(util = allUtils[3], utils = myUtils, path = base_path), "exists but is not supported")
  # error when tool supported but not exist
  expect_error(.check_valid_util(util = "tool4", utils = c(myUtils, "tool4"), path = base_path), "invalid path to supported")
})

test_that("util listing works", {
  check_build <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  valid_utils <- check_paths[2:3]
  names(valid_utils) <- NULL
  expect_equal(check_build(util = TRUE), valid_utils)
})

test_that("is_valid_install behaves correctly", {
  check_build_good <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  is_valid <- cmd_install_is_valid(check_build_good)
  is_valid_util <- cmd_install_is_valid(check_build_good, util = myUtils[1])
  is_valid_util_bad <- cmd_install_is_valid(is_valid_good, util = 'bad_tool')
  
  expect_true(is_valid())
  expect_true(is_valid_util())
  expect_false(is_valid_util_bad())
  
  expect_false(is_valid('bad/path'))
  expect_false(is_valid_util('bad/path'))
  expect_false(suppressWarnings(is_valid_util_bad('bad/path')))
  
})


test_that("cmd_install_check works", {
  context("Check works with utils")
  check_build <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path,
                                   utils = myUtils)
  
  expect_invisible(cmd_install_check(check_build))
  expect_message(cmd_install_check(check_build), "main install")
  expect_message(cmd_install_check(check_build), "util install")
  expect_error(cmd_install_check('bad value'), "must be a function")
  
  context("Check works without utils")
  check_build_noutil <- cmd_path_search(environment_var = NULL, 
                                   option_name = NULL, 
                                   default_path = base_path)
  
  expect_invisible(cmd_install_check(check_build_noutil))
  expect_message(cmd_install_check(check_build_noutil), "main install")
  
  context("works when main is bad")
  expect_invisible(cmd_install_check(check_build, "bad/path"))
  expect_message(cmd_install_check(check_build, "bad/path"), cli::symbol$cross)
  
})

teardown({
  # Cleanup temp dir & files
  cleanup_valid_path(check_paths)
})


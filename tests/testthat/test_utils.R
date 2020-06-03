
test_that("Can detect illegal flags", {
  expect_false(flag_is_illegal("a"))
  expect_true(flag_is_illegal("&echo"))
  expect_true(flag_is_illegal("@echo"))
})


test_that("drop flags", {
  flags <- list("arg1" = "test", "arg2" = "dontdrop", "arg2" = "dropMe")
  expect_equal(cmd_list_drop(flags, c("arg2")), list(arg1 = "test"))
  expect_equal(cmd_list_drop(flags, c("arg2" = "dropMe")), list(arg1 = "test", arg2 = "dontdrop"))
  expect_equal(cmd_list_drop(flags, c("arg2" = "DROOE")), flags)
  expect_equal(cmd_list_drop(flags, c("arg2" = "DROOE", "arg2" = "dropMe")), 
                          list(arg1 = "test", arg2 = "dontdrop"))
  
  moreFlags <- list(arg1 = "test", arg2 = "arg2", "arg2" = "test")
  expect_warning(cmd_list_drop(moreFlags, c("arg1" = "test", "arg2")), "values have no names")
  expect_equal(
    suppressWarnings(cmd_list_drop(moreFlags, c("arg1" = "test", "arg2"))), 
    list(arg2 = "arg2", "arg2" = "test")
    )
})

test_that("keep flags", {
  flags <- list("arg1" = "test", "arg2" = "dontdrop", "arg2" = "dropMe")
  expect_equal(cmd_list_keep(flags, c("arg2")), list(arg2 = "dontdrop", arg2 = "dropMe"))
  expect_equal(cmd_list_keep(flags, c("arg2" = "dropMe")), list(arg2 = "dropMe"))
  expect_equal(cmd_list_keep(flags, c("arg2" = "DROOE")), list())
  expect_equal(cmd_list_keep(flags, c("arg2" = "DROOE", "arg2" = "dropMe")), 
                          list(arg2 = "dropMe"))
  
  moreFlags <- list(arg1 = "test", arg2 = "arg2", "arg2" = "test")
  expect_warning(cmd_list_keep(moreFlags, c("arg1" = "test", "arg2")), "values have no names")
  expect_equal(
    suppressWarnings(cmd_list_keep(moreFlags, c("arg1" = "test", "arg2"))), 
    list(arg1 = "test")
    )
})

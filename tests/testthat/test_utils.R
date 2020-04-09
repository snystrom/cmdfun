
test_that("Can detect illegal flags", {
  expect_false(flag_is_illegal("a"))
  expect_true(flag_is_illegal("&echo"))
  expect_true(flag_is_illegal("@echo"))
})


test_that("drop flags", {
  flags <- list("arg1" = "test", "arg2" = "dontdrop", "arg2" = "dropMe")
  expect_equal(drop_flags(flags, c("arg2")), list(arg1 = "test"))
  expect_equal(drop_flags(flags, c("arg2" = "dropMe")), list(arg1 = "test", arg2 = "dontdrop"))
  expect_equal(drop_flags(flags, c("arg2" = "DROOE")), flags)
  expect_equal(drop_flags(flags, c("arg2" = "DROOE", "arg2" = "dropMe")), 
                          list(arg1 = "test", arg2 = "dontdrop"))
  
  moreFlags <- list(arg1 = "test", arg2 = "arg2", "arg2" = "test")
  expect_warning(drop_flags(moreFlags, c("arg1" = "test", "arg2")), "values have no names")
  expect_equal(
    suppressWarnings(drop_flags(moreFlags, c("arg1" = "test", "arg2"))), 
    list(arg2 = "arg2", "arg2" = "test")
    )
})


test_that("Can detect illegal flags", {
  expect_false(flag_is_illegal("a"))
  expect_true(flag_is_illegal("&echo"))
  expect_true(flag_is_illegal("@echo"))
})

library(dotargs)

named_test <- function(arg1, ...){
  getNamedArgs()
}

all_test <- function(arg1, ...){
  getAllArgs()
}

dot_test <- function(arg1, ...){
  getDotArgs()
}

pipe_test <- function(arg1, ...){
  getAllArgs() %>% 
    argsToFlags() %>% 
    crystallize_flags()
}

pipe_test_named <- function(arg1, arg2, ...){
  getNamedArgs() %>% 
    argsToFlags() %>% 
    crystallize_flags()
}

keep_test <- function(arg1, arg2, ...){
  getAllArgs(keep = c("arg1", "dot_keep")) %>% 
    argsToFlags() %>% 
    crystallize_flags()
}

drop_test <- function(arg1, arg2, ..){
  getAllArgs(drop = "arg2") %>% 
    argsToFlags() %>% 
    crystallize_flags()
}

test_that("Can get Named Args", {
  expect_equal(named_test("a"), list("arg1" = "a"))
})

test_that("Can get Dot Args", {
  expect_equal(dot_test("a", "b" = 2), list("b" = 2))
})

test_that("Can get All Args", {
  expect_equal(all_test("a", "b" = 2), list("arg1" = "a", "b" = 2))
})

test_that("Output is list", {
  expect_type(all_test(), "list")
  expect_type(named_test(), "list")
  expect_type(dot_test(), "list")
})

test_that("Pipe works", {
  expect_equal(pipe_test("a"), c("-arg1", "a"))
})

test_that("T/F Filtered", {
  expect_equal(pipe_test(T,arg2 = F), c("-arg1"))
  expect_equal(pipe_test(T,arg2 = T), c("-arg1", "-arg2"))
  expect_error(pipe_test_named(T,arg2 = T, arg2 = F), "formal argument \"arg2\"")
})

test_that("Keep/drop works", {
  expect_null(drop_test())
  expect_null(keep_test())
  expect_equal(drop_test(arg1 = "test", arg2 = "drop"), 
               c("-arg1", "test"))
  expect_equal(keep_test(arg1 = "test", arg2 = "drop", dot_keep = "value"), 
               c("-arg1", "test", "-dot_keep", "value"))
})
  

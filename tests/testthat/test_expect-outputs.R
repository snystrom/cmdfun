test_that("cmd_file_expect works", {
  expect_equal(cmd_file_expect(exist_prefix, exist_ext, outdir = dir), exist_file_combn_list)
  expect_error(cmd_file_expect(exist_prefix, exist_ext, outdir = "bad/dir"))
})

test_that("cmd_file_combn Generates correct names",{
  context("1 file, many ext")
  expect_equal(cmd_file_combn("myFile", c("txt", "ext", "xml")),
               list(txt = "./myFile.txt",
                    ext = "./myFile.ext",
                    xml = "./myFile.xml"))

  context("many files, 1 ext")
  expect_equal(cmd_file_combn(c("myFile", "myFile1", "myFile2"), "txt"),
               list(myFile = "./myFile.txt",
                    myFile1 = "./myFile1.txt",
                    myFile2 = "./myFile2.txt"))
  
  context("many files, many ext")
  out_expected <- list("a.txt" = "./a.txt",
                       "a.csv" = "./a.csv",
                       "b.txt" = "./b.txt",
                       "b.csv" = "./b.csv")
  
  expect_equal(cmd_file_combn(c("a", "b"), c("txt", "csv")), out_expected)
})

test_that("Outdir works",{
  expect_equal(
    cmd_file_combn("myFile", "txt", outdir = "test/test"),
    cmd_file_combn("myFile", "txt", outdir = "test/test/")
  )
  
  expect_equal(
    cmd_file_combn("myFile", "txt", outdir = "test/test"),
    list("txt" = "test/test/myFile.txt")
  )
})

test_that("combn commands work", {
  expect_equal(combn_prefix_suffix(c("a", "b"), c(1,2)), c("a.1", "a.2", "b.1", "b.2"))
  expect_equal(combn_prefix_suffix(c("a", "b", "c"), c(1,2)), c("a.1", "a.2", "b.1", "b.2", "c.1", "c.2"))
  expect_equal(combn_prefix_suffix(c("a", "b", "c"), c(1,2,3)), c("a.1", "a.2", "a.3",
                                                                  "b.1", "b.2", "b.3",
                                                                  "c.1", "c.2", "c.3"))
  
})

test_that("combn utils work", {
  context("merge_combn_vector")
  expect_equal(merge_combn_vector(c("1", "2")), "1.2")
  expect_equal(merge_combn_vector(1:2), "1.2")
  
  expect_error(merge_combn_vector(1:3), "length")
  expect_error(merge_combn_vector(1), "length")
  
  context("combine_and_merge")
  # single vector
  expect_equal(combine_and_merge(1:3), c("1.2", "1.3", "2.3"))
  # nested vector
  expect_equal(combine_and_merge(c(c("a", "b"), c(1,2))), c("a.b", "a.1", "a.2", "b.1", "b.2", "1.2"))
 
})

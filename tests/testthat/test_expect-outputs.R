test_that("Expect Outputs Generates correct names",{
  expect_equal(cmd_file_combn("myFile", c("txt", "ext", "xml")),
               list(txt = "./myFile.txt",
                    ext = "./myFile.ext",
                    xml = "./myFile.xml"))

  expect_equal(cmd_file_combn(c("myFile", "myFile1", "myFile2"), "txt"),
               list(myFile = "./myFile.txt",
                    myFile1 = "./myFile1.txt",
                    myFile2 = "./myFile2.txt"))
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

test_that("Expect Outputs Generates correct names",{
  expect_equal(expected_outputs(c("txt", "ext", "xml"), "myFile"),
               list(txt = "./myFile.txt",
                    ext = "./myFile.ext",
                    xml = "./myFile.xml"))

  expect_equal(expected_outputs("txt", c("myFile", "myFile1", "myFile2")),
               list(myFile = "./myFile.txt",
                    myFile1 = "./myFile1.txt",
                    myFile2 = "./myFile2.txt"))
})

test_that("Outdir works",{
  expect_equal(
    expected_outputs("txt", "myFile", outdir = "test/test"),
    expected_outputs("txt", "myFile", outdir = "test/test/")
  )
  
  expect_equal(
    expected_outputs("txt", "myFile", outdir = "test/test"),
    list("txt" = "test/test/myFile.txt")
  )
})

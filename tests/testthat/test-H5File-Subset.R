context("H5File")
fname <- "test.h5"

test_that("H5File-Subset-Group",{
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  
  f <- function() file[c("1", "2", "3")] 
  expect_that(f(), throws_error("length(groupname) == 1 is not TRUE", 
          fixed = TRUE))

  group3 <- file["/testgroup3"] 
  expect_that(group3, is_a("H5Group"))
  expect_that(group3@location, is_identical_to("/testgroup3"))
  
  groupnested <- group3["test"]
  expect_that(groupnested, is_a("H5Group"))
  expect_that(groupnested@location, is_identical_to("/testgroup3/test"))
  closeh5(groupnested)
  closeh5(group3)
  
  group3 <- file["/testgroup3"]
  grouprelative <- group3["/test"]
  expect_that(grouprelative, is_a("H5Group"))
  expect_that(grouprelative@location, is_identical_to("/testgroup3/test"))
  closeh5(grouprelative)
  closeh5(group3)
  
  # recursive group creation
  group5 <- file["/test1/test2/test3/test4/test5"]
  expect_that(group5, is_a("H5Group"))
  expect_that(group5@location, is_identical_to("/test1/test2/test3/test4/test5"))
  closeh5(group5)
  closeh5(file)
})

test_that("H5File-Subset-DataSet",{
  if(file.exists(fname)) file.remove(fname)
  file <- H5File(fname, "a")
     
  # TODO: should actually work
  #f <- file["/testgroup3", "test"]
  #expect_that(f(), throws_error("DataSet 'test' does not exist")) 
  
  file["test/test1/test2/test3", "dset1"] <- 1:10
  file["test/test1/test2/test3", "dset2"] <- matrix(1:9, nrow = 3)
  file["test/test1/test2/test3", "dset3"] <- array(1:6, dim = c(1, 2, 3))    
  file[, "dset4"] <- matrix(1:10, nrow = 5)
      
  expect_that(file["test/test1/test2/test3", "dset1"][], is_identical_to(1:10))
  expect_that(file["test/test1/test2/test3", "dset2"][], 
      is_identical_to(matrix(1:9, nrow = 3)))
  expect_that(file["test/test1/test2/test3", "dset3"][], 
      is_identical_to(array(1:6, dim = c(1, 2, 3))))
  expect_that(file[, "dset4"][], 
      is_identical_to(matrix(1:10, nrow = 5)))
  
  # check subsets
  expect_that(file["test/test1/test2/test3", "dset1"][1:5], is_identical_to(1:5))
  expect_that(file["test/test1/test2/test3", "dset2"][1:2,], 
      is_identical_to(matrix(1:9, nrow = 3)[1:2,]))
  expect_that(file["test/test1/test2/test3", "dset3"][1,1:2,3], 
      is_identical_to(array(1:6, dim = c(1, 2, 3))[1,1:2,3,drop = FALSE]))
  
  # set subsets
  # TODO: include test cases
  closeh5(file)
  expect_that(file.remove(fname), is_true())
})


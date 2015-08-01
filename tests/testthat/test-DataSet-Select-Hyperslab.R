context("DataSet-Select-Hyperslab")

fname <- "test.h5"

test_that("DataSet-Select-Hyperslab-params",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Check offset/count NAs
  testmat_n_na_all <- readDataSet(dset1, 
      selectDataSpace(dset1, rep(NA_integer_, 2), rep(NA_integer_, 2)))
  expect_that(testmat_n_na_all, is_identical_to(testmat_n))
  
  testmat_n_na_offset <- readDataSet(dset1, selectDataSpace(dset1, offset = c(2, 2)))
  expect_that(testmat_n_na_offset, 
      is_identical_to(testmat_n[2:nrow(testmat_n), 2:ncol(testmat_n)]))

  testmat_n_na_count <- readDataSet(dset1, selectDataSpace(dset1, count = c(2, 2)))
  expect_that(testmat_n_na_count, 
      is_identical_to(testmat_n[1:2, 1:2]))
  
  # Check offset/count length
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, count = 1:3))
  expect_that(f(), throws_error("Parameter count must have length of dataset dimensions"))    
  
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, offset = 1:3)) 
  expect_that(f(), throws_error("Parameter offset must have length of dataset dimensions"))    
      
  # Check offset/count positive    
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, offset = rep(-1, 2))) 
  expect_that(f(), throws_error("Elements of parameter offset must be greater than zero or NA"))    
      
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, count = rep(-1, 2))) 
  expect_that(f(), throws_error("Elements of parameter count must be greater than zero or NA"))    

  # Check boundaries
  testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, count = dim(testmat_n)))   
  expect_that(testmat_n_na_boundall, 
      is_identical_to(testmat_n))
  
  f <- function() testmat_n_na_bound_1 <- 
        readDataSet(dset1, selectDataSpace(dset1, offset = c(2, 2), count = dim(testmat_n)))   
  expect_that(f(), throws_error("subscript out of bounds"))
  
  dim1 <- dim(testmat_n)
  dim1[1] <- dim1[1] + 1
  f <- function() testmat_n_na_bound_1 <- readDataSet(dset1, selectDataSpace(dset1, count = dim1))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  dim2 <- dim(testmat_n)
  dim2[1] <- dim2[1] + 1
  f <- function() testmat_n_na_bound_1 <- readDataSet(dset1, selectDataSpace(dset1, count = dim2))
  expect_that(f(), throws_error("subscript out of bounds"))

  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Hyperslab-vector",{  
  testvec_n <- as.integer(1:90)
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  h5close(dset1)
  
  # Write hyperslab
  dset2 <- createDataSet(group, "testvec_n2", testvec_n)
  
  subvec <- 1:3
  f <- function() writeDataSet(dset2, subvec, selectDataSpace(dset1, offset = c(dset2@dim[1L] - 1), count = length(subvec)))
  expect_that(f(), throws_error("subscript out of bounds"))

  writeDataSet(dset2, subvec, selectDataSpace(dset2, offset = c(10), count = length(subvec)))
  h5close(dset2)
  
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testvec_n")
  
  testvec_n_read_all_vanilla <- readDataSet(dset1)
  expect_that(testvec_n_read_all_vanilla, is_identical_to(testvec_n))
  
  testvec_n_read_all <- readDataSet(dset1, selectDataSpace(dset1, c(1), length(testvec_n)))
  expect_that(testvec_n_read_all, is_identical_to(testvec_n))
  
  # Read entire dataset
  dset2 <- openDataSet(group, "testvec_n2")
  testvec_n2_read_all_vanilla <- readDataSet(dset2)
  testvec_n2 <- testvec_n
  testvec_n2[10:12] <- subvec
  expect_that(testvec_n2_read_all_vanilla, is_identical_to(testvec_n2))

  # Read hyperslab
  testvec_n_read <- readDataSet(dset1, selectDataSpace(dset1, c(2), c(4)))
  expect_that(testvec_n_read, is_identical_to(testvec_n[2:5]))
  
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})


test_that("DataSet-Select-Hyperslab-matrix",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  submat <- matrix(1L:9L, nrow = 3)
  f <- function() writeDataSet(dset2, submat, selectDataSpace(dset2, offset = c(dset2@dim[1L] - 1, 3), count = dim(submat)))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() writeDataSet(dset2, submat, selectDataSpace(dset2, offset = c(3, dset2@dim[2L] - 1), count = dim(submat)))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  writeDataSet(dset2, submat, selectDataSpace(dset2, offset = c(3, 3), count = dim(submat)))
  
  h5close(dset2)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_read_all_vanilla <- readDataSet(dset1)
  expect_that(testmat_n_read_all_vanilla, is_identical_to(testmat_n))
  
  testmat_n_read_all <- readDataSet(dset1, selectDataSpace(dset1, c(1, 1), dim(testmat_n)))
  expect_that(testmat_n_read_all, is_identical_to(testmat_n))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2 <- testmat_n
  testmat_n2[3:5, 3:5] <- submat
  expect_that(readDataSet(dset2), is_identical_to(testmat_n2))
  
  # Read hyperslab
  testmat_n_read <- readDataSet(dset1, selectDataSpace(dset1, c(1, 2), c(3, 4)))
  expect_that(testmat_n_read, is_identical_to(testmat_n[1:3, 2:5]))
  
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Hyperslab-array",{  
  testmat_n <- array(as.integer(1:90), dim = c(3, 3, 10))
  subarray <- array(as.integer(100:120), dim = c(2, 2, 5))
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  f <- function() writeDataSet(dset2, subarray, selectDataSpace(dset2, offset = c(dset2@dim[1L], 1, 1), count = dim(subarray)))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  writeDataSet(dset2, subarray, selectDataSpace(dset2, offset = c(2, 2, 3), count = dim(subarray)))
  
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_read_all_vanilla <- readDataSet(dset1)
  expect_that(testmat_n_read_all_vanilla, is_identical_to(testmat_n))
  
  testmat_n_read_all <- readDataSet(dset1, selectDataSpace(dset1, c(1, 1, 1), dim(testmat_n)))
  expect_that(testmat_n_read_all, is_identical_to(testmat_n))
  
  testmat_n_read_all <- readDataSet(dset1, selectDataSpace(dset1, offset = c(2, 2, 2)))
  dimtestmat <- dim(testmat_n)
  expect_that(testmat_n_read_all, is_identical_to(testmat_n[2:dimtestmat[1], 2:dimtestmat[2], 2:dimtestmat[3]]))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2_read_all <- readDataSet(dset2)
  testmat_n2 <- testmat_n
  testmat_n2[2:3, 2:3, 3:7] <- subarray
  expect_that(testmat_n2_read_all, is_identical_to(testmat_n2))
 
  # Read hyperslab
  testmat_n_read <- readDataSet(dset1, selectDataSpace(dset1, c(1, 1, 1), c(NA, NA, 3)))
  expect_that(testmat_n_read, is_identical_to(testmat_n[,,1:3]))
  
  testmat_n_read <- readDataSet(dset1, selectDataSpace(dset1, c(2, 2, 4)))
  expect_that(testmat_n_read, is_identical_to(testmat_n[2:3,2:3,4:10]))
  
  testmat_n_read <- readDataSet(dset1, selectDataSpace(dset1, c(2, 3, 4)))
  expect_that(testmat_n_read, is_identical_to(testmat_n[2:3,3,4:10, drop = FALSE]))
  
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

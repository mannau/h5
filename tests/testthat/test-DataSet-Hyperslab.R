context("DataSet-Hyperslab")

fname <- "test.h5"

test_that("DataSet-Hyperslab-params",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
  
  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_read_all_vanilla <- readDataSet(dset1)
  expect_that(testmat_n_read_all_vanilla, is_identical_to(testmat_n))
  
  testmat_n_read_all <- readDataSet(dset1, c(1, 1), dim(testmat_n))
  expect_that(testmat_n_read_all, is_identical_to(testmat_n))
  
  # Read hyperslab
  testmat_n_read <- readDataSet(dset1, c(1, 2), c(3, 4))
  expect_that(testmat_n_read, is_identical_to(testmat_n[1:3, 2:5]))
  
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
  
})

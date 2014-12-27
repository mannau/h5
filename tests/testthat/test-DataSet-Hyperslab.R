
context("DataSet-Hyperslab")

fname <- "test.h5"

test_that("DataSet-Hyperslab",{
        
  testmat_n <- matrix(rnorm(100), ncol = 10)
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  
  testmat_n_read <- readDataSet(dset1, c(1, 2), c(3, 3))
  
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
  
  
})
context("DataSet-Select-Elem")

fname <- "test.h5"

test_that("DataSet-Select-Elem-params",{  
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
  
  # Check elem NAs
  f <- function() readDataSet(dset1, selectDataSpace(dset1, elem = matrix(NA_integer_)))
  expect_that(f(), throws_error("NAs are not allowed in element coordinates"))    

  # Check elem datatype
  f <- function() readDataSet(dset1, selectDataSpace(dset1, elem = matrix("a")))
  expect_that(f(), throws_error("Element matrix must be of type double or integer"))    
   
  # Check elem length
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, 
        selectDataSpace(dset1, elem = matrix(1:9, nrow = 3)))
  expect_that(f(), throws_error("Number of elem matrix columns must equal length of dataset dimensions"))    

  # Check elem positive    
  f <- function() testmat_n_na_boundall <- readDataSet(dset1, selectDataSpace(dset1, elem = matrix(rep(0, 2)))) 
  expect_that(f(), throws_error("Elements of parameter elem must be greater or equal than one."))    

  f <- function() testmat_n_na_bound_1 <- 
        readDataSet(dset1, selectDataSpace(dset1, elem = cbind(rep(1:11, each = 9), rep(1:9, 11))))   
  expect_that(f(), throws_error("subscript out of bounds"))

  f <- function() testmat_n_na_bound_1 <- 
        readDataSet(dset1, selectDataSpace(dset1, elem = cbind(rep(1:10, each = 10), rep(1:10, 10))))   
  expect_that(f(), throws_error("subscript out of bounds"))
  
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Elem-vector",{  
  testvec_n <- as.integer(1:90)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  h5close(dset1)
  
  # Write Elem
  dset2 <- createDataSet(group, "testvec_n2", testvec_n)
  
  subvec <- 1:3
  f <- function() writeDataSet(dset2, subvec, selectDataSpace(dset1, elem = matrix(89:91)))
  expect_that(f(), throws_error("subscript out of bounds"))

  dspace <- selectDataSpace(dset2, elem = matrix(88:90))
  writeDataSet(dset2, subvec, dspace)
  h5close(dset2)
  
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testvec_n")
  
  # Read entire dataset
  dspace <- selectDataSpace(dset1, elem = matrix(1:dset1@dim))
  testvec_n_read_all <- readDataSet(dset1, dspace)
  expect_that(testvec_n_read_all, is_identical_to(testvec_n))

  # Read dataset parts
  selector <- as.integer(seq(1, dset1@dim), by = 2)
  dspace <- selectDataSpace(dset1, elem = matrix(selector))
  testvec_n_read_sel <- readDataSet(dset1, dspace)
  expect_that(testvec_n_read_sel, is_identical_to(selector))    
  h5close(dset1)
  
  dset2 <- openDataSet(group, "testvec_n2")
  expect_that(readDataSet(dset2), is_identical_to(c(1:87, 1:3)))

  h5close(dset2)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})


test_that("DataSet-Select-Elem-matrix",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  submat <- matrix(-1L:-9L, nrow = 3)
  f <- function() selectDataSpace(dset2, elem = matrix(c(10, 11, 9, 9), nrow = 2))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() selectDataSpace(dset2, elem = matrix(c(10, 10, 9, 10), nrow = 2))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  writeDataSet(dset2, submat, selectDataSpace(dset2, elem = cbind(rep(c(1, 3, 5), each = 3), rep(c(1, 3, 5), 3))))
  
  h5close(dset2)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_na_boundall <- readDataSet(dset1, 
      selectDataSpace(dset1, elem = cbind(rep(1:10, 9), rep(1:9, each = 10))))
  expect_that(testmat_n_na_boundall, is_identical_to(as.vector(testmat_n)))
      
  # Read single element
  testmat_n_read_2_2 <- readDataSet(dset1, selectDataSpace(dset1, elem = t(c(2L, 2L))))
  expect_that(testmat_n_read_2_2, is_identical_to(testmat_n[2, 2]))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2 <- testmat_n
  testmat_n2[c(1, 3, 5), c(1, 3, 5)] <- submat
  expect_that(readDataSet(dset2), is_identical_to(testmat_n2))
  
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Elem-array",{  
  testmat_n <- array(as.integer(1:90), dim = c(3, 3, 10))
  subarray <- array(as.integer(-100:-120), dim = c(2, 2, 5))
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  
  f <- function() selectDataSpace(dset2, elem = matrix(c(3, 4, 3, 3, 10, 10), nrow = 2))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() selectDataSpace(dset2, elem = matrix(c(3, 3, 3, 4, 10, 10), nrow = 2))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() selectDataSpace(dset2, elem = matrix(c(3, 3, 3, 3, 10, 11), nrow = 2))
  expect_that(f(), throws_error("subscript out of bounds"))

  writeDataSet(dset2, subarray, selectDataSpace(dset2, 
          elem = cbind(rep(2:3, each = 2*5), rep(2:3, each = 5), rep(3:7, 4))))
  
  h5close(dset2)
  h5close(group)
  h5close(file)

  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_read_all <- readDataSet(dset1, selectDataSpace(dset1, 
          elem = cbind(rep(1:3), rep(1:3, each = 3), rep(1:10, each = 9))))

  expect_that(testmat_n_read_all, is_identical_to(as.vector(testmat_n)))
  
  testmat_n_read_2 <- readDataSet(dset1, selectDataSpace(dset1, elem = rbind(c(1, 2, 3), c(2, 3, 10))))
  expect_that(testmat_n_read_2, is_identical_to(c(testmat_n[1, 2, 3], testmat_n[2, 3, 10])))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2_read_all <- readDataSet(dset2)
  testmat_n2 <- testmat_n
  testmat_n2[2:3, 2:3, 3:7] <- subarray
  expect_that(testmat_n2_read_all, is_identical_to(testmat_n2))
 
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

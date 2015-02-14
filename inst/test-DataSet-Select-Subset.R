context("DataSet-Select-Subset")

fname <- "test.h5"

test_that("DataSet-Select-Subset-params",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
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
  
  # Check elem NAs
  f <- function() dset1[c(1:5, NA), 1:5]
  expect_that(f(), throws_error("NAs are not allowed in element coordinates"))    

  # Check elem datatype
  #f <- function() readDataSet(dset1, selectDataSpace(dset1, elem = matrix("a")))
  #expect_that(f(), throws_error("Element matrix must be of type double or integer"))    
   
  # Check elem length
  #f <- function() testmat_n_na_boundall <- readDataSet(dset1, 
  #      selectDataSpace(dset1, elem = matrix(1:9, nrow = 3)))
  #expect_that(f(), throws_error("Number of elem matrix columns must equal length of dataset dimensions"))    

  # Check elem positive    
  f <- function() dset1[0L, 1L]
  expect_that(f(), throws_error("Elements of parameter elem must be greater or equal than one."))    

  f <- function() testmat_n_na_bound_1 <- dset1[1:11, 1:9]
  expect_that(f(), throws_error("subscript out of bounds"))

  f <- function() testmat_n_na_bound_1 <- dset1[1:10, 1:10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
  
})

test_that("DataSet-Select-Subset-vector",{  
  testvec_n <- as.integer(1:90)
  
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  closeh5(dset1)
  
  # Write Elem
  dset2 <- createDataSet(group, "testvec_n2", testvec_n)
  
  subvec <- 1:3
  f <- function() dset2[89:91]
  expect_that(f(), throws_error("subscript out of bounds"))

  dset2[88:90] <- subvec
  closeh5(dset2)
  
  closeh5(group)
  closeh5(file)
  
  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testvec_n")
  
  # Read entire dataset
  testvec_n_read_all <- dset1[1:dset1@dim]
  expect_that(testvec_n_read_all, is_identical_to(testvec_n))

  # Read dataset parts
  selector <- as.integer(seq(1, dset1@dim), by = 2)
  testvec_n_read_sel <- dset1[selector]
  expect_that(testvec_n_read_sel, is_identical_to(selector))    
  closeh5(dset1)
  
  dset2 <- openDataSet(group, "testvec_n2")
  expect_that(dset2[], is_identical_to(c(1:87, 1:3)))

  closeh5(dset2)
  closeh5(group)
  closeh5(file)
})


test_that("DataSet-Select-Subset-matrix",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  closeh5(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  submat <- matrix(-1L:-9L, nrow = 3)
  f <- function() dset2[10:11, 9]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[10, 9:10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  dset2[c(1, 3, 5), c(1, 3, 5)] <- submat
  
  closeh5(dset2)
  closeh5(group)
  closeh5(file)
  
  file <- new( "H5File", fname, "r")
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
  
  closeh5(dset2)
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
})

test_that("DataSet-Select-Subset-array",{  
  testmat_n <- array(as.integer(1:90), dim = c(3, 3, 10))
  subarray <- array(as.integer(-100:-120), dim = c(2, 2, 5))
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  closeh5(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  
  f <- function() dset2[3:4, 3, 10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 3:4, 10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 3, 10:11]
  expect_that(f(), throws_error("subscript out of bounds"))

  dset2[2:3, 2:3, 3:7] <- subarray

  closeh5(dset2)
  closeh5(group)
  closeh5(file)

  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  testmat_n_read_all <- dset1[1:3, 1:3, 1:10]
  expect_that(testmat_n_read_all, is_identical_to(as.vector(testmat_n)))
  
  testmat_n_read_2 <- c(dset1[1, 2, 3], dset1[2, 3, 10]) 
  expect_that(testmat_n_read_2, is_identical_to(c(testmat_n[1, 2, 3], testmat_n[2, 3, 10])))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2_read_all <- dset2[]
  testmat_n2 <- testmat_n
  testmat_n2[2:3, 2:3, 3:7] <- subarray
  expect_that(testmat_n2_read_all, is_identical_to(testmat_n2))
 
  closeh5(dset2)
  closeh5(dset1)
  closeh5(group)
  closeh5(file)
})

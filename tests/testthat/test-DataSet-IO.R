context("DataSet-Vector")

fname <- "test.h5"

test_that("DataSet-Vector",{
  testvec_n <- rnorm(120)
  testvec_i <- as.integer(runif(120)*10000)
  testvec_l <- as.logical(round(runif(120)))
  testvec_l <- as.logical(round(runif(120)))
  testvec_c <-rep(paste0(LETTERS[1:3], rev(LETTERS)[1:3]), 120/3)
  testvec_c[1] <- paste0(testvec_c[1], testvec_c[1])
  testvec_c[40] <- paste0(testvec_c[1], testvec_c[1])
  
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  closeh5(dset1)
  dset2 <- createDataSet(group, "testvec_l", testvec_l)
  closeh5(dset2)
  dset3 <- createDataSet(group, "testvec_i", testvec_i)
  closeh5(dset3)
  
  dset4 <- createDataSet(group, "testvec_c", testvec_c)
  closeh5(dset4)
  closeh5(group)
  closeh5(file)
  
  ### Check if written data equals input data
  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset11 <- openDataSet(group, "testvec_n")
  testvec_n_read <- readDataSet(dset11)
  closeh5(dset11)
  
  expect_that(testvec_n, is_identical_to(testvec_n_read))
  
  dset13 <- openDataSet(group, "testvec_i")
  testvec_i_read <- readDataSet(dset13)
  closeh5(dset13)
  expect_that(testvec_i, is_identical_to(testvec_i_read))
  
  dset14 <- openDataSet(group, "testvec_c")
  testvec_c_read <- readDataSet(dset14)
  closeh5(dset14)
  expect_that(testvec_c, is_identical_to(testvec_c_read))
  closeh5(group)
  closeh5(file)		
  file.remove(fname)
})

test_that("DataSet-Vector-boundaries",{
  
  if(file.exists(fname)) file.remove(fname)
  
  file <- new( "H5File", fname, "a")
  
  testvec_i_0 <- integer(0)
  dset0 <- createDataSet(file, "testvec_i_0", testvec_i_0)
  closeh5(dset0)
  
  testvec_i_1 <- 1L
  dset1 <- createDataSet(file, "testvec_i_1", testvec_i_1)
  closeh5(dset1)
  
  testvec_i_2 <- c(1L, 1L)
  dset2 <- createDataSet(file, "testvec_i_2", testvec_i_2)
  closeh5(dset2)
  
#  testvec_i_max <- rep(1L, length.out = .Machine$integer.max)
#  dsetmax <- createDataSet(file, "testvec_i_max", testvec_i_max)
#  closeh5(dsetmax)
#  
#  testvec_i_max_1 <- rep(1L, length.out = .Machine$integer.max + 1)
#  dsetmax_1 <- createDataSet(file, "testvec_i_max_1", testvec_i_max_1)
#  closeh5(dsetmax_1)

  closeh5(file)
  
  ### Check if written data equals input data
  file <- new( "H5File", fname, "r")
  
  dset0 <- openDataSet(file, "testvec_i_0")
  testvec_i_0_read <- readDataSet(dset0)
  closeh5(dset0)
  expect_that(testvec_i_0, is_identical_to(testvec_i_0_read))
  
  dset1 <- openDataSet(file, "testvec_i_1")
  testvec_i_1_read <- readDataSet(dset1)
  closeh5(dset1)
  expect_that(testvec_i_1, is_identical_to(testvec_i_1_read))
  
#  dsetmax <- openDataSet(file, "testvec_i_max")
#  testvec_i_max_read <- readDataSet(dsetmax)
#  closeh5(dsetmax)
#  expect_that(testvec_i_max, is_identical_to(testvec_i_max_read))
#  
#  dsetmax_1 <- openDataSet(file, "testvec_i_max_1")
#  testvec_i_max_1_read <- readDataSet(dsetmax_1)
#  closeh5(dsetmax_1)
#  expect_that(testvec_i_max_1, is_identical_to(testvec_i_max_1_read))
  
  closeh5(file)		
})

context("DataSet-Matrix")

test_that("datatypes-Matrix",{
  
  testmat_n <- matrix(rnorm(120), ncol = 3)
  testmat_i <- matrix(as.integer(runif(120)*10000), ncol = 3)
  testmat_l <- matrix(as.logical(round(runif(120))), ncol = 3)
  testmat_l <- matrix(as.logical(round(runif(120))), ncol = 3)
  testmat_c <- matrix(rep(paste0(LETTERS[1:3], rev(LETTERS)[1:3]), 120/3), ncol = 3)
  testmat_c[1,1] <- paste0(testmat_c[1,1], testmat_c[1,1])
  testmat_c[40,2] <- paste0(testmat_c[1,1], testmat_c[1,1])
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  closeh5(dset1)
  dset2 <- createDataSet(group, "testmat_l", testmat_l)
  closeh5(dset2)
  dset3 <- createDataSet(group, "testmat_i", testmat_i)
  closeh5(dset3)
  
  dset4 <- createDataSet(group, "testmat_c", testmat_c)
  closeh5(dset4)
  closeh5(group)
  closeh5(file)
  
  ### Check if written data equals input data
  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset11 <- openDataSet(group, "testmat_n", "double")
  testmat_n_read <- readDataSet(dset11)
  closeh5(dset11)
  expect_that(testmat_n, is_identical_to(testmat_n_read))
  
  dset13 <- openDataSet(group, "testmat_i", "integer")
  testmat_i_read <- readDataSet(dset13)
  closeh5(dset13)
  expect_that(testmat_i, is_identical_to(testmat_i_read))
  
  dset14 <- openDataSet(group, "testmat_c", "character")
  testmat_c_read <- readDataSet(dset14)
  closeh5(dset14)
  
  expect_that(testmat_c, is_identical_to(testmat_c_read))
  closeh5(group)
  closeh5(file)
  
  file.remove(fname)
})

context("DataSet-Array")

test_that("datatypes-Array",{
  
  testmat_n <- array(rnorm(120), c(3,4,10))
  testmat_i <- array(as.integer(runif(120)*10000), c(3,4,2,5))
  testmat_l <- array(as.logical(round(runif(120))), c(3,4,10))
  testmat_l <- array(as.logical(round(runif(120))), c(3,4,2,5))
  testmat_c <- array(rep(paste0(LETTERS[1:3], rev(LETTERS)[1:3]), 120/3), c(3,4,2,5))
  testmat_c[1,1,1,1] <- paste0(testmat_c[1,1,1,1], testmat_c[1,1,1,1])
  testmat_c[3,4,2,1] <- paste0(testmat_c[1,1,1,1], testmat_c[1,1,1,1])
  
  fname <- "test.h5"
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  closeh5(dset1)
  dset2 <- createDataSet(group, "testmat_l", testmat_l)
  closeh5(dset2)
  dset3 <- createDataSet(group, "testmat_i", testmat_i)
  closeh5(dset3)
  
  dset4 <- createDataSet(group, "testmat_c", testmat_c)
  closeh5(dset4)
  closeh5(group)
  closeh5(file)
  
  
  ### Check if written data equals input data
  file <- new( "H5File", fname, "r")
  group <- openGroup(file, "/testgroup")
  dset11 <- openDataSet(group, "testmat_n")
  testmat_n_read <- readDataSet(dset11)
  closeh5(dset11)
  expect_that(testmat_n, is_identical_to(testmat_n_read))
  
  dset13 <- openDataSet(group, "testmat_i")
  testmat_i_read <- readDataSet(dset13)
  closeh5(dset13)
  expect_that(testmat_i, is_identical_to(testmat_i_read))
  
  dset14 <- openDataSet(group, "testmat_c")
  testmat_c_read <- readDataSet(dset14)
  closeh5(dset14)
  expect_that(testmat_c, is_identical_to(testmat_c_read))
  closeh5(group)
  closeh5(file)
  
  file.remove(fname)		
})

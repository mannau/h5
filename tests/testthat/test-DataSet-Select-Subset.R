context("DataSet-Select-Subset")

fname <- "test.h5"

test_that("DataSet-Select-Subset-params",{  
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
  
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-vector-read",{  
  testvec_n <- as.integer(1:90)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  h5close(dset1)
  
  # Write Elem
  dset2 <- createDataSet(group, "testvec_n2", testvec_n)
  
  subvec <- 1:3
  f <- function() dset2[89:91]
  expect_that(f(), throws_error("subscript out of bounds"))

  dset2[88:90] <- subvec
  h5close(dset2)
  
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testvec_n")
  
  # Read entire dataset
  testvec_n_read_all <- dset1[1:dset1@dim]
  expect_that(testvec_n_read_all, is_identical_to(testvec_n))

  # Read dataset parts
  selector <- as.integer(seq(1, dset1@dim), by = 2)
  testvec_n_read_sel <- dset1[selector]
  expect_that(testvec_n_read_sel, is_identical_to(selector))    
  h5close(dset1)
  
  dset2 <- openDataSet(group, "testvec_n2")
  expect_that(dset2[], is_identical_to(c(1:87, 1:3)))

  h5close(dset2)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-read-string", { 
      vec <- paste0(LETTERS[1:9], c(LETTERS[1:9]))
      vec[1] <- "AAA"
      mat <- matrix(LETTERS[1:9], nrow = 3)
      arr <- array(paste0(LETTERS[rep(1, 45)], c(LETTERS[rep(1, 45)])), 
          dim = c(3, 3, 5))
      arr[1, 1, 1] <- "blablabla"
          
      if(file.exists(fname)) file.remove(fname)
      file <- h5file(fname)
      file["test/testvec"] <- LETTERS[1:9]
      file["test/testmat/testmat"] <- mat
      file["test/testarray"] <- arr
      h5close(file)
      
      file <- h5file(fname, "r")
      testvec <- file["test/testvec"]
      expect_that(testvec[1:3], is_identical_to(LETTERS[1:3]))
      h5close(testvec)
      
      testmat <- file["test/testmat/testmat"]
      expect_that(testmat[1:2, ], is_identical_to(mat[1:2, ]))
      h5close(testmat)
      
      testarray <- file["test/testarray"]
      expect_that(testarray[], is_identical_to(arr))
      h5close(testarray)
      h5close(file)
      expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-read-string-fixed", { 
      vec <- paste0(LETTERS[1:9], c(LETTERS[1:9]))
      vec[1] <- "AAA"
      mat <- matrix(LETTERS[1:9], nrow = 3)
      arr <- array(paste0(LETTERS[rep(1, 45)], c(LETTERS[rep(1, 45)])), 
          dim = c(3, 3, 5))
      arr[1, 1, 1] <- "blablabla"
      
      if(file.exists(fname)) file.remove(fname)
      file <- h5file(fname)
      file["test/testvec", size = 1] <- LETTERS[1:9]
      file["test/testmat/testmat", size = 1] <- mat
      file["test/testarray", size = max(nchar(as.vector(arr)))] <- arr
      h5close(file)
      
      file <- h5file(fname, "r")
      testvec <- file["test/testvec"]
      expect_that(testvec[1:3], is_identical_to(LETTERS[1:3]))
      h5close(testvec)
      
      testmat <- file["test/testmat/testmat"]
      expect_that(testmat[1:2, ], is_identical_to(mat[1:2, ]))
      h5close(testmat)
      
      testarray <- file["test/testarray"]
      expect_that(testarray[], is_identical_to(arr))
      h5close(testarray)
      h5close(file)
      expect_that(file.remove(fname), is_true())
    })
      
test_that("DataSet-Select-Subset-vector-write",{  
  testvec_n <- as.integer(1:90)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testvec_n", testvec_n)
  h5close(dset1)
  
  # Write Elem
  dset2 <- createDataSet(group, "testvec_n2", testvec_n)
  
  subvec <- 1:3
  f <- function() dset2[89:91] <- subvec
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[87:90] <- subvec
  expect_that(f(), throws_error("number of items to replace is not equal to replacement length"))
  
  dset2[88:90] <- subvec
  h5close(dset2)
  
  dset3 <- createDataSet(group, "testvec_n3", testvec_n)
  dset3[] <- -testvec_n
  
  h5close(dset3)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testvec_n")
  
  # Read entire dataset
  testvec_n_read_all <- dset1[]
  expect_that(testvec_n_read_all, is_identical_to(testvec_n))
  
  # Read dataset parts
  selector <- as.integer(seq(1, dset1@dim), by = 2)
  testvec_n_read_sel <- dset1[selector]
  expect_that(testvec_n_read_sel, is_identical_to(selector))    
  h5close(dset1)
  
  dset2 <- openDataSet(group, "testvec_n2")
  expect_that(dset2[], is_identical_to(c(1:87, 1:3)))
  
  dset3 <- openDataSet(group, "testvec_n3")
  expect_that(dset3[], is_identical_to(-testvec_n))
  
  h5close(dset3)
  h5close(dset2)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-matrix-read",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  submat <- matrix(-1L:-9L, nrow = 3)
  f <- function() dset2[10:11, 9]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[10, 9:10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[1, 1, 1]
  expect_that(f(), throws_error("incorrect number of dimensions"))
  
  f <- function() dset2[ , , ]
  expect_that(f(), throws_error("incorrect number of dimensions"))

  h5close(dset2)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  expect_that(readDataSet(dset1), is_identical_to(testmat_n))
  expect_that(dset1[], is_identical_to(testmat_n))
  expect_that(dset1[,], is_identical_to(testmat_n))
      
  # Read single element
  testmat_n_read_2_2 <- readDataSet(dset1, selectDataSpace(dset1, elem = t(c(2L, 2L))))
  expect_that(testmat_n_read_2_2, is_identical_to(testmat_n[2, 2]))
  expect_that(dset1[2, 2], is_identical_to(testmat_n[2, 2, drop = FALSE]))
  
  # Read columns / rows
  # TODO: drop should also be implemented
  expect_that(dset1[, 2], is_identical_to(testmat_n[, 2, drop = FALSE]))
  expect_that(dset1[, 2:4], is_identical_to(testmat_n[, 2:4, drop = FALSE]))
  expect_that(dset1[2, ], is_identical_to(testmat_n[2,, drop = FALSE]))
  expect_that(dset1[2:4, ], is_identical_to(testmat_n[2:4,, drop = FALSE]))

  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-matrix-write",{  
  testmat_n <- matrix(as.integer(1:90), ncol = 9)
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  submat <- matrix(-1L:-9L, nrow = 3)
  f <- function() dset2[10:11, 9] <- matrix(rep(0, 2*9), nrow = 2)
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[10, 9:10] <- matrix(rep(0, 2*9), ncol = 2)
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[1, 1, 1] <- testmat_n
  expect_that(f(), throws_error("incorrect number of dimensions"))
  
  # TODO: this should actually work!
  #f <- function() dset2[ , , ] <- testmat_n
  #expect_that(f(), throws_error("incorrect number of dimensions"))
  
  # Write single element
  dset2[2, 2] <- -2L 
  expect_that(dset2[2, 2], is_identical_to(matrix(-2L)))
  
  dset2[,] <- dset1[,]
  expect_that(dset2[,], is_identical_to(dset1[,]))
  
  dset2[] <- dset1[]
  expect_that(dset2[], is_identical_to(dset1[]))
  
  # Assign submatrix to DataSet
  f <- function()  dset2[c(1, 3, 5, 1), c(1, 3, 5)] <- submat
  expect_that(f(), throws_error("number of items to replace is not equal to replacement length"))
  
  dset2[c(1, 3, 5), c(1, 3, 5)] <- submat
  h5close(dset2)
 
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2 <- testmat_n
  testmat_n2[c(1, 3, 5), c(1, 3, 5)] <- submat
  expect_that(readDataSet(dset2), is_identical_to(testmat_n2))
  
  dset3 <- createDataSet(group, "testmat_n3", testmat_n)
  dset3[1:2, ] <- matrix(rep(1L, 18), nrow = 2)
  
  dset4 <- createDataSet(group, "testmat_n4", testmat_n)
  dset4[, c(1, 3, 5)] <- matrix(rep(1L, 30), nrow = 10)
  
  h5close(dset4)
  h5close(dset3)
  h5close(dset1)
  h5close(dset2)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  dset2 <- openDataSet(group, "testmat_n2")
  dset3 <- openDataSet(group, "testmat_n3")
  dset4 <- openDataSet(group, "testmat_n4")
  
  # Read entire dataset
  expect_that(readDataSet(dset1), is_identical_to(testmat_n))
  expect_that(dset1[], is_identical_to(testmat_n))
  expect_that(dset2[], is_identical_to(testmat_n2))
  testmat_n3 <- testmat_n
  testmat_n3[1:2,] <- 1L
  expect_that(dset3[], is_identical_to(testmat_n3))
  testmat_n4 <- testmat_n
  testmat_n4[,c(1, 3, 5)] <- 1L
  expect_that(dset4[], is_identical_to(testmat_n4))
  
  h5close(dset4)
  h5close(dset3)
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-array-read",{  
  testmat_n <- array(as.integer(1:90), dim = c(3, 3, 10))
  subarray <- array(as.integer(-100:-120), dim = c(2, 2, 5))
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  h5close(dset1)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  
  f <- function() dset2[3:4, 3, 10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 3:4, 10]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 3, 10:11]
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[1, 1, 1, 1]
  expect_that(f(), throws_error("incorrect number of dimensions"))
  
  # TODO: This should generate an ERROR!
  #f <- function() dset2[ , , , ]
  #expect_that(f(), throws_error("incorrect number of dimensions"))

  dset2[2:3, 2:3, 3:7] <- subarray

  h5close(dset2)
  h5close(group)
  h5close(file)

  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  
  # Read entire dataset
  expect_that(readDataSet(dset1), is_identical_to(testmat_n))
  expect_that(dset1[], is_identical_to(testmat_n))
  expect_that(dset1[,,], is_identical_to(testmat_n))    
      
  # Read columns / rows
  # TODO: drop should also be implemented
  expect_that(dset1[2,,, drop = FALSE], is_identical_to(testmat_n[2,,, drop = FALSE]))
  expect_that(dset1[2:3,,, drop = FALSE ], is_identical_to(testmat_n[2:3,,, drop = FALSE]))
  expect_that(dset1[,2,, drop = FALSE], is_identical_to(testmat_n[, 2,, drop = FALSE]))
  expect_that(dset1[, 2:3,, drop = FALSE], is_identical_to(testmat_n[, 2:3,, drop = FALSE]))
  expect_that(dset1[,,2, drop = FALSE], is_identical_to(testmat_n[,,2, drop = FALSE]))    
  expect_that(dset1[,,2:4, drop = FALSE], is_identical_to(testmat_n[,,2:4, drop = FALSE]))    
  
  testmat_n_read_all <- dset1[1:3, 1:3, 1:10]
  expect_that(testmat_n_read_all, is_identical_to(testmat_n))
  
  testmat_n_read_2 <- c(dset1[1, 2, 3], dset1[2, 3, 10]) 
  expect_that(testmat_n_read_2, is_identical_to(c(testmat_n[1, 2, 3], testmat_n[2, 3, 10])))
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2_read_all <- dset2[]
  testmat_n2 <- testmat_n
  testmat_n2[2:3, 2:3, 3:7] <- subarray
  expect_that(testmat_n2_read_all, is_identical_to(testmat_n2))
 
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("DataSet-Select-Subset-array-write",{  
  testmat_n <- array(as.integer(1:90), dim = c(3, 3, 10))
  subarray <- array(as.integer(-100:-120), dim = c(2, 2, 5))
  
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname, "a")
  group <- createGroup(file, "/testgroup")
  dset1 <- createDataSet(group, "testmat_n", testmat_n)
  dset2 <- createDataSet(group, "testmat_n2", testmat_n)
  f <- function() dset2[4, 3, 10] <- array(1:3, dim = c(1, 1, 1))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 4, 10] <- array(1:3, dim = c(1, 1, 1))
  expect_that(f(), throws_error("subscript out of bounds"))
  
  f <- function() dset2[3, 3, 11] <- array(1:3, dim = c(1, 1, 1))
  expect_that(f(), throws_error("subscript out of bounds"))

  f <- function() dset2[1, 1, 1, 1] <- testmat_n
  expect_that(f(), throws_error("incorrect number of dimensions"))
  
  # TODO: this should actually work!
  #f <- function() dset2[,,,] <- testmat_n
  #expect_that(f(), throws_error("incorrect number of dimensions"))
  
  # Write single element
  dset2[2, 2, 2] <- -2L 
  expect_that(dset2[2, 2, 2], is_identical_to(array(-2L, dim = c(1, 1, 1))))
  
  dset2[,,] <- dset1[,,]
  expect_that(dset2[,,], is_identical_to(dset1[,,]))
  
  # TODO: this should actually work!
  #dset2[] <- dset1[]
  #expect_that(dset2[], is_identical_to(dset1[]))
  
  # Assign submatrix to DataSet
  f <- function()  dset2[2:3, 2:3, 1:6] <- subarray
  expect_that(f(), throws_error("number of items to replace is not equal to replacement length"))
  
  dset2[2:3, 2:3, 1:5] <- subarray
  h5close(dset2)
  
  dset2 <- openDataSet(group, "testmat_n2")
  testmat_n2 <- testmat_n
  testmat_n2[2:3, 2:3, 1:5] <- subarray
  expect_that(readDataSet(dset2), is_identical_to(testmat_n2))
  
  # TODO: this should actually work
  # dset3[1, 1:2, ]
  
  dset3 <- createDataSet(group, "testmat_n3", testmat_n)
  dset3[, 1:2, 1:3] <- array(rep(1L, 18), dim = c(3, 2, 3))
  
  dset4 <- createDataSet(group, "testmat_n4", testmat_n)
  dset4[1:2,,1:3] <- array(rep(1L, 18), dim = c(3, 2, 3))
  
  h5close(dset4)
  h5close(dset3)
  h5close(dset1)
  h5close(dset2)
  h5close(group)
  h5close(file)
  
  file <- h5file(fname, "r")
  group <- openGroup(file, "/testgroup")
  dset1 <- openDataSet(group, "testmat_n")
  dset2 <- openDataSet(group, "testmat_n2")
  dset3 <- openDataSet(group, "testmat_n3")
  dset4 <- openDataSet(group, "testmat_n4")
  
  # Read entire dataset
  expect_that(readDataSet(dset1), is_identical_to(testmat_n))
  expect_that(dset1[], is_identical_to(testmat_n))
  expect_that(dset2[], is_identical_to(testmat_n2))
  testmat_n3 <- testmat_n
  testmat_n3[, 1:2, 1:3] <- 1L
  expect_that(dset3[], is_identical_to(testmat_n3))
  testmat_n4 <- testmat_n
  testmat_n4[1:2,,1:3] <- 1L
  expect_that(dset4[], is_identical_to(testmat_n4))
  
  h5close(dset4)
  h5close(dset3)
  h5close(dset2)
  h5close(dset1)
  h5close(group)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})
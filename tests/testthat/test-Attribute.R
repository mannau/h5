context("Attribute")

fname <- "test.h5"

testvec_i <- as.integer(1:90)
testmat_i <- matrix(testvec_i, ncol = 9)
testarray_i <- array(testvec_i, dim = c(3, 3, 10))

testvec_n <- as.numeric(1:90)
testmat_n <- matrix(testvec_n, ncol = 9)
testarray_n <- array(testvec_n, dim = c(3, 3, 10))

testvec_s <- paste0(LETTERS[1:90], LETTERS[seq(90, 1)])
testmat_s <- matrix(testvec_s, ncol = 9)
testarray_s <- array(testvec_s, dim = c(3, 3, 10))

testvec_l <- rep(c(TRUE, FALSE), 45)
testmat_l <- matrix(testvec_l, ncol = 9)
testarray_l <- array(testvec_l, dim = c(3, 3, 10))

testall <- list(
    testvec_i, testmat_i, testarray_i,
    testvec_n, testmat_n, testarray_n, 
    testvec_s, testmat_s, testarray_s
    #testvec_l, testmat_l, testarray_l
    )

test_that("Attribute-Errors", {   
  if(file.exists(fname)) file.remove(fname)
  file <- H5File(fname, "a")

  f <- function() h5attr(file, "test")
  expect_that(f(), throws_error("Opening Attribute failed"))    
  h5attr(file, "test") <- c("A", "BE", "BU")
  
  group <- file["testgroup"]
  f <- function() h5attr(group, "test")
  expect_that(f(), throws_error("Opening Attribute failed"))    
  h5attr(group, "test") <- c("A", "BE", "BU")
  
  group[, "testset"] <- 1:10
  dset <- group[, "testset"] 
  f <- function() h5attr(dset, "test")
  expect_that(f(), throws_error("Opening Attribute failed"))    
  h5attr(dset, "test") <- c("A", "BE", "BU")
  h5close(dset)
  h5close(group)
  h5close(file)
  
  file <- H5File(fname, "r")
  expect_that(h5attr(file, "test"), is_identical_to(c("A", "BE", "BU")))
  group <- file["testgroup"]
  expect_that(h5attr(group, "test"), is_identical_to(c("A", "BE", "BU")))
  dset <- group[, "testset"] 
  expect_that(h5attr(dset, "test"), is_identical_to(c("A", "BE", "BU")))
  h5close(dset)
  h5close(group)
  h5close(file)
  
})      
      
test_that("Attribute-H5Type-File", {      
  if(file.exists(fname)) file.remove(fname)
  file <- H5File(fname, "a")
  
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    h5attr(file, aname) <- testall[[i]]
  }
  
  group <- file["testgroup"]
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    h5attr(group, aname) <- testall[[i]]
  }
 
  file["testgroup", "dset"] <- 1:10
  dset <- file["testgroup", "dset"]
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    h5attr(dset, aname) <- testall[[i]]
  }
  h5close(group)
  h5close(dset)
  h5close(file) 
  
  file <- H5File(fname, "r")
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    expect_that(h5attr(file, aname), is_identical_to(testall[[i]]))
  }
  
  group <- file["testgroup"]
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    expect_that(h5attr(group, aname), is_identical_to(testall[[i]]))
  }
  
  dset <- group[, "dset"]
  for(i in 1:length(testall)) {
    aname <- sprintf("attribute_%02d", i)
    expect_that(h5attr(dset, aname), is_identical_to(testall[[i]]))
  }
  h5close(group)
  h5close(dset)
  h5close(file) 
})    
   
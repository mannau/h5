context("H5Group")
fname <- "test.h5"

test_that("H5Group-param",{
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname)
  
  group1 <- createGroup(file, "//testgroup")
  expect_that(group1, is_a("H5Group"))
  closeh5(group1)
  
  group2 <- createGroup(file, "testgroup2//")
  expect_that(group2, is_a("H5Group"))
  closeh5(group2)
  
  f <- function() grouproot <- createGroup(file, "/")
  expect_that(f(), throws_error("H5Gcreate failed"))
  
  f <- function() grouproot <- createGroup(file, "/")
  expect_that(f(), throws_error("H5Gcreate failed"))
   
      # Test very long groupname
  gname <- paste(rep(LETTERS, 1000), collapse = "")
  groupn <- createGroup(file, gname)
  expect_that(group1, is_a("H5Group"))
  closeh5(groupn)
  closeh5(file)
  file.remove(fname)
})

test_that("H5Group-createGroup",{
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname)
  
  file <- new( "H5File", fname)
  # Fail for nested (non-existent) group name
  f <- function() group1 <- createGroup(file, "/testgroup/test")
  expect_that(f(), throws_error("H5Gcreate failed"))
  
  group3 <- createGroup(file, "/testgroup3")
  expect_that(group3, is_a("H5Group"))
  expect_that(group3@name, is_identical_to("/testgroup3"))
  closeh5(group3)
  
  groupnested <- createGroup(file, "/testgroup3/test")
  expect_that(groupnested, is_a("H5Group"))
  expect_that(groupnested@name, is_identical_to("/testgroup3/test"))
  closeh5(groupnested)
  
  closeh5(file)
  file.remove(fname)
})

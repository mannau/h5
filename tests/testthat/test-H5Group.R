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
  #file.remove(fname)
})

test_that("H5Group-openGroup",{
  file <- new( "H5File", fname, "r")
  # Fail for nested (non-existent) group name
  f <- function() group1 <- openGroup(file, "/testgroup/test")
  expect_that(f(), throws_error("H5Gopen failed"))
  
  group3 <- openGroup(file, "/testgroup3")
  expect_that(group3, is_a("H5Group"))
  expect_that(group3@name, is_identical_to("/testgroup3"))
  closeh5(group3)
  
  groupnested <- openGroup(file, "/testgroup3/test")
  expect_that(groupnested, is_a("H5Group"))
  expect_that(groupnested@name, is_identical_to("/testgroup3/test"))
  closeh5(groupnested)
  
  group3 <- openGroup(file, "/testgroup3")
  grouprelative <- openGroup(group3, "test")
  expect_that(grouprelative, is_a("H5Group"))
  expect_that(grouprelative@name, is_identical_to("test"))
  # TODO: should absolute path be displayed?
  # eg. expect_that(grouprelative@name, is_identical_to("/testgroup3/test"))
  closeh5(grouprelative)
  
  closeh5(file)
  #file.remove(fname)
})

test_that("H5Group-existsGroup",{
  file <- new( "H5File", fname, "r")
  # Fail for nested (non-existent) group name
  expect_that(existsGroup(file, "/testgroup/test"), is_false())     
  expect_that(existsGroup(file, "/testgroup3"), is_true())
  expect_that(existsGroup(file, "/testgroup3/test"), is_true())
  
  group3 <- openGroup(file, "/testgroup3")
  expect_that(existsGroup(group3, "test"), is_true())
  closeh5(group3)
  closeh5(file)
})



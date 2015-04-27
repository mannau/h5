context("H5File")
fname <- "test.h5"

test_that("H5File-param",{
	f <- function() file <- new( "H5File", 1, "a")
	expect_that(f(), throws_error("Error : is.character.*", perl = TRUE))
	
	f <- function() file <- new( "H5File", c("a", "a"), "a")
	expect_that(f(), throws_error("Error : length.*", perl = TRUE))
	
	f <- function() file <- new( "H5File", "a", 1)
  expect_that(f(), throws_error("Error : is.character.*", perl = TRUE))
	
	f <- function() file <- new( "H5File", "a", c("a", "a"))
	expect_that(f(), throws_error("Error : length.*", perl = TRUE))
	
	f <- function() file <- new( "H5File", "a", c("a", "a"))
  expect_that(f(), throws_error("Error : length.*", perl = TRUE))
	
	f <- function() file <- new( "H5File", "path", "w--")
	expect_that(f(), throws_error("Parameter mode must be either"))
})

test_that("H5File-FileMode-param",{
	f <- function() file <- new( "H5File", "path", "w--")
	expect_that(f(), throws_error("Parameter mode must be either"))
})	

test_that("H5File-FileMode-param-a",{
	if(file.exists(fname)) file.remove(fname)
	file <- new( "H5File", fname)
	expect_that(file, is_a("H5File"))
	expect_that(file@mode, is_identical_to("a"))
#  expect_that(file@location, is_identical_to(file.path(getwd(), fname)))
	expect_that(basename(file@location), is_identical_to(fname))
	group1 <- createGroup(file, "testgroup")
	expect_that(group1, is_a("H5Group"))
	h5close(group1)
	h5close(file)
	
	# Open existing file for append
	expect_that(file.exists(fname), is_true())
	file <- new( "H5File", fname, "a")
	expect_that(file, is_a("H5File"))
	existsGroup(file, "testgroup")
	group2 <- createGroup(file, "testgroup2")
	h5close(group2)
	h5close(file)
})

test_that("H5File-FileMode-param-w-",{
	expect_that(file.exists(fname), is_true())
	f <- function() file <- new( "H5File", fname, "w-")
	expect_that(f(), throws_error("H5Fcreate failed"))
	expect_that(file.remove(fname), is_true())
	
	file <- new( "H5File", fname, "w-")
	expect_that(file@mode, is_identical_to("w-"))
	expect_that(basename(file@location), is_identical_to(fname))
	group1 <- createGroup(file, "testgroup1")
	expect_that(existsGroup(file, "testgroup1"), is_true())
	h5close(group1)
	h5close(file)
})

test_that("H5File-FileMode-param-w",{
  expect_that(file.exists(fname), is_true())
  file <- new( "H5File", fname, "w")
  expect_that(file@mode, is_identical_to("w"))
  expect_that(basename(file@location), is_identical_to(fname))
  expect_that(existsGroup(file, "testgroup1"), is_false())
  group2 <- createGroup(file, "testgroup1")
  h5close(group2)
  h5close(file)
	
  expect_that(file.exists(fname), is_true())
  file <- new( "H5File", fname, "w")
  expect_that(file@mode, is_identical_to("w"))
  expect_that(basename(file@location), is_identical_to(fname))
  expect_that(existsGroup(file, "testgroup1"), is_false())
  h5close(file)
  #expect_that(file.remove(fname), is_true())		
})
			
test_that("H5File-FileMode-param-r",{
	expect_that(file.exists(fname), is_true())
  file <- new( "H5File", fname, "r")
  expect_that(file@mode, is_identical_to("r"))
  expect_that(basename(file@location), is_identical_to(fname))
	#expect_that(existsGroup(file, "testgroup1"), is_true())
	h5close(file)
  
  file.remove(fname)
  expect_that(file.exists(fname), is_false())
  f <- function() file <- new( "H5File", fname, "r")
  expect_that(f(), throws_error("H5Fopen failed"))
})

test_that("H5File-FileMode-param-r+",{
  if(file.exists(fname)) file.remove(fname)
  f <- function() file <- new( "H5File", fname, "r+")
  expect_that(f(), throws_error("H5Fopen failed"))
  
  file <- new( "H5File", fname, "a")
  group1 <- createGroup(file, "testgroup")
  expect_that(group1, is_a("H5Group"))
  h5close(group1)
  h5close(file)
  
  file <- new( "H5File", fname, "r+")
  expect_that(existsGroup(file, "testgroup"), is_true())
  expect_that(group1, is_a("H5Group"))
  h5close(group1)
  h5close(file)
  file.remove(fname) 
})


test_that("H5File-show",{
  if(file.exists(fname)) file.remove(fname)
  file <- new( "H5File", fname)
  group1 <- createGroup(file, "testgroup")
  group2 <- createGroup(file, "testgroup2")
  group3 <- createGroup(group2, "testgroup3")
  h5close(group1)
  h5close(group2)
  h5close(group3)
  file
  h5close(file)
})

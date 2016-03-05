context("H5File")
fname <- "test.h5"

test_that("H5File-param",{
	f <- function() file <- h5file(1, "a")
	expect_that(f(), throws_error("is.character\\(name\\) is not TRUE"))
	
	f <- function() file <- h5file(c("a", "a"), "a")
	expect_that(f(), throws_error("length\\(name\\) == 1 is not TRUE"))
	
	f <- function() file <- h5file("a", 1)
  expect_that(f(), throws_error("is.character\\(mode\\) is not TRUE"))
	
	f <- function() file <- h5file("a", c("a", "a"))
	expect_that(f(), throws_error("length\\(mode\\) == 1 is not TRUE"))
	
	f <- function() file <- h5file("a", c("a", "a"))
  expect_that(f(), throws_error("length\\(mode\\) == 1 is not TRUE"))
	
	f <- function() file <- h5file("path", "w--")
	expect_that(f(), throws_error("Parameter mode must be either.*"))
})

test_that("H5File-FileMode-param",{
	f <- function() file <- h5file("path", "w--")
	expect_that(f(), throws_error("Parameter mode must be either"))
})	

test_that("H5File-FileMode-param-a",{
	if(file.exists(fname)) file.remove(fname)
	file <- h5file(fname)
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
	file <- h5file(fname, "a")
	expect_that(file, is_a("H5File"))
	existsGroup(file, "testgroup")
	group2 <- createGroup(file, "testgroup2")
	h5close(group2)
	h5close(file)
})

test_that("H5File-FileMode-param-w-",{
	expect_that(file.exists(fname), is_true())
	f <- function() file <- h5file(fname, "w-")
	expect_that(f(), throws_error("H5Fcreate failed"))
	expect_that(file.remove(fname), is_true())
	
	file <- h5file(fname, "w-")
	expect_that(file@mode, is_identical_to("w-"))
	expect_that(basename(file@location), is_identical_to(fname))
	group1 <- createGroup(file, "testgroup1")
	expect_that(existsGroup(file, "testgroup1"), is_true())
	h5close(group1)
	h5close(file)
})

test_that("H5File-FileMode-param-w",{
  expect_that(file.exists(fname), is_true())
  # Seems HDF5 since 1.8.15 does not detect that file does exist but is already 
  # closed, see also https://www.hdfgroup.org/HDF5/doc/RM/RM_H5F.html
  # We therefore need to delete file
  expect_that(file.remove(fname), is_true())
      
  file <- h5file(fname, "w")
  expect_that(file@mode, is_identical_to("w"))
  expect_that(basename(file@location), is_identical_to(fname))
  expect_that(existsGroup(file, "testgroup1"), is_false())
  group2 <- createGroup(file, "testgroup1")
  h5close(group2)
  h5close(file)
	
  expect_that(file.exists(fname), is_true())
  # See above
  expect_that(file.remove(fname), is_true())
  
  file <- h5file(fname, "w")
  expect_that(file@mode, is_identical_to("w"))
  expect_that(basename(file@location), is_identical_to(fname))
  expect_that(existsGroup(file, "testgroup1"), is_false())
  h5close(file)
  
})
			
test_that("H5File-FileMode-param-r",{
	expect_that(file.exists(fname), is_true())
  file <- h5file(fname, "r")
  expect_that(file@mode, is_identical_to("r"))
  expect_that(basename(file@location), is_identical_to(fname))
	#expect_that(existsGroup(file, "testgroup1"), is_true())
	h5close(file)
  
  expect_that(file.remove(fname), is_true())		
  f <- function() file <- h5file(fname, "r")
  expect_that(f(), throws_error("H5Fopen failed"))
})

test_that("H5File-FileMode-param-r+",{
  if(file.exists(fname)) file.remove(fname)
  f <- function() file <- h5file(fname, "r+")
  expect_that(f(), throws_error("H5Fopen failed"))
  
  file <- h5file(fname, "a")
  group1 <- createGroup(file, "testgroup")
  expect_that(group1, is_a("H5Group"))
  h5close(group1)
  h5close(file)
  
  file <- h5file(fname, "r+")
  expect_that(existsGroup(file, "testgroup"), is_true())
  expect_that(group1, is_a("H5Group"))
  h5close(group1)
  h5close(file)
  expect_that(file.remove(fname), is_true())
})


test_that("H5File-show",{
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname)
  group1 <- createGroup(file, "testgroup")
  group2 <- createGroup(file, "testgroup2")
  group3 <- createGroup(group2, "testgroup3")
  h5close(group1)
  h5close(group2)
  h5close(group3)
  file
  h5close(file)
  expect_that(file.remove(fname), is_true())
})

test_that("H5File-is-h5file",{
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname)
  group1 <- createGroup(file, "testgroup")
  h5close(group1)
  h5close(file)
  
  expect_that(is.h5file(fname), is_true())
  expect_warning(expect_that(is.h5file("abc"), is_false()))
  fnametxt <- "test.txt"
  writeLines("abc", fnametxt)
  expect_that(is.h5file(fnametxt), is_false())
  expect_that(file.remove(fname), is_true())
  expect_that(file.remove(fnametxt), is_true())
})

test_that("H5File-flush",{
  if(file.exists(fname)) file.remove(fname)
  file <- h5file(fname)
  file["testgroup/testset"] <- 1:100
  expect_that(h5flush(file), is_true())
  h5close(file)
})


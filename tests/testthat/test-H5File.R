context("H5File")

test_that("H5File-InputParamsExceptions",{
	fname <- "test.h5"

	f <- function() file <- new( "H5File", 1, "a")
	expect_that(f(), throws_error("is.character\\(filePath\\) is not TRUE"))
	
	f <- function() file <- new( "H5File", c("a", "a"), "a")
	expect_that(f(), throws_error("length\\(filePath\\) == 1 is not TRUE"))
	
	f <- function() file <- new( "H5File", "a", 1)
	expect_that(f(), throws_error("is.character\\(mode\\) is not TRUE"))
	
	f <- function() file <- new( "H5File", "a", c("a", "a"))
	expect_that(f(), throws_error("length\\(mode\\) == 1 is not TRUE"))
	
	f <- function() file <- new( "H5File", "a", c("a", "a"))
	expect_that(f(), throws_error("length\\(mode\\) == 1 is not TRUE"))
})

test_that("H5File-FileMode",{
			f <- function() file <- new( "H5File", "path", "w--")
			expect_that(f(), throws_error("Parameter mode must be either"))
			
			fname <- "test.h5"
			if(file.exists(fname)) file.remove(fname)
			file <- new( "H5File", fname)
			expect_that(file, is_a("H5File"))
			expect_that(file@mode, is_identical_to("a"))
			expect_that(file@filePath, is_identical_to(fname))
			closeh5(file)
			expect_that(file.exists(fname), is_true())
			
			# Open existing file for append
			file <- new( "H5File", fname, "a")
			expect_that(file, is_a("H5File"))
			closeh5(file)
			expect_that(file.exists(fname), is_true())
			
			f <- function() file <- new( "H5File", fname, "w-")
			expect_that(f(), throws_error("H5Fcreate failed"))
			file.remove(fname)
			
			file <- new( "H5File", fname, "w-")
			expect_that(file@mode, is_identical_to("w-"))
			expect_that(file@filePath, is_identical_to(fname))
			closeh5(file)
			expect_that(file.exists(fname), is_true())
			
			
			
			
			
			
			
			
})



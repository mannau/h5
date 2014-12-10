context("datatypes-IO")

test_that("datatypes-IO",{
	
	testmat <- matrix(rnorm(120), ncol = 3)
	fname <- "test.h5"
	if(file.exists(fname)) file.remove(fname)
	dsetname <- c("testmat1", "testmat2")
	
	file <- new( "H5File", fname, "a")
	group <- createGroup(file, "/testgroup")
	dset <- createDataSet(group, dsetname[1], testmat)
	closeh5(dset)
	closeh5(group)
	closeh5(file)
	
	file <- new( "H5File", fname, "r")
	group <- openGroup(file, "/testgroup")
	dset <- openDataSet(group, dsetname[1], "double")
	outmat <- readDataSet(dset)
	expect_that(testmat, is_identical_to(outmat))
	closeh5(dset)
	closeh5(group)
	closeh5(file)
	
	file <- new( "H5File", fname, "a")
	group2 <- createGroup(file, "/testgroup/testgroup2")
	dset2 <- createDataSet(group2, dsetname[2], testmat)
	outmat2 <- readDataSet(dset2)
	expect_that(testmat, is_identical_to(outmat2))
	closeh5(dset2)
	closeh5(group2)
	closeh5(file)
	
	file2 <- new( "H5File", fname, "r")
	group3 <- openGroup(file2, "/testgroup/testgroup2")
	dset3 <- openDataSet(group3, dsetname[2], "double") 
	outmat3 <- readDataSet(dset3)
	expect_that(testmat, is_identical_to(outmat3))
	closeh5(dset3)
	closeh5(group3)
	closeh5(file2)
	
	file.remove(fname)
})


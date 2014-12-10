context("datatypes-Vector")

test_that("datatypes-Vector",{
	
	testmat_n <- rnorm(120)
	testmat_i <- as.integer(runif(120)*10000)
	testmat_l <- as.logical(round(runif(120)))
	testmat_l <- as.logical(round(runif(120)))
	testmat_c <-rep(paste0(LETTERS[1:3], rev(LETTERS)[1:3]), 120/3)
	testmat_c[1] <- paste0(testmat_c[1], testmat_c[1])
	testmat_c[40] <- paste0(testmat_c[1], testmat_c[1])
	
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
	# TODO: does not work for variable string length
	testmat_c_read <- readDataSet(dset14)
	closeh5(dset14)
	expect_that(testmat_c, is_identical_to(testmat_c_read))
	closeh5(group)
	closeh5(file)		
	file.remove(fname)
})


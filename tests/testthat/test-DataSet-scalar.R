context("DataSet-scalar")

fname <- "test.h5"

test_that("datatypes-Array-BugWithScalar",{
      fname <- system.file("test-scalar.h5", package = "h5", mustWork = TRUE)
      f <- h5file(fname, "r")
      dset <- f["/Analyses/Basecall_2D_000/BaseCalled_2D/Fastq"]
      dat <- dset[]
      expect_identical(length(dat), 1L)
      expect_is(dat, "character")
      h5close(f)
    })

context("DataSet-Enum")

test_that("DataSet-Enum-Matrix-open", {
    # Use the sample file for the enum datatype from the HDF Group itself,
    # see also https://www.hdfgroup.org/ftp/HDF5/examples/files/exbyapi
    fname <- system.file("h5ex_t_enum.h5", package = "h5", mustWork = TRUE)
    datafile <- h5file(fname, "r")
    dataset <- datafile["DS1"][]
    expect_that(is.factor(dataset), is_true())
    expect_that(levels(dataset), equals(c("SOLID", "LIQUID", "GAS", "PLASMA")))
    h5close(datafile)
})


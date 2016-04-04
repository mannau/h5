context("DataSet-Enum-Vector")

# Use the sample file for the enum datatype from the HDF Group itself.
fname <- "h5ex_t_enum.h5"
download.file("https://www.hdfgroup.org/ftp/HDF5/examples/files/exbyapi/h5ex_t_enum.h5",
              fname, quiet = TRUE)

test_that("DataSet-Enum-Vector-open", {
    datafile <- h5file(fname, "r")
    dataset <- datafile["DS1"][]

    expect_that(is.factor(dataset), is_true())
    expect_that(levels(dataset), equals(c("SOLID", "LIQUID", "GAS", "PLASMA")))

    h5close(datafile)
})

file.remove(fname)

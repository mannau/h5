```r
library(h5)
testvec <- rnorm(10)
testmat <- matrix(1:9, nrow = 3)
row.names(testmat) <- 1:3
colnames(testmat) <- c("A", "BE", "BU")
letters1 <- paste(LETTERS[runif(45, min = 1, max = length(LETTERS))])
letters2 <- paste(LETTERS[runif(45, min = 1, max = length(LETTERS))])
testarray <- array(paste0(letters1, letters2), c(3, 3, 5))

file <- h5file("test.h5")
# Save testvec in group 'test' as DataSet 'testvec'
file["test", "testvec"] <- testvec
file["test", "testmat"] <- testmat
file["test", "testarray"] <- testarray
h5close(file)
```

We can now retrieve the data from the file


```r
file <- H5File("test.h5")
dataset_testmat <- file["test", "testmat"]
# We can now retrieve all data from the DataSet object using e.g. the  subsetting operator
dataset_testmat[]
```

```
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    2    5    8
## [3,]    3    6    9
```

```r
# We can also subset the data directly, e.g. row 1 and 3
dataset_testmat[c(1, 3), ]
```

```
##      [,1] [,2] [,3]
## [1,]    1    4    7
## [2,]    3    6    9
```

Note, that we have now lost the row- and column names associated with the *testmat* object
in the retrieved matrix. HDF5 supports metadata with attributes, which we need to
add to (retrieve from) the DataSet manually.


```r
h5attr(dataset_testmat, "rownames") <- row.names(testmat)
h5attr(dataset_testmat, "colnames") <- colnames(testmat)
```

We can now retrieve our matrix including meta-data as follows:


```r
outmat <- dataset_testmat[]
row.names(outmat) <- h5attr(dataset_testmat, "rownames")
colnames(outmat) <- h5attr(dataset_testmat, "colnames")
identical(outmat, testmat)
```

```
## [1] TRUE
```

Do not forget to close the HDF5 file in the end


```r
h5close(file)
```
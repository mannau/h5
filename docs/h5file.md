# H5File - HDF5 File Objects

*H5File* objects are are the main entry point to access HDF5 data from binary files.
The *H5File* S4 class directly maps [H5File](https://www.hdfgroup.org/HDF5/doc/cpplus_RM/class_h5_1_1_h5_file.html) objects from the C++ API to R. Through the implemented class hierarchy it shares common functionality with *H5Group*. 

HDF5 Files can contain the following objects:
- Groups: Similar to a file system folder, used to organize HDF5 objects in a
  hierarchical way.
- Datasets: Objects to store actual data.
- Attributes: Meta data objects to store extra informatino about Files, Groups and Datasets.

## Reading and Writing Files
HDF5 files can be created and accessed using `h5file()`:
```
file <- h5file(name = "test.h5", mode = "a")
```

The following access-modes are defined:

| Mode |                    Description                    |
|:----:|---------------------------------------------------|
|   **a**  | Read/write if exists, create otherwise (default). |
|   **r**  | Read only, file must exist.                       |
|  **r+**  | Read/write, file must exist.                      |
|   **w**  | Create file, truncate if exists.                  |
|  **w-**  | Create file, fail if exists.                      |

After *H5File* files have been processed, they need to be closed using `h5close()`:
```
h5close(file)
```

## Show File Contents

HDF5 objects stored in a file are shown with the following symbols:

|Symbol|    Description        |
|:----:|-----------------------|
|**+** | HDF5 Group            |
|**D** | HDF5 Dataset          |
|**A** | HDF5 Attribute        |

The following examples generates a HDF5 file with the different HDF5 Objects and shows its contents:

```
file <- h5file(name = "test.h5", mode = "a")
file["testdataset"] <- 1:10
h5attr(file, "testattrib") <- LETTERS[1:10]
file["testgroup/testdataset2"] <- 1:10
file
```

## Extract File Contents

The following functions are defined to extract HDF5 file contents:
- *list.groups*: List HDF5 groups in file.
- *list.datasets*: List HDF5 datasets in file.
- *list.attributes*: List Attributes of HDF5 object (file, group or dataset).

The following example shows hdf5 file contents and how to use them to iterate over HDF5 elements:

```
file <- h5file(name = "test.h5", mode = "a")
test["testgroup1/testset1"] <- 1:10
test["testgroup2/testset2"] <- 11:20
test["testgroup3/testset3"] <- 21:30

# Extract first 3 elements from each dataset and combine result to matrix
sapply(list.datasets(test, recursive = TRUE), function(x) test[x][1:3])

# Add new dataset to each group in HDF5 file
for(g in list.groups(test)) {
  test[paste(g, "testsetx", collapse = "/")] <- 1:10
}
list.datasets(test, recursive = TRUE)
```












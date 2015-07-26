# H5File - HDF5 File Objects

*H5File* objects are are the main entry point to access HDF5 data from binary files.
The *H5File* S4 class and directly maps [H5File](https://www.hdfgroup.org/HDF5/doc/cpplus_RM/class_h5_1_1_h5_file.html) objects from the C++ API to R. Through the implemented class hierarchy it shares common functionality with *H5Group*. 

## Reading and Writing Files
HDF5 files can be accessed using `h5file()`:
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
```
file <- h5file(name = "test.h5", mode = "a")
file["testdataset"] <- 1:10
h5attr(file, "testattrib") <- LETTERS[1:10]
file["testgroup/testdataset2"] <- 1:10
file
```



## Extract File Contents
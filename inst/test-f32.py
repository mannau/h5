import h5py
import numpy

import h5py
from numpy import array

f = h5py.File('test-h5link.h5', 'w')
grp = f.create_group("testgroup")
grp["test"] = array([1, 2, 3])
subgrp = grp.create_group("subgroup")
grp["test-sub"] = array([4, 5, 6])

# Create Hardlink
grp2 = f.create_group("hardlink")
grp2["test2"] = grp["test"]

# Create Softlink
grp3 = f.create_group("softlink")
grp3["test3"] = h5py.SoftLink('/testgroup')

# Create External link
grp4 = f.create_group("extlink")
grp4["test4"] = h5py.ExternalLink("test-f32.h5", "/")
f.close() h5py.File('test-f32.h5', 'w') as f:
    dset = f.create_dataset("floats", (3,), dtype='>f4')
    dset[:] = [1, 2, 3]
    dset.attrs["scalar"] = numpy.string_("Hello")

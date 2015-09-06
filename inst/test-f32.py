import h5py
import numpy

with h5py.File('test-f32.h5', 'w') as f:
    dset = f.create_dataset("floats", (3,), dtype='>f4')
    dset[:] = [1, 2, 3]
    dset.attrs["scalar"] = numpy.string_("Hello")

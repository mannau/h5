import random, string
import numpy as np
import h5py

def random_string(n=10):
    return ''.join(random.SystemRandom().choice(string.digits + string.ascii_uppercase) 
                   for _ in range(n))

fn = "test-ascii-length-bug.h5"


test = h5py.File(fn, "w")
standard_ascii = "mar231-21y ha131d a litt321le lamb its Fleece As Wh31ite as snow".split()
random_strings = [random_string() for _ in range(20)]
codedb = np.array(standard_ascii, dtype="a10")
test.create_dataset("/test/ascii", data=codedb)
codedc = np.array(random_strings, dtype="a11")
test.create_dataset("/test/randomalpha", data=codedc)
codedd = np.array(random_strings, dtype="a10")
test.create_dataset("/test/randomalphashort", data=codedd)
test.close()

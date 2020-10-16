#! /usr/bin/env python

from __future__ import print_function, division, absolute_import
import numpy as np
import sys
import time

from fortran_source.binning import mbinning


# test function
print(mbinning.test(3,5))


# fortran ordering is important
# dont know if one can avoid this
A = np.ones((5000,5000), dtype=np.float64, order='F')


vec1 = np.array([1000,], order='F', dtype=np.int)
vec2 = np.array([1500,3000], order='F', dtype=np.int)

B = np.zeros((vec1.shape[0]+1, vec2.shape[0]+1), dtype=np.float64, order='F')

mbinning.binmatrix(A,vec1,vec2,B)

print(A)
print(B)

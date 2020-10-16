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
vec1 = np.array([0,1000,4000], order='F', dtype=np.int)
vec2 = np.array([2000,3000,0], order='F', dtype=np.int)
B = np.zeros((vec1.shape[0], vec2.shape[0]), dtype=np.float64, order='F')
mbinning.binmatrix_r(A,vec1,vec2,B)
print('matrix: ', A)
print('binned: ', B)



A = np.ones((5000,5000), dtype=np.complex128, order='F')
vec1 = np.array([0,1000,4000], order='F', dtype=np.int)
vec2 = np.array([2000,3000,0], order='F', dtype=np.int)
B = np.zeros((vec1.shape[0], vec2.shape[0]), dtype=np.complex128, order='F')
mbinning.binmatrix_c(A,vec1,vec2,B)
print('matrix: ', A)
print('binned: ', B)

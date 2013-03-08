#! /usr/bin/python
from __future__ import division, print_function
import sys
import argparse
import math
import random
import numpy

parser = argparse.ArgumentParser()
parser.add_argument("dimension", help="the dimension of the generated data", type=int)
parser.add_argument("examples", help="the number of generated examples", type=int)
parser.add_argument("-w" , "--wcoeff", help="coefficient to multiply the generated logistic parameter w by",
  type=float, default=10.0)
parser.add_argument("-b" , "--bcoeff", help="coefficient to multiply the generated logistic parameter b by",
  type=float, default=0.1)
parser.add_argument("-c", "--chunk", help="size of chunks to compute at once",
  type=int, default=512*1024)

def main():
  args = parser.parse_args()
  # select model parameters at random
  b = -args.bcoeff * numpy.random.randn() / math.sqrt(args.dimension)
  w = args.wcoeff * numpy.random.randn(args.dimension, 1) / math.sqrt(args.dimension)
  # generate data points
  remaining_examples = args.examples
  while remaining_examples > 0:
    cur_examples = min(remaining_examples, args.chunk)
    data = numpy.random.randn(cur_examples, args.dimension) / math.sqrt(args.dimension)
    u = data.dot(w) + b
    p = 1.0 / (numpy.exp(-u) + 1.0)
    r = numpy.random.rand(cur_examples, 1)
    y = (p >= r)
    for i in range(cur_examples):
      print(("1.0" if y[i] else "0.0") + ' ' + ' '.join(str(v) for v in data[i, :]))
    remaining_examples -= cur_examples


if __name__ == "__main__":
  main()

OptiCVX
=======

OptiCVX is a Delite DSL for convex optimization.

To Do List:
  - Allow for constant multiplication, multiplication by matrices, and other arbitrary operations on vectors or matrices that the user marks as affine
  - Fix the issue where all constants need to be wrapped, which is syntactically awkward
  - Figure out how to make looping IR nodes, so that I can implement arbitrary-length constraints
  - Implement a basic solver
  - Rework the VectorOps to be based off of optila or some other appropriate package


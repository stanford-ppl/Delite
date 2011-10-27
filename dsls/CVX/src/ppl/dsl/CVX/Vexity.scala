package ppl.dsl.CVX

// traits for convexity analysis
trait Convex
trait Concave
// affine expressions are both convex and concave
// this allows us to define behavior on convex and concave functions
// and have affine functions properly handled without a separate case
trait Affine extends Convex with Concave

// anything that is none of the above has no attached traits
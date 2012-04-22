DeLiszt
==========

1.  Build the `deliszt` branch of `virtualization-lms-core` and publish locally
2.  Build the `lisztlib` library under `dsls/deliszt/lisztlib`. Run `cmake` to generate the `Makefile` then run `make`
3.  Build `Delite`
4.  Build `runtime` and make sure to run `sbt update`
5.  Set your `LD_LIBRARY_PATH` to `dsls/deliszt/lisztlib/lib`
# vector-sum-benchmarks

This package holds micro-benchmarks for various implementations of a
basic function `vsum :: Vector v Double => NonEmpty (v Double) -> v
Double` which adds elements of vectors elements-wise.

The idea is to provide multiple possible implementations for this
function and check which one performs the best for some use-cases.

To define new implementation, simply add a module in
`src/Benchmarking/VectorSum`. Remember to add your implementation in
tests (so that we can check it's consistent with others) and
benchmarks, both of which should be a couple of lines addition.

Benchmarks are ran for all three immutable vector types defined in the
`vector` package, for various input sizes. Note that it will take a
fairly long time to run default benchmarks for every defined size. I'd
recommend picking a size you want instead with an argument to
criterion:

```bash
stack bench vector-sum-benchmarks:bench:vector-sum-benchmarks-bench
--benchmark-arguments='-m pattern 1000000x1'
```

Allocation numbers run fairly quickly so that benchmark should work
out of the box without tweaking.

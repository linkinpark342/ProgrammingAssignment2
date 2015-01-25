source("cachematrix.R")

assert.cached <- function(m, v) stopifnot(identical(m$get.cached.transposed(), v))
assert.uncached <- function(m) assert.cached(m, NULL)

# test with null matrix
m <- makeCacheMatrix(x = matrix())

stopifnot(identical(m$get(), matrix()))
assert.uncached(m)

stopifnot(identical(cacheSolve(m), matrix()))
assert.cached(m, matrix())
stopifnot(identical(m$get(), matrix()))

# Test with actual matrix
mat <- matrix(c(1, 2, 3, 4), 2, 2)
m <- makeCacheMatrix(mat)
stopifnot(identical(m$get(), mat))
assert.uncached(m)
stopifnot(identical(cacheSolve(m), t(mat)))
stopifnot(identical(m$get(), mat))
assert.cached(m, t(mat))

# Ensure calling set() resets the cache
mat <- matrix(c(1, 2, 3, 4), 4, 1)
m$set(mat)
stopifnot(identical(m$get(), mat))
assert.uncached(m)
## Put comments here that give an overall description of what your
## functions do

# Returns a container that can hold a matrix, and some cached data computed
# from the matrix
makeCacheMatrix <- function(x = matrix()) {
  cached.transposed <- NULL
  
  # Update matrices
  set <- function(y) {
    x <<- y
    cached.transposed <<- NULL
  }
  get <- function() x
  
  # Transposed
  set.cached.transposed <- function(transposed) {
    cached.transposed <<- transposed
  }
  get.cached.transposed <- function() cached.transposed
  
  list(set = set,
       get = get,
       set.cached.transposed = set.cached.transposed,
       get.cached.transposed = get.cached.transposed)
  
}


## Returns a transposed version of the matrix contained in x
#
# This function returns a transposed version of the matrix that is
# stored in x, and will cache it for subsequent calls
cacheSolve <- function(x, ...) {
  transposed <- x$get.cached.transposed()
  if (!is.null(transposed)) {
    message("returning cached transposed matrix")
    return(transposed)
  }
  
  transposed <- t(x$get(), ...)
  x$set.cached.transposed(transposed)
  transposed
}
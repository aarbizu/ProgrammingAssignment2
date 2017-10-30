## Implement cachable matrix, which stores inverse of a given matrix
## to avoid repeated computation of that inverse. Provide a method
## to obtain a matrix which makes use of the cached matrix, if available.

## Matrix, augmented with a cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setsolved <- function(inverse) {
    inv <<- inverse
  }
  getsolved <- function() {
    inv
  }
  list(
    set = set,
    get = get,
    setsolved = setsolved,
    getsolved = getsolved
  )
}


## Compute inverse of the given matrix, with caching of previous
## values for the same given matrix

cacheSolve <- function(x, ...) {
  i <- x$getsolved()
  
  ## don't recompute the value of solve(), return cached value
  if (!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  
  mat <- x$get()
  solved <- solve(mat, ...)
  x$setsolved(solved)
  solved
}

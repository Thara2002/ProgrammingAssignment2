## Matrix inversion is usually a costly computation T
##There may be some benefit to caching the inverse of a matrix than computing it repeatedly. 
## The following pairs of functions helps to cache the inverse of a matrix

## The following function creates an object of the class matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invse <- NULL
  set <- function(y) {
    x <<- y
    invse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invse <<- inverse
  getInverse <- function() invse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invse <- x$getInverse()
    if (!is.null(invse)) {
      message("getting cached data")
      return(invse)
    }
    matrx <- x$get()
    invse <- solve(matrx, ...)
    x$setInverse(invse)
    invse
}


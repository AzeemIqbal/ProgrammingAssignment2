## Script to store the inverse of a matrix in cache.
## Assumes that the matrix is always square and invertible

## Creates an object to store a matrix and its
## inverse in cache. Initially sets inverse to
## NULL and stores any updated inverse calculated
## using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}


## Calculates the inverse of a matrix, initially checks to
## see if the inverse has already been calculated, if so
## returns it from cache, otherwise computes the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}


## The functions below computes cache of each newly created inverse matrix.
## When the user intends to get inverse of the matrix that has been cached, it returns the cache. Otherwise computes new inverse.

## Creates special matrix object
makeCacheMatrix <- function(x = matrix()) {
  matrix.inverse <- NULL
  
  set <- function(y) {
    x <<- y
    matrix.inverse <<- NULL
  }
  get <- function() x
  
  setMatrixInverse <- function(inv) matrix.inverse <<- inv
  getMatrixInverse <- function() matrix.inverse
  
  list(set = set,
       get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Creates inverse of special matrix object returned by makeCacheMatrix function
cacheSolve <- function(x, ...) {
  matrix.inverse <- x$getMatrixInverse()
  if (!is.null(matrix.inverse)) {
    message("getting cached matrix inverse")
    return(matrix.inverse)
  }
  matrix <- x$get()
  matrix.inverse <- solve(matrix, ...)
  x$setMatrixInverse(matrix.inverse)
  matrix.inverse
}
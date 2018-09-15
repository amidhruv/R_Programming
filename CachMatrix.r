## It computes a special "matrix".

## which contains the following function- To set the value of matrix, to get the value of the matrix, set the value of inverse of matrix,
# get the inverse of matrix
 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function calculates the inverse of special "matrix".It first see whether the inverse is calculated or not.
# and if it is calculated it gets the inverse from cache and skips the computation.Otherwise it calculates the inverse 
# of the matrix and sets the value in cache via setinverse function. Then it returns the matrix with inverse of X.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
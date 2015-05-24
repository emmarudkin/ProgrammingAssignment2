# The following pair of functions will compute and cache the inverse of a matrix.



# This function creates a special "matrix" object that can cache its inverse by setting the matrix, 
# getting the matrix, setting the inverse of the matrix and getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) inverse <<- inv
  getinv <- function() return(inverse)
  return(list(set = set, get = get,
              setinv = setinv,
              getinv = getinv))
}




# This function calculates the inverse of the special "matrix" created with the above function 'makeCacheMatrix'.
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation.


cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  invserse <- solve(data, ...)
  x$setinv(inverse)
  return(inverse)
}

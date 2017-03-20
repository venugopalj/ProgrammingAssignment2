## makeCacheMatrix provides a way to cache the inverse of a given matrix. cacheSolve provides a way to check
#whether the inverse of a matrix is available, and if so returns that inverse. Else, it calculates the inverse
#caches it for future use and returns the inverse

# makeCacheMatrix creates a special vector which is a list containing four functions that:
# set the value of a matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve gets the inverse of a matrix x. If the inverse is available in the cache, then
# the cached value is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv (inv)
  inv
}

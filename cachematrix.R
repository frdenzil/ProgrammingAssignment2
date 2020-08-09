## Put comments here that give an overall description of what your
## functions do

## This function caches the matrix x. Thre are four functions
## set, get, set_inverse and get_inverse. The function returns a list of functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)     
        
}


## CacheSolve function calculates the INVERSE of cached matrix x by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv             
        
}

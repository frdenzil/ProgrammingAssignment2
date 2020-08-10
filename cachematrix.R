## Put comments here that give an overall description of what your
## Program to calculate the inverse of a matrix
## by cacheing the matrix using the function makeCacheMatric()
## The matrix returns a list of four functions.

## 

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


## -finding the inverse of the matrix x

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

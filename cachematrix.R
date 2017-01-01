## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix  creates a list representing a matrix whose inverse
##   is cached.

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  
  set <- function(y) {
    x <<- y
    xInverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) xInverse <<- inv
  getInverse <- function() xInverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}




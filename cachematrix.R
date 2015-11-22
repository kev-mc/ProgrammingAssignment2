## Mechanism to cache the inverse of a matrix

## Create a list that acts as a wrapper object to cache the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # initial value of the inverse is null
  inv <- NULL
  
  # resetting the matrix needs to also reset the inverse to its inital (NULL) value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # basic get function for the original matrix
  get <- function() x
  
  # getter/setter functions for the cached inverse value
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  
  # create (and return) the list that exposes the getter and setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Find the inverse of a matrix, returning a cached value if available, otherwise calculating and caching the inverse
cacheSolve <- function(x, ...) {
  # try and retreive the cahched value
  i <- x$getinverse()
  
  if(!is.null(i)) {
    # the inverse has already been calculated, so return it
    return(i)
  }
  
  # retrieve the underlying matrix from the list
  mtx <- x$get()
  
  # calculate its inverse
  i <- solve(mtx, ...)
  
  # cache the calculated value
  x$setinverse(i)
  
  #return the result
  i
}

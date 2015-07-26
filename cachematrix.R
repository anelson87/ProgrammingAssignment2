## makeCacheMatrix takes a matrix as input and stores its inverse. cacheSolve returns the
## inverse of the matrix entered in makeCacheMatrix. If the inverse is already available through
## a past entry, cacheSovle returns that value from the cache, while letting the user know
## it's grabbing data from the cache.

## makeCacheMatrix takes a matrix as input and stores the process to get the inverse in a 
## list of objects to be used later.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  
  ## list of objects to get the inverse of matrix x
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix entered in makeCacheMatrix. If the inverse is
## already available through a past entry, cacheSovle returns that value from the cache, while
## letting the user know it's grabbing data from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    
    ## message displayed if the inverse of x is already available in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
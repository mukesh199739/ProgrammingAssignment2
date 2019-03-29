## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## creates a wrapper object around a matrix that can optionally store
## its inverse. exposes setters and getters for both. invalidates
## cache on set.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function
## computes the inverse of a matrix with caching.
## expects a wrapped matrix as returned from makeCacheMatrix. first
## checks cache in the object passed in for the presence of the result
## of a previous run. if found returns that, otherwise computes the
## result and then stores it in the cache as well as returning it to
## the caller

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
}

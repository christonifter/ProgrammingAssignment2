## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list of four functions, and an environment to cache the inverse matrix in "outmat"
#

makeCacheMatrix <- function(x = matrix()) {
    outmat <- NULL
  set <- function(y) {
    x <<- y
    outmat <<- NULL
  }
  get <- function() x
  setinv <- function(y) outmat <<- y
  getinv <- function() outmat
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# CacheSolve returns the inverse of matrix "x", either by retrieving the cached matrix, or with function solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  outmat<-x$getinv()
  if(!is.null(outmat)) {
    message("getting cached data")
    return(outmat)
  }
  data <- x$get()
  outmat <- solve(data, ...)
  x$setinv(outmat)
  outmat
}

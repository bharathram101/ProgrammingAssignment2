## Put comments here that give an overall description of what your
## functions do
## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## These functions are written in order to cache the inverse of the matrix. 

## The below function is used to create a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The below function is used to find the inverse of the matrix returned by the makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  invr
}

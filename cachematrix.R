## Put comments here that give an overall description of what your
## functions do

## This function creates and returns a new 'cached matrix' object with 4 method: 
## get - returns data of intenral matrix
## set - resets data for internal matrix
## getinverse - returns inversed pre-cached matrix
## setinverse - sets inversed pre-cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(y) inverse <<- y
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This is a cached version of solve(x) function. It uses a special 'cached matrix' object created by
## the makeCacheMatrix() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}

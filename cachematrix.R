## A client function for calculating the inverse of a matrix when it has not already been calculated.
## This is done through the use of closure functions.

## This is the parent function that provides an environment for the symbol value pair
## associated with inv to live. 
## It also holds the getter and setter methods for getting and setting a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## Use superassignment here to set the value of x in the environment where makeCacheMatrix
    ## is defined or the next parent environment where x is defined.
    x <<- y  
    ## Use superassignment here to set the value of inv in the parent environment to set()
    ## i.e. within the function makeCacheMatrix()
    inv <<- NULL
  }
  get <- function() x
  ## Use superassignment here to set the value of inv within the function makeCacheMatrix()
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  ## display the functions for use
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## A client function for calling the setter method for inv.
## It avoids calculating the inverse if it has already been calculated for the vector x.
cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data)
  a$setinverse(inv)
  inv
}

## How to call:
## > x <- matrix(1:4,nrow = 2, ncol = 2)
## > xx <- makeCacheMatrix(x)
## > cacheSolve(xx)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(xx)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

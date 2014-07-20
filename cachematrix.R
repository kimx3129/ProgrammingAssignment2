## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix creates a special 'matrix' object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  ## This set function takes one parameter which sets the matrix value x
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  ## This get function returns a matrix x
  get <- function() x
  
  ## This function gets an inverse matrix of x
  setInv <- function(solve) invMatrix <<- solve
  
  ## This function returns a calculated inverse matrix 'invMatrix'
  getInv <- function() invMatrix
  
  ## Return functions available within a makeCacheMatrix function
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. If the inverse has already
## been calculated, then this function should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
  
  ## Get the value from the cache. If this is the first computation, m is NULL 
  ## and calculate inverse matrix
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInv(m)
  
  ## Return the inverse matrix
  m
}

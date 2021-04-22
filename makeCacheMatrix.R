## CACHING THE INVERSE OF A MATRIX
## In this programming assignment, caching the inverse of a matrix
## using the makeCacheMatrix and cacheSolve functions are practiced.

## The first function, makeCacheMatrix, is a function that assigns a special "vector" or "matrix"
## which could set and get the value of the vector as a variable and also its inverse.

## With regards to the function used in caching the mean of a vector, all "mean" functions
## from the sample code were changed to "inv" functions to indicate the inverse of the given vector.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_a) inv <<- inv_a
  getinc <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}

## Solving for the inverse of the special "vector" or "matrix" using the function below.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting the Inveresd Matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$etinv(inv)
  inv
}

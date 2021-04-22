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

## Testing the code if it works and doesn't commit any error.

> matrix1 <- makeCacheMatrix(matrix(rnorm(25), 5, 5))
> cacheSolve(matrix1)
           [,1]       [,2]        [,3]       [,4]
[1,] -0.3882684 -0.1483645 -0.08430182  0.0945661
[2,] -0.1591748  0.2492793  0.17920892 -0.2686415
[3,]  0.3216937  0.1834941  0.03547586  0.2962013
[4,] -0.4511279  0.7306911 -0.50819070 -0.2203324
[5,] -0.3299662  0.2176942 -0.11729142  0.3144062
             [,5]
[1,]  0.362289756
[2,] -0.084644491
[3,]  0.290488615
[4,] -0.002542555
[5,] -0.250489442

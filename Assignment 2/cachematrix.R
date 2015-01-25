## In this assignment we created two functions that are used to create a 
## special object that stores a numeric matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to: set the value of 
## the matrix, get the value of the matrix, set the value of its inverse
## and get the value of its inverse.

## The function ginv() is used to get Moore-Penrose Generalisied Inverse
## of a matrix (not only square matrix).First we install it from CRAN using 
## install.packages("MASS"), then library(MASS) to load it in R session.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(ginv) m <<- ginv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. It first checks to see if the inverse 
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setinv(m)
  m
}

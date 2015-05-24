## 5/22/15
## R Programming Coursera Assignment 2
## Write two functions, given a stub
## The results of the first function, makeCacheMatrix, will be used as the input for the second function, cacheSolve,
## as a means cache the inverse of a matrix.

## makeCacheMatrix will return a list of getter and setter functions that can
## be used to set and/or retrieve a matrix through set() and get(), as well as 
## set and/or retrieve the inverse of that matrix through setsolve() and getsolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solver) m <<- solver
  getsolve <-function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function will take input, which for our testing will be the list 
## returned by makeCacheMatrix, and it will return the inverse matrix if it is not null.  
## If the matrix passed in is null then it will get the cached matrix, determine the inverse of it, and then return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

## This assignment is to create R functions that is able to cache potentially time-consuming computations. 

## This function creates matrix object which is a list containing a function to
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse matrix using solve function
## 4. get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## Set the value of matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## Get the value of matrix
  get <- function() x
  ## Set the value of inverse matrix using solve function
  setsolve <- function(solve) s <<- solve
  ## Get the value of inverse matrix
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s  
}

## makeCacheMatrix, permits a matrix to be stored in memory
## That matrix will be used as a parameter to calculate its inverse
## using the solve command.

## cacheSolve is a function that works with makeCacheMatrix
## It is called to calculate the inverse of a matrix
## If the inverse was calculated before it gets the information from memory
## oterhwise it calculates it on the fly
## It is assumed for this assignment that the matrix is always invertible

## makeCacheMatrix - saves the matrix in memory

makeCacheMatrix <- function(x = matrix()) {
  ## Generate the matrix that will be cached 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve - returns the inverse of the Matrix
## It tries to get the inverse from memory first
## If it doesn't get it, it calculates it and saves it in memory for future queries

cacheSolve <- function(x, ...) {
  ## Solves for the inverse of the matrix
  m <- x$getinverse()

  ## Looks if the matrix is in memory, to use the Cached information
  if(!is.null(m)) {
    message("getting cached data - solve")
    return(m)
  }
  
  ## If matrix was not in memory, it runs the calculation normally (solve fn)
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

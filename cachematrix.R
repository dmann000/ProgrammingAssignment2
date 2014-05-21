## Two functions below provide a mechanism for providing a matrix, and computing the 
## inverse of that matrix (assuming it is invertable).  The cacheSolve function will 
## provide the inverse from cache if the matrix has not changed since the last request.
## This caching is done to reduce load where possible

## makeCacheMatrix is a list of functions.  This function allows you to set the 
## matrix values, show the matrix, or the inverse if it it has been calculated

## Example usage:
## myMatrix <- makeCacheMatrix() ## open function as variable
## myMatrix <- makeCacheMatrix(1:4,2,2) ## define matrix values upon calling the function
## myMatrix$set(1:4,2,2) # sets the initial matrix values
## myMatrix$get() # retrieves the current matrix values
## myMatrix$getinverse & myMatrix$setinverse are used by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y, ...) {
    x <<- matrix(y, ...)
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is the function used to obtain the inverse of the matrix
## cacheSolve will pull data from cache if the same inverse is called twice

## Example usage:
## cacheSolve(myMatrix)
## this function will return the inverse of myMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}


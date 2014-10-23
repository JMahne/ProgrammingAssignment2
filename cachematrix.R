## A function that gets the inverse of a matrix and then another function that is used to cache
## this inverse. 

## Used to create a special matrix

makeCacheMatrix <- function(x = matrix()) {

  # Initialize the inv property
  i <- NULL
  
  # Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  # Get the matrix
  get <- function() {
    # Return the matrix
    m
  }
  # Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  # Get the inverse of the matrix
  getInverse <- function() {
    # Return the inverse property
    i
  }
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Determine the inverse of the matrix created by "makecachematrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Just return the inverse if its already set
  if( !is.null(m) ) {
    message("get cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}


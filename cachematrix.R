# A pair of functions which cache inverse of a matrix


# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  # Initializing inverse property
  i <- NULL
  
  # set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # get matrix
  get <- function() {
    # Return matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  # get the inverse of the matrix
  getInverse <- function() {
    #Return inverse property
    i
  }
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been
#calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Return inverse matrix of 'x'
  m <- x$getInverse()
  
  # Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  #Calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  #Setting inverse to the object
  x$setInverse(m)
  
  # Return the matrix
  m
}

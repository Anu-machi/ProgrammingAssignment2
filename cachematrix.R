# Matrix inversion 
# Following two functions(makeCacheMatrix and cacheSolve) are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list and functions to
# - set the matrix
# - get the matrix
# - set the inverse of the matrix
# - get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) 
  {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Below function first checks if the inverse has already been calculated. 
# If so, it gives the result and skips the computation.
# If not, the inverse is calculated using setinverse.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) 
  {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data.")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
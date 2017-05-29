## Creates a matrix object represented as a list of functions that can keep
## track of the matrix itself and its inverse. Inverse of the matrix is cached
## to avoid recalculating the inverse for the same matrix. makeCacheMatrix
## creates the CacheMatrix and cacheSolve retrieves the inverse of the matrix

## makeCacheMatrix creates a "CacheMatrix" that can keep track of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  # initialize the inverse value of matrix
  set <- function(y) {
    x <<- y
    i <- NULL  # when we reset the matrix, must also reset the inverse
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse  # used by cacheSolve function
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve checks if the inverse of a CacheMatrix is already calculated
## If not, the function calculates and caches it in the CacheMatrix object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {  # check if the inverse of the matrix cached
    message("getting cached data")
    return(i)
  }
  # if inverse not in cache, calculate and cache it
  m <- x$get()
  i <- solve(m)
  x$setinverse(i)
  i
}

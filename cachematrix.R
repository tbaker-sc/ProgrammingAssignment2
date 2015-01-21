## The two functions below create a special object that holds a matrix and caches its inverse
## and a function that utilizes the special object to find/return a inverse matrix, pulling it
## from cache when possible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #Rewrite this piece
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
  setinverse = setinverse,
  getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  # If the inverse exists, return that value
  if(!is.null(m)  {
    message("getting cached data")
    return(m)
  }
  ## If inverse was null in test above, get matrix from special object, calculate inverse, then set 
  ## inverse via special object
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

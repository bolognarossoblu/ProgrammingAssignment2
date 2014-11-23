## The following are a pair of functions to cache the inverse of a
## matrix.

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

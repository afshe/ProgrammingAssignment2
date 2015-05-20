## The functions in this R program return inverse of a matrix,
## If the inverse is already calculated then cached result
## is returned instead of recalculating it


## makeCacheMatrix creates a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##Function to get the matrix
  get <- function() x
  
  ## Set the inverse matrix
  setcachedinverse <- function(solve) m <<- solve
  
  ##Get the inverse matrix
  getcachedinverse <- function() m
  list(set = set, get = get,
       setcachedinverse = setcachedinverse,
       getcachedinverse = getcachedinverse)
}


## cacheSolve - calculates the inverse of metrix if the 
## inverse is not cached already. If the inverse is present
## retruns the inverse from cache instead ofrecommputing it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getcachedinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcachedinverse(m)
  m       ## Return a matrix that is the inverse of 'x'
}

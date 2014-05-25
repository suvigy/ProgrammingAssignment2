## We create functions that inverse a matrix in a cached way. 
## Once we get the matrix we'll store the result, next time
## we get it we'll return the stored value

## makeCacheMatrix - creates a matrix wrapper "class"
## with some caching and setter-getter functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## CacheSolve makes the actual cached computation using the previous function

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

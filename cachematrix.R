## MakeCacheMatrix(x) and cacheSolve(x, ...) can be used to perform cached matrix operations
## Can be used with square matrices only.
## EXAMPLE:
## someMatrix <- matrix(sample(36), nrow=6, ncol=6)
## newMatrix <- makeCacheMatrix()
## newMatrix$set(someMatrix)
## newMatrix$get()



## makeCacheMatrix(x) creates a matrix object x and caches it

makeCacheMatrix <- function(x = matrix()) {
  
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Try if inversed matrix can be loaded from cache
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached matrix data")
    return(i)
  }
  
  ## If failed to load from cache, generate new inversed matrix
  ## and return it
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
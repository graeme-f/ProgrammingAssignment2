## Put comments here that give an overall description of what your
## functions do

## Function to create a cacheable
##
## Arguments
## x a matrix
##
## Returns a matrix that can cache the inverse
##

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## originally the inverse matrix will be null
  set <- function (y){
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
  i<- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i ## Return a matrix that is the inverse of 'x'
}
